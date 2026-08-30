{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.ACP.State
Description : Runtime state for ACP sessions and notifications
Stability   : experimental
-}

module Bot.ACP.State
  ( AcpState
  , AcpClientId
  , AcpClientCapabilities (..)
  , AcpClientQueue
  , AcpClientEvent (..)
  , AcpSessionId
  , newAcpState
  , registerClient
  , unregisterClient
  , setClientCapabilities
  , readClient
  , writeClient
  , resolveClientResponse
  , requestSessionClient
  , withActiveSessionClient
  , broadcast
  , openSession
  , deleteSession
  , enqueueUserMessage
  , enqueuePromptMessage
  , incomingMessages
  , sessionIdFromMessage
  , acpSessionIdText
  , PromptCompletion (..)
  , completeSessionPrompt
  , cancelSessionPrompts
  , notifyPromptComplete
  , withPromptWaiter
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import qualified Bot.Session as Session
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.STM as STM
import qualified Effectful.FileSystem as FileSystem
import qualified JSONRPC
import qualified Streaming as S
import qualified Streaming.Prelude as S

type AcpClientId = Integer

data AcpClientCapabilities = AcpClientCapabilities
  { readTextFile :: !Bool
  , writeTextFile :: !Bool
  , terminal :: !Bool
  }
  deriving (Eq, Show)

data AcpClientQueue = AcpClientQueue
  { clientId :: !AcpClientId
  , events :: !(STM.TBQueue AcpClientEvent)
  }

data AcpClientEvent
  = AcpClientSend !Aeson.Value
  | AcpClientDisconnect !Text
  deriving (Eq, Show)

type AcpSessionId = Session.SessionId

data PromptCompletion
  = PromptCompleted !MessageId
  | PromptCancelled
  deriving (Eq, Show)

data AcpState = AcpState
  { clients :: !(STM.TVar (Map AcpClientId AcpClientQueue))
  , clientCapabilities :: !(STM.TVar (Map AcpClientId AcpClientCapabilities))
  , activeSessionClients :: !(STM.TVar (Map AcpSessionId AcpClientId))
  , pendingClientRequests :: !(STM.TVar (Map (AcpClientId, AcpRequestKey) (STM.TMVar (Either Text Aeson.Value))))
  , nextClientId :: !(STM.TVar AcpClientId)
  , nextClientRequestId :: !(STM.TVar Integer)
  , inbound :: !(STM.TChan IncomingMessage)
  , promptWaiters :: !(STM.TVar (Map AcpSessionId [STM.TMVar PromptCompletion]))
  , activePromptMessages :: !(STM.TVar (Map AcpSessionId [MessageId]))
  }

newAcpState :: Concurrent :> es => Eff es AcpState
newAcpState = STM.atomically do
  clients <- STM.newTVar Map.empty
  clientCapabilities <- STM.newTVar Map.empty
  activeSessionClients <- STM.newTVar Map.empty
  pendingClientRequests <- STM.newTVar Map.empty
  nextClientId <- STM.newTVar 1
  nextClientRequestId <- STM.newTVar 1
  inbound <- STM.newTChan
  promptWaiters <- STM.newTVar Map.empty
  activePromptMessages <- STM.newTVar Map.empty
  pure AcpState{clients, clientCapabilities, activeSessionClients, pendingClientRequests, nextClientId, nextClientRequestId, inbound, promptWaiters, activePromptMessages}

registerClient :: Concurrent :> es => AcpState -> Eff es (AcpClientId, AcpClientQueue)
registerClient acpState =
  STM.atomically do
    clientId <- STM.readTVar acpState.nextClientId
    STM.writeTVar acpState.nextClientId (clientId + 1)
    queue <- AcpClientQueue clientId <$> STM.newTBQueue acpClientQueueCapacity
    STM.modifyTVar' acpState.clients (Map.insert clientId queue)
    pure (clientId, queue)

unregisterClient :: Concurrent :> es => AcpState -> AcpClientId -> Eff es ()
unregisterClient acpState clientId =
  STM.atomically do
    STM.modifyTVar' acpState.clients (Map.delete clientId)
    STM.modifyTVar' acpState.clientCapabilities (Map.delete clientId)
    STM.modifyTVar' acpState.activeSessionClients (Map.filter (/= clientId))
    pending <- STM.readTVar acpState.pendingClientRequests
    let (removed, remaining) = Map.partitionWithKey (\(pendingClientId, _) _ -> pendingClientId == clientId) pending
    STM.writeTVar acpState.pendingClientRequests remaining
    traverse_ (\waiter -> STM.tryPutTMVar waiter (Left "ACP client disconnected")) removed

setClientCapabilities :: Concurrent :> es => AcpState -> AcpClientQueue -> AcpClientCapabilities -> Eff es ()
setClientCapabilities acpState queue capabilities =
  STM.atomically $
    STM.modifyTVar' acpState.clientCapabilities (Map.insert queue.clientId capabilities)

readClient :: Concurrent :> es => AcpClientQueue -> Eff es AcpClientEvent
readClient AcpClientQueue{events} =
  STM.atomically (STM.readTBQueue events)

writeClient :: Concurrent :> es => AcpClientQueue -> Aeson.Value -> Eff es ()
writeClient AcpClientQueue{events} value =
  STM.atomically (STM.writeTBQueue events (AcpClientSend value))

resolveClientResponse :: Concurrent :> es => AcpState -> AcpClientQueue -> JSONRPC.JSONRPCMessage -> Eff es Bool
resolveClientResponse acpState queue message =
  STM.atomically do
    let key = (queue.clientId, requestKey (responseId message))
    pending <- STM.readTVar acpState.pendingClientRequests
    case Map.lookup key pending of
      Nothing ->
        pure False
      Just waiter -> do
        STM.modifyTVar' acpState.pendingClientRequests (Map.delete key)
        _ <- STM.tryPutTMVar waiter (responseResult message)
        pure True

requestSessionClient
  :: (Concurrent :> es, IOE :> es)
  => AcpState
  -> AcpSessionId
  -> Text
  -> (AcpClientCapabilities -> Bool)
  -> Aeson.Value
  -> Eff es (Either Text Aeson.Value)
requestSessionClient acpState sessionId method supported params =
  STM.atomically acquire >>= \case
    Left err ->
      pure (Left err)
    Right (queue, requestId, waiter) -> do
      writeClient queue (Aeson.toJSON (clientRequest requestId))
      STM.atomically (STM.readTMVar waiter)
        `finally` cleanupPending queue requestId
  where
    acquire = do
      activeClients <- STM.readTVar acpState.activeSessionClients
      clients <- STM.readTVar acpState.clients
      capabilitiesByClient <- STM.readTVar acpState.clientCapabilities
      case Map.lookup sessionId activeClients >>= \clientId -> (clientId,) <$> Map.lookup clientId clients of
        Nothing ->
          pure (Left "No active ACP client for this session.")
        Just (clientId, queue) -> do
          let capabilities = Map.findWithDefault legacyClientCapabilities clientId capabilitiesByClient
          if not (supported capabilities)
            then
              pure (Left [i|ACP client does not support #{method}.|])
            else do
              requestNumber <- STM.readTVar acpState.nextClientRequestId
              STM.writeTVar acpState.nextClientRequestId (requestNumber + 1)
              let requestId = JSONRPC.RequestId (Aeson.Number (fromInteger requestNumber))
              waiter <- STM.newEmptyTMVar
              STM.modifyTVar' acpState.pendingClientRequests (Map.insert (clientId, requestKey requestId) waiter)
              pure (Right (queue, requestId, waiter))
    clientRequest requestId =
      JSONRPC.RequestMessage $
        JSONRPC.JSONRPCRequest JSONRPC.rPC_VERSION requestId method params
    cleanupPending queue requestId =
      STM.atomically $
        STM.modifyTVar' acpState.pendingClientRequests (Map.delete (queue.clientId, requestKey requestId))

legacyClientCapabilities :: AcpClientCapabilities
legacyClientCapabilities =
  AcpClientCapabilities
    { readTextFile = True
    , writeTextFile = True
    , terminal = False
    }

withActiveSessionClient
  :: (Concurrent :> es, IOE :> es)
  => AcpState
  -> AcpClientQueue
  -> AcpSessionId
  -> Eff es a
  -> Eff es a
withActiveSessionClient acpState queue sessionId =
  bracket acquire release . const
  where
    acquire =
      STM.atomically do
        previous <- Map.lookup sessionId <$> STM.readTVar acpState.activeSessionClients
        STM.modifyTVar' acpState.activeSessionClients (Map.insert sessionId queue.clientId)
        pure previous
    release previous =
      STM.atomically $
        STM.modifyTVar' acpState.activeSessionClients \active ->
          case previous of
            Nothing ->
              Map.delete sessionId active
            Just previousClientId ->
              Map.insert sessionId previousClientId active

newtype AcpRequestKey = AcpRequestKey Text
  deriving newtype (Eq, Ord)

requestKey :: JSONRPC.RequestId -> AcpRequestKey
requestKey requestId =
  AcpRequestKey . TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode $ requestId

responseId :: JSONRPC.JSONRPCMessage -> JSONRPC.RequestId
responseId = \case
  JSONRPC.ResponseMessage response ->
    response.id
  JSONRPC.ErrorMessage response ->
    response.id
  JSONRPC.RequestMessage request ->
    request.id
  JSONRPC.NotificationMessage{} ->
    JSONRPC.RequestId Aeson.Null

responseResult :: JSONRPC.JSONRPCMessage -> Either Text Aeson.Value
responseResult = \case
  JSONRPC.ResponseMessage response ->
    Right response.result
  JSONRPC.ErrorMessage response ->
    Left response.error.message
  JSONRPC.RequestMessage{} ->
    Left "Expected ACP client response, got request."
  JSONRPC.NotificationMessage{} ->
    Left "Expected ACP client response, got notification."

broadcast :: Concurrent :> es => AcpState -> Aeson.Value -> Eff es ()
broadcast acpState value =
  STM.atomically do
    clients <- STM.readTVar acpState.clients
    clients' <- Map.traverseMaybeWithKey (broadcastClient value) clients
    STM.writeTVar acpState.clients clients'

openSession :: Storage.Storage :> es => Maybe Text -> Eff es Session.Session
openSession =
  Session.openSession

deleteSession :: (Storage.Storage :> es, FileSystem.FileSystem :> es) => AcpSessionId -> Eff es Bool
deleteSession =
  Session.deleteSession

enqueueUserMessage
  :: (Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => AcpState
  -> Session.SessionSend
  -> Eff es (Either Text (Maybe IncomingMessage))
enqueueUserMessage acpState sessionSend =
  Session.appendUserMessage sessionSend >>= \case
    Left err ->
      pure (Left err)
    Right Nothing ->
      pure (Right Nothing)
    Right (Just sessionMessage) -> do
      message <- acpIncomingMessage sessionSend sessionMessage
      STM.atomically (STM.writeTChan acpState.inbound message)
      pure (Right (Just message))

enqueuePromptMessage
  :: (Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => AcpState
  -> Session.SessionSend
  -> Eff es (Either Text (Maybe IncomingMessage))
enqueuePromptMessage acpState sessionSend =
  Session.appendUserMessage sessionSend >>= \case
    Left err ->
      pure (Left err)
    Right Nothing ->
      pure (Right Nothing)
    Right (Just sessionMessage) -> do
      message <- acpIncomingMessage sessionSend sessionMessage
      STM.atomically do
        for_ message.messageId \messageId ->
          registerPromptMessageSTM acpState sessionSend.sessionId messageId
        STM.writeTChan acpState.inbound message
      pure (Right (Just message))

incomingMessages :: Concurrent :> es => AcpState -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessages acpState =
  forever do
    message <- S.lift (STM.atomically (STM.readTChan acpState.inbound))
    S.yield message

sessionIdFromMessage :: IncomingMessage -> AcpSessionId
sessionIdFromMessage message =
  Session.SessionId (fromMaybe "session" (listToMaybe message.chatAliases))

acpSessionIdText :: AcpSessionId -> Text
acpSessionIdText =
  Session.sessionIdText

notifyPromptComplete :: Concurrent :> es => AcpState -> AcpSessionId -> MessageId -> Eff es ()
notifyPromptComplete acpState sessionId messageId =
  STM.atomically do
    waiters <- STM.readTVar acpState.promptWaiters
    case Map.lookup sessionId waiters of
      Nothing ->
        pure ()
      Just sessionWaiters ->
        traverse_ (`STM.tryPutTMVar` PromptCompleted messageId) sessionWaiters

completeSessionPrompt :: Concurrent :> es => AcpState -> AcpSessionId -> Eff es ()
completeSessionPrompt acpState sessionId =
  STM.atomically $
    STM.modifyTVar' acpState.activePromptMessages (Map.delete sessionId)

cancelSessionPrompts :: Concurrent :> es => AcpState -> AcpSessionId -> Eff es [MessageId]
cancelSessionPrompts acpState sessionId = do
  STM.atomically do
    messageIds <- Map.findWithDefault [] sessionId <$> STM.readTVar acpState.activePromptMessages
    waiters <- Map.findWithDefault [] sessionId <$> STM.readTVar acpState.promptWaiters
    STM.modifyTVar' acpState.activePromptMessages (Map.delete sessionId)
    traverse_ (`STM.tryPutTMVar` PromptCancelled) waiters
    pure messageIds

withPromptWaiter
  :: (Concurrent :> es, IOE :> es)
  => AcpState
  -> AcpSessionId
  -> Eff es a
  -> Eff es PromptCompletion
withPromptWaiter acpState sessionId action =
  bracket acquire release use
  where
    acquire =
      STM.atomically do
        waiter <- STM.newEmptyTMVar
        STM.modifyTVar' acpState.promptWaiters (Map.insertWith (<>) sessionId [waiter])
        pure waiter
    release waiter =
      STM.atomically $
        STM.modifyTVar' acpState.promptWaiters (Map.update (removeWaiter waiter) sessionId)
    use waiter = do
      _ <- action
      STM.atomically (STM.readTMVar waiter)

removeWaiter :: Eq a => a -> [a] -> Maybe [a]
removeWaiter waiter waiters =
  case filter (/= waiter) waiters of
    [] ->
      Nothing
    remaining ->
      Just remaining

registerPromptMessageSTM :: AcpState -> AcpSessionId -> MessageId -> STM.STM ()
registerPromptMessageSTM acpState sessionId messageId =
  STM.modifyTVar' acpState.activePromptMessages (Map.insertWith (<>) sessionId [messageId])

broadcastClient :: Aeson.Value -> AcpClientId -> AcpClientQueue -> STM.STM (Maybe AcpClientQueue)
broadcastClient value _clientId client@AcpClientQueue{events} = do
  full <- STM.isFullTBQueue events
  if full
    then do
      drainTBQueue events
      STM.writeTBQueue events (AcpClientDisconnect "ACP notification queue overflow")
      pure Nothing
    else do
      STM.writeTBQueue events (AcpClientSend value)
      pure (Just client)

drainTBQueue :: STM.TBQueue a -> STM.STM ()
drainTBQueue queue =
  STM.tryReadTBQueue queue >>= \case
    Nothing ->
      pure ()
    Just _ ->
      drainTBQueue queue

acpClientQueueCapacity :: Natural
acpClientQueueCapacity =
  256

acpIncomingMessage
  :: (Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => Session.SessionSend
  -> Session.SessionMessage
  -> Eff es IncomingMessage
acpIncomingMessage sessionSend messageRow = do
  llmImageUrls <- Session.sessionSendLlmImageUrls sessionSend
  pure IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = PlatformACP
    , kind = ChatPrivate
    , chatId = Nothing
    , chatAliases = [Session.sessionIdText sessionSend.sessionId]
    , digest = MessageDigest
        { chatIsAllowed = True
        , senderIsAllowed = True
        , senderIsSuperuser = True
        , mentionsBot = True
        , botId = Just "acp"
        }
    , senderId = Just "acp-user"
    , senderUsername = Just "ACP"
    , messageId = Just messageRow.messageId
    , replyToMessageId = sessionSend.replyToMessageId
    , mentions = []
    , mentionUsernames = []
    , imageUrls = llmImageUrls
    , files = []
    , text = Session.sessionSendContextText sessionSend
    , raw = Aeson.toJSON sessionSend
    }
