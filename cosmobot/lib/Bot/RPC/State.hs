{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.RPC.State
Description : Shared runtime state for RPC sessions and notifications
Stability   : experimental
-}

module Bot.RPC.State
  ( RpcState
  , RpcClientId
  , RpcClientQueue
  , RpcClientEvent (..)
  , RpcSessionId
  , RpcOutbound (..)
  , RpcChatMessage
  , RpcChatSession
  , RpcChatAttachmentRef (..)
  , RpcChatSend (..)
  , unRpcSessionId
  , newRpcState
  , registerClient
  , unregisterClient
  , readClient
  , writeClient
  , broadcast
  , broadcastAuditRecord
  , openChatSession
  , listChatSessions
  , getChatSession
  , chatHistory
  , forkChatSession
  , renameChatSession
  , deleteChatSession
  , enqueueChatMessage
  , incomingMessages
  , sessionIdFromMessage
  , rememberMessageNumber
  , storedMessageToRpc
  , storedMediaRef
  , parseMediaId
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Media as Media
import Bot.Prelude
import qualified Bot.JSONRPC as RPC
import qualified Bot.Session as Session
import qualified Bot.Effect.Storage as StorageEffect
import qualified Bot.Storage.Session as Storage
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Effectful.Concurrent.STM as STM
import qualified Effectful.FileSystem as FileSystem
import qualified Streaming as S
import qualified Streaming.Prelude as S

type RpcClientId = Integer

newtype RpcClientQueue = RpcClientQueue (STM.TBQueue RpcClientEvent)

data RpcClientEvent
  = RpcClientSend !Aeson.Value
  | RpcClientDisconnect !Text
  deriving (Eq, Show)

type RpcSessionId = Session.SessionId

unRpcSessionId :: RpcSessionId -> Text
unRpcSessionId =
  Session.sessionIdText

data RpcState = RpcState
  { clients :: !(STM.TVar (Map RpcClientId RpcClientQueue))
  , nextClientId :: !(STM.TVar RpcClientId)
  , nextSessionNumber :: !(STM.TVar Integer)
  , nextMessageNumber :: !(STM.TVar Integer)
  , inbound :: !(STM.TChan IncomingMessage)
  }

data RpcOutbound = RpcOutbound
  { sessionId :: !RpcSessionId
  , messageId :: !(Maybe MessageId)
  , text :: !Text
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

type RpcChatMessage = Session.SessionMessage

type RpcChatSession = Session.Session

data RpcChatAttachmentRef = RpcChatAttachmentRef
  { attachmentId :: !Text
  , name :: !Text
  , mediaType :: !Text
  , kind :: !Text
  , size :: !Int
  , url :: !Text
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

instance Aeson.FromJSON RpcChatAttachmentRef where
  parseJSON =
    Aeson.withObject "RPC chat attachment" \o -> do
      attachmentId <- o Aeson..: "attachmentId" <|> o Aeson..: "attachment_id" <|> o Aeson..: "id"
      name <- fromMaybe attachmentId <$> o Aeson..:? "name"
      mediaType <- fromMaybe "application/octet-stream" <$> (o Aeson..:? "mediaType" >>= \case
        Just value -> pure (Just value)
        Nothing -> o Aeson..:? "media_type")
      kind <- fromMaybe (kindFromMediaType mediaType) <$> o Aeson..:? "kind"
      size <- fromMaybe 0 <$> o Aeson..:? "size"
      url <- fromMaybe (defaultMediaUrl attachmentId) <$> o Aeson..:? "url"
      pure RpcChatAttachmentRef{attachmentId, name, mediaType, kind, size, url}

data RpcChatSend = RpcChatSend
  { sessionId :: !RpcSessionId
  , text :: !Text
  , imageUrls :: ![Text]
  , attachments :: ![RpcChatAttachmentRef]
  , replyToMessageId :: !(Maybe MessageId)
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

newRpcState :: Concurrent :> es => Eff es RpcState
newRpcState = STM.atomically do
  clients <- STM.newTVar Map.empty
  nextClientId <- STM.newTVar 1
  nextSessionNumber <- STM.newTVar 1
  nextMessageNumber <- STM.newTVar 1
  inbound <- STM.newTChan
  pure RpcState{clients, nextClientId, nextSessionNumber, nextMessageNumber, inbound}

registerClient :: Concurrent :> es => RpcState -> Eff es (RpcClientId, RpcClientQueue)
registerClient rpcState =
  STM.atomically do
    clientId <- STM.readTVar rpcState.nextClientId
    STM.writeTVar rpcState.nextClientId (clientId + 1)
    queue <- RpcClientQueue <$> STM.newTBQueue rpcClientQueueCapacity
    STM.modifyTVar' rpcState.clients (Map.insert clientId queue)
    pure (clientId, queue)

unregisterClient :: Concurrent :> es => RpcState -> RpcClientId -> Eff es ()
unregisterClient rpcState clientId =
  STM.atomically $
    STM.modifyTVar' rpcState.clients (Map.delete clientId)

readClient :: Concurrent :> es => RpcClientQueue -> Eff es RpcClientEvent
readClient (RpcClientQueue queue) =
  STM.atomically (STM.readTBQueue queue)

writeClient :: Concurrent :> es => RpcClientQueue -> Aeson.Value -> Eff es ()
writeClient (RpcClientQueue queue) value =
  STM.atomically (STM.writeTBQueue queue (RpcClientSend value))

broadcast :: Concurrent :> es => RpcState -> Aeson.Value -> Eff es ()
broadcast rpcState value = do
  STM.atomically do
    clients <- STM.readTVar rpcState.clients
    clients' <- Map.traverseMaybeWithKey (broadcastClient value) clients
    STM.writeTVar rpcState.clients clients'

broadcastAuditRecord :: Concurrent :> es => RpcState -> Aeson.Value -> Eff es ()
broadcastAuditRecord rpcState recordValue =
  broadcast rpcState (Aeson.toJSON (RPC.notification "audit.event" recordValue))

openChatSession :: (Concurrent :> es, StorageEffect.Storage :> es) => RpcState -> Maybe Text -> Eff es RpcChatSession
openChatSession rpcState label = do
  session <- Session.openSession label
  rememberSessionNumber rpcState (Session.sessionIdText session.sessionId)
  pure session

listChatSessions :: StorageEffect.Storage :> es => Eff es [RpcChatSession]
listChatSessions =
  Session.listSessions

getChatSession :: StorageEffect.Storage :> es => RpcSessionId -> Eff es (Maybe RpcChatSession)
getChatSession sessionId =
  Session.getSession sessionId

chatHistory :: StorageEffect.Storage :> es => RpcSessionId -> Eff es [RpcChatMessage]
chatHistory sessionId =
  Session.sessionHistory sessionId

forkChatSession :: (Concurrent :> es, StorageEffect.Storage :> es) => RpcState -> RpcSessionId -> MessageId -> Maybe Text -> Eff es (Maybe RpcChatSession)
forkChatSession rpcState sourceSessionId messageId label = do
  session <- Session.forkSession sourceSessionId messageId label
  traverse_ (rememberSessionNumber rpcState . Session.sessionIdText . (.sessionId)) session
  pure session

renameChatSession :: StorageEffect.Storage :> es => RpcSessionId -> Text -> Eff es (Maybe RpcChatSession)
renameChatSession sessionId label =
  Session.renameSession sessionId label

deleteChatSession :: (StorageEffect.Storage :> es, FileSystem.FileSystem :> es) => RpcSessionId -> Eff es Bool
deleteChatSession sessionId =
  Session.deleteSession sessionId

enqueueChatMessage
  :: (Concurrent :> es, StorageEffect.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => RpcState
  -> RpcChatSend
  -> Eff es (Either Text (Maybe IncomingMessage))
enqueueChatMessage rpcState chatSend = do
  appended <- Session.appendUserMessage (rpcChatSendToSession chatSend)
  case appended of
    Left err ->
      pure (Left err)
    Right Nothing ->
      pure (Right Nothing)
    Right (Just sessionMessage) -> do
      rememberMessageNumber rpcState sessionMessage.messageId
      message <- rpcIncomingMessage chatSend sessionMessage
      STM.atomically (STM.writeTChan rpcState.inbound message)
      broadcast rpcState (Aeson.toJSON (RPC.notification "chat.message" sessionMessage))
      pure (Right (Just message))

incomingMessages :: Concurrent :> es => RpcState -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessages rpcState = forever do
  message <- S.lift (STM.atomically (STM.readTChan rpcState.inbound))
  S.yield message

rpcIncomingMessage :: (StorageEffect.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es) => RpcChatSend -> RpcChatMessage -> Eff es IncomingMessage
rpcIncomingMessage chatSend messageRow = do
  let sessionText = Session.sessionIdText chatSend.sessionId
      canonicalSend :: Session.SessionSend
      canonicalSend =
        Session.SessionSend
          { sessionId = chatSend.sessionId
          , text = chatSend.text
          , imageUrls = messageRow.imageUrls
          , attachments = messageRow.attachments
          , replyToMessageId = chatSend.replyToMessageId
          }
  llmImageUrls <- Session.sessionSendLlmImageUrls canonicalSend
  pure IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = PlatformRPC
    , kind = ChatPrivate
    , chatId = Nothing
    , chatAliases = [sessionText]
    , digest = MessageDigest
        { chatIsAllowed = True
        , senderIsAllowed = True
        , senderIsSuperuser = True
        , mentionsBot = True
        , botId = Just "rpc"
        }
    , senderId = Just "rpc-user"
    , senderUsername = Just "RPC"
    , messageId = Just messageRow.messageId
    , replyToMessageId = chatSend.replyToMessageId
    , mentions = []
    , mentionUsernames = []
    , imageUrls = llmImageUrls
    , files = []
    , text = Session.sessionSendContextText canonicalSend
    , raw = Aeson.toJSON canonicalSend
    }

rememberMessageNumber :: Concurrent :> es => RpcState -> MessageId -> Eff es ()
rememberMessageNumber rpcState messageId =
  case Text.stripPrefix "session-" (messageIdText messageId) >>= readMaybe . Text.unpack of
    Nothing ->
      pure ()
    Just number ->
      STM.atomically $
        STM.modifyTVar' rpcState.nextMessageNumber (max (number + 1))

rememberSessionNumber :: Concurrent :> es => RpcState -> Text -> Eff es ()
rememberSessionNumber rpcState sessionId =
  case readMaybe . Text.unpack =<< viaNonEmpty last (Text.splitOn "-" sessionId) of
    Nothing ->
      pure ()
    Just number ->
      STM.atomically $
        STM.modifyTVar' rpcState.nextSessionNumber (max (number + 1))

sessionIdFromMessage :: IncomingMessage -> RpcSessionId
sessionIdFromMessage message =
  Session.SessionId (fromMaybe "session" (listToMaybe message.chatAliases))

storedMessageToRpc :: Storage.StoredChatMessage -> RpcChatMessage
storedMessageToRpc =
  Session.storedMessageToSession

broadcastClient :: Aeson.Value -> RpcClientId -> RpcClientQueue -> STM.STM (Maybe RpcClientQueue)
broadcastClient value _clientId (RpcClientQueue queue) = do
  full <- STM.isFullTBQueue queue
  if full
    then do
      drainTBQueue queue
      STM.writeTBQueue queue (RpcClientDisconnect "RPC notification queue overflow")
      pure Nothing
    else do
      STM.writeTBQueue queue (RpcClientSend value)
      pure (Just (RpcClientQueue queue))

drainTBQueue :: STM.TBQueue a -> STM.STM ()
drainTBQueue queue =
  STM.tryReadTBQueue queue >>= \case
    Nothing ->
      pure ()
    Just _ ->
      drainTBQueue queue

rpcClientQueueCapacity :: Natural
rpcClientQueueCapacity = 256

storedMediaRef :: Media.MediaFileInfo -> Text -> Storage.StoredMediaRef
storedMediaRef =
  Session.storedMediaRef

parseMediaId :: Text -> Maybe Text
parseMediaId =
  Session.parseMediaId

rpcChatSendToSession :: RpcChatSend -> Session.SessionSend
rpcChatSendToSession chatSend =
  Session.SessionSend
    { sessionId = chatSend.sessionId
    , text = chatSend.text
    , imageUrls = chatSend.imageUrls
    , attachments = map rpcAttachmentToSession chatSend.attachments
    , replyToMessageId = chatSend.replyToMessageId
    }

rpcAttachmentToSession :: RpcChatAttachmentRef -> Session.SessionAttachmentRef
rpcAttachmentToSession attachment =
  Session.SessionAttachmentRef
    { attachmentId = attachment.attachmentId
    , name = attachment.name
    , mediaType = attachment.mediaType
    , kind = attachment.kind
    , size = attachment.size
    , url = attachment.url
    }

defaultMediaUrl :: Text -> Text
defaultMediaUrl attachmentId =
  attachmentId

kindFromMediaType :: Text -> Text
kindFromMediaType mediaType
  | "image/" `Text.isPrefixOf` media = "image"
  | "audio/" `Text.isPrefixOf` media = "audio"
  | otherwise = "file"
  where
    media = Text.toLower mediaType
