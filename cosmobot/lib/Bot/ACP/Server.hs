{-# LANGUAGE RankNTypes #-}
{-|
Module      : Bot.ACP.Server
Description : Minimal Agent Client Protocol websocket server
Stability   : experimental
-}

module Bot.ACP.Server
  ( runAcpServer
  , acpServerApplication
  , acpServerApp
  , dispatchAcpRequest
  )
where

import qualified Bot.ACP.Config as Config
import qualified Bot.ACP.Content as Content
import qualified Bot.ACP.State as State
import qualified Bot.JSONRPC as RPC
import Bot.Core.Message (ChatPlatform (PlatformACP), IncomingMessage (..), MessageId)
import Bot.Core.Thread (ThreadMessageKey (..))
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import qualified Bot.Session as Session
import Bot.Storage.Thread (ThreadStore, haltThread)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.FileSystem as FileSystem
import qualified JSONRPC
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

data AcpClientDisconnected = AcpClientDisconnected Text
  deriving (Show)

instance Exception AcpClientDisconnected

data AcpPromptDispatchError
  = AcpPromptInvalid !Text
  | AcpPromptSessionNotFound
  deriving (Show)

instance Exception AcpPromptDispatchError

data PromptParams = PromptParams
  { sessionId :: !State.AcpSessionId
  , text :: !Text
  , imageUrls :: ![Text]
  }
  deriving (Eq, Show)

data ListSessionsParams = ListSessionsParams
  { cwd :: !(Maybe Text)
  , cursor :: !(Maybe Text)
  }
  deriving (Eq, Show)

data InitializeParams = InitializeParams
  { clientCapabilities :: !State.AcpClientCapabilities
  }
  deriving (Eq, Show)

runAcpServer
  :: (IOE :> es, KatipE :> es, Concurrency.Concurrency :> es, Prim :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => Config.Config
  -> ThreadStore
  -> State.AcpState
  -> Eff es ()
runAcpServer cfg@Config.Config{enabled} threads acpState =
  when enabled do
    let Config.Config{host, port} = cfg
        settings =
          Warp.setHost (fromString host) $
            Warp.setPort port Warp.defaultSettings
    logInfo [i|ACP server listening on #{host}:#{port}; websocket endpoint /acp|]
    withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
      liftIO $
        Warp.runSettings settings (acpServerApplication runInIO cfg threads acpState)

acpServerApplication
  :: (IOE :> es, KatipE :> es, Concurrency.Concurrency :> es, Prim :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => (forall a. Eff es a -> IO a)
  -> Config.Config
  -> ThreadStore
  -> State.AcpState
  -> Wai.Application
acpServerApplication runInIO cfg threads acpState =
  WaiWS.websocketsOr WS.defaultConnectionOptions websocketApp httpApp
  where
    websocketApp pending =
      runInIO (acpServerApp cfg threads acpState pending)

acpServerApp
  :: (IOE :> es, KatipE :> es, Concurrency.Concurrency :> es, Prim :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => Config.Config
  -> ThreadStore
  -> State.AcpState
  -> WS.PendingConnection
  -> Eff es ()
acpServerApp cfg threads acpState pending
  | not (requestIsAcpPath (WS.pendingRequest pending)) =
      liftIO $
        WS.rejectRequestWith pending $
          WS.defaultRejectRequest
            { WS.rejectCode = 404
            , WS.rejectMessage = "Not Found"
            , WS.rejectBody = "not found"
            }
  | requestIsAuthorized cfg (WS.pendingRequest pending) = do
      conn <- liftIO (WS.acceptRequest pending)
      serveAcceptedClient threads acpState conn
  | otherwise =
      liftIO $
        WS.rejectRequestWith pending $
          WS.defaultRejectRequest
            { WS.rejectCode = 401
            , WS.rejectMessage = "Unauthorized"
            , WS.rejectBody = "unauthorized"
            }

serveAcceptedClient
  :: (IOE :> es, KatipE :> es, Concurrency.Concurrency :> es, Prim :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => ThreadStore
  -> State.AcpState
  -> WS.Connection
  -> Eff es ()
serveAcceptedClient threads acpState conn = do
  (clientId, queue) <- State.registerClient acpState
  logDebug [i|ACP client #{clientId} connected|]
  (Concurrency.raceTasks_
      [i|acp.client.#{clientId}.writer|]
      (writeQueuedFrames queue conn)
      [i|acp.client.#{clientId}.reader|]
      (readRequestFrames threads acpState queue conn)
    `catchSync` \err ->
      logDebug [i|ACP client #{clientId} disconnected: #{displayException err}|])
    `finally` do
      State.unregisterClient acpState clientId
      logDebug [i|ACP client #{clientId} unregistered|]

writeQueuedFrames
  :: (IOE :> es, Concurrent :> es)
  => State.AcpClientQueue
  -> WS.Connection
  -> Eff es ()
writeQueuedFrames queue conn =
  forever do
    State.readClient queue >>= \case
      State.AcpClientSend value ->
        liftIO (WS.sendTextData conn (Aeson.encode value))
      State.AcpClientDisconnect reason -> do
        liftIO (WS.sendClose conn reason)
        throwIO (AcpClientDisconnected reason)

readRequestFrames
  :: (IOE :> es, KatipE :> es, Prim :> es, Concurrent :> es, Concurrency.Concurrency :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => ThreadStore
  -> State.AcpState
  -> State.AcpClientQueue
  -> WS.Connection
  -> Eff es ()
readRequestFrames threads acpState queue conn =
  forever do
    bytes <- liftIO (WS.receiveData conn :: IO ByteString)
    response <- case Aeson.eitherDecodeStrict bytes of
      Left err ->
        pure (Just (RPC.parseErrorResponse (Text.pack err)))
      Right value ->
        case Aeson.fromJSON value of
          Aeson.Success (JSONRPC.RequestMessage request) ->
            dispatchAcpRequestFrame threads acpState queue request
          Aeson.Success (JSONRPC.NotificationMessage notification_) -> do
            _ <- dispatchAcpRequestWithThreadStore threads acpState queue (notificationToRequest notification_)
            pure Nothing
          Aeson.Success message@(JSONRPC.ResponseMessage{}) ->
            resolveClientMessage message
          Aeson.Success message@(JSONRPC.ErrorMessage{}) ->
            resolveClientMessage message
          Aeson.Error err ->
            pure (Just (RPC.invalidRequestResponse (Text.pack err)))
    traverse_ (State.writeClient queue . Aeson.toJSON) response
  where
    resolveClientMessage message =
      State.resolveClientResponse acpState queue message <&> \case
        True ->
          Nothing
        False ->
          Just (RPC.invalidRequestResponse "Unexpected ACP client response")

dispatchAcpRequestFrame
  :: (KatipE :> es, Prim :> es, Concurrent :> es, Concurrency.Concurrency :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => ThreadStore
  -> State.AcpState
  -> State.AcpClientQueue
  -> RPC.JsonRpcRequest
  -> Eff es (Maybe RPC.JsonRpcResponse)
dispatchAcpRequestFrame threads acpState queue request
  | RPC.requestMethod request == "session/prompt" = do
      Concurrency.fire "acp.session.prompt" do
        response <- dispatchPrompt acpState queue request
        State.writeClient queue (Aeson.toJSON response)
      pure Nothing
  | otherwise =
      Just <$> dispatchAcpRequestWithThreadStore threads acpState queue request

dispatchAcpRequest
  :: (Concurrent :> es, Concurrency.Concurrency :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => State.AcpState
  -> State.AcpClientQueue
  -> RPC.JsonRpcRequest
  -> Eff es RPC.JsonRpcResponse
dispatchAcpRequest acpState queue request =
  dispatchAcpRequestWithCancel (\_ _ -> pure ()) acpState queue request

dispatchAcpRequestWithThreadStore
  :: (Concurrent :> es, Concurrency.Concurrency :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es, KatipE :> es, Prim :> es)
  => ThreadStore
  -> State.AcpState
  -> State.AcpClientQueue
  -> RPC.JsonRpcRequest
  -> Eff es RPC.JsonRpcResponse
dispatchAcpRequestWithThreadStore threads =
  dispatchAcpRequestWithCancel \_sessionId messageIds ->
    traverse_ (cancelAcpThread threads) messageIds

dispatchAcpRequestWithCancel
  :: (Concurrent :> es, Concurrency.Concurrency :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => (State.AcpSessionId -> [MessageId] -> Eff es ())
  -> State.AcpState
  -> State.AcpClientQueue
  -> RPC.JsonRpcRequest
  -> Eff es RPC.JsonRpcResponse
dispatchAcpRequestWithCancel cancelMessages acpState queue request =
  case RPC.requestMethod request of
    "initialize" ->
      dispatchInitialize acpState queue request
    "authenticate" ->
      pure (RPC.successResponse (RPC.requestId request) (Aeson.object []))
    "session/new" ->
      dispatchNewSession request
    "session/list" ->
      dispatchListSessions request
    "session/load" ->
      dispatchLoadSession queue request
    "session/resume" ->
      dispatchExistingSession request (RPC.successResponse (RPC.requestId request) (Aeson.object []))
    "session/close" ->
      dispatchExistingSession request (RPC.successResponse (RPC.requestId request) (Aeson.object []))
    "session/cancel" ->
      dispatchCancelSession cancelMessages acpState request
    "session/delete" ->
      dispatchDeleteSession request
    "session/prompt" ->
      dispatchPrompt acpState queue request
    method ->
      pure (RPC.errorResponse (RPC.requestId request) "method_not_found" [i|Unknown ACP method: #{method}|])

initializeResponse :: Aeson.Value
initializeResponse =
  Aeson.object
    [ "protocolVersion" Aeson..= (1 :: Int)
    , "agentCapabilities" Aeson..=
        Aeson.object
          [ "loadSession" Aeson..= True
          , "promptCapabilities" Aeson..=
              Aeson.object
                [ "image" Aeson..= True
                , "audio" Aeson..= False
                , "embeddedContext" Aeson..= False
                ]
          , "sessionCapabilities" Aeson..=
              Aeson.object
                [ "delete" Aeson..= Aeson.object []
                , "list" Aeson..= Aeson.object []
                , "resume" Aeson..= Aeson.object []
                , "close" Aeson..= Aeson.object []
                ]
          ]
    , "agentInfo" Aeson..=
        Aeson.object
          [ "name" Aeson..= ("cosmobot" :: Text)
          , "title" Aeson..= ("Cosmobot" :: Text)
          , "version" Aeson..= ("0.1.0.0" :: Text)
          ]
    , "authMethods" Aeson..= ([] :: [Aeson.Value])
    ]

dispatchInitialize
  :: Concurrent :> es
  => State.AcpState
  -> State.AcpClientQueue
  -> RPC.JsonRpcRequest
  -> Eff es RPC.JsonRpcResponse
dispatchInitialize acpState queue request =
  case AesonTypes.parseEither parseInitializeParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right InitializeParams{clientCapabilities} -> do
      State.setClientCapabilities acpState queue clientCapabilities
      pure (RPC.successResponse (RPC.requestId request) initializeResponse)

dispatchNewSession
  :: Storage.Storage :> es
  => RPC.JsonRpcRequest
  -> Eff es RPC.JsonRpcResponse
dispatchNewSession request =
  case AesonTypes.parseEither parseNewSessionParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right label -> do
      session <- State.openSession label
      pure $
        RPC.successResponse (RPC.requestId request) $
          Aeson.object
            [ "sessionId" Aeson..= State.acpSessionIdText session.sessionId
            ]

dispatchDeleteSession
  :: (Storage.Storage :> es, FileSystem.FileSystem :> es)
  => RPC.JsonRpcRequest
  -> Eff es RPC.JsonRpcResponse
dispatchDeleteSession request =
  case AesonTypes.parseEither parseSessionIdParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right sessionId -> do
      _deleted <- State.deleteSession sessionId
      pure (RPC.successResponse (RPC.requestId request) (Aeson.object []))

dispatchListSessions
  :: Storage.Storage :> es
  => RPC.JsonRpcRequest
  -> Eff es RPC.JsonRpcResponse
dispatchListSessions request =
  case AesonTypes.parseEither parseListSessionsParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right ListSessionsParams{cursor = Just _} ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" "Invalid session/list cursor")
    Right ListSessionsParams{cwd} -> do
      sessions <- filter (matchesCwd cwd) <$> Session.listSessions
      pure $
        RPC.successResponse (RPC.requestId request) $
          Aeson.object
            [ "sessions" Aeson..= map sessionInfo sessions
            ]

dispatchLoadSession
  :: (Concurrent :> es, Storage.Storage :> es)
  => State.AcpClientQueue
  -> RPC.JsonRpcRequest
  -> Eff es RPC.JsonRpcResponse
dispatchLoadSession queue request =
  case AesonTypes.parseEither parseSessionIdParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right sessionId ->
      Session.getSession sessionId >>= \case
        Nothing ->
          pure (RPC.errorResponse (RPC.requestId request) "not_found" "Session not found")
        Just _ -> do
          history <- Session.sessionHistory sessionId
          traverse_ (writeSessionReplay queue) history
          pure (RPC.successResponse (RPC.requestId request) Aeson.Null)

dispatchExistingSession
  :: Storage.Storage :> es
  => RPC.JsonRpcRequest
  -> RPC.JsonRpcResponse
  -> Eff es RPC.JsonRpcResponse
dispatchExistingSession request response =
  case AesonTypes.parseEither parseSessionIdParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right sessionId ->
      Session.getSession sessionId >>= \case
        Nothing ->
          pure (RPC.errorResponse (RPC.requestId request) "not_found" "Session not found")
        Just _ ->
          pure response

dispatchCancelSession
  :: (Concurrent :> es, Storage.Storage :> es)
  => (State.AcpSessionId -> [MessageId] -> Eff es ())
  -> State.AcpState
  -> RPC.JsonRpcRequest
  -> Eff es RPC.JsonRpcResponse
dispatchCancelSession cancelMessages acpState request =
  case AesonTypes.parseEither parseSessionIdParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right sessionId ->
      Session.getSession sessionId >>= \case
        Nothing ->
          pure (RPC.errorResponse (RPC.requestId request) "not_found" "Session not found")
        Just _ -> do
          messageIds <- State.cancelSessionPrompts acpState sessionId
          cancelMessages sessionId messageIds
          pure (RPC.successResponse (RPC.requestId request) (Aeson.object []))

dispatchPrompt
  :: (Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => State.AcpState
  -> State.AcpClientQueue
  -> RPC.JsonRpcRequest
  -> Eff es RPC.JsonRpcResponse
dispatchPrompt acpState queue request =
  case AesonTypes.parseEither parsePromptParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right prompt ->
      ( State.withActiveSessionClient acpState queue prompt.sessionId $
          State.withPromptWaiter acpState prompt.sessionId (enqueuePrompt prompt) >>= \case
          State.PromptCompleted messageId -> do
            State.completeSessionPrompt acpState prompt.sessionId
            pure $
              RPC.successResponse (RPC.requestId request) $
                Aeson.object
                  [ "stopReason" Aeson..= ("end_turn" :: Text)
                  , "messageId" Aeson..= messageId
                  ]
          State.PromptCancelled ->
            pure $
              RPC.successResponse (RPC.requestId request) $
                Aeson.object
                  [ "stopReason" Aeson..= ("cancelled" :: Text)
                  ]
      ) `catchSync` \err ->
          case fromException err of
            Just (AcpPromptInvalid invalidParams) ->
              pure (RPC.errorResponse (RPC.requestId request) "invalid_params" invalidParams)
            Just AcpPromptSessionNotFound ->
              pure (RPC.errorResponse (RPC.requestId request) "not_found" "Session not found")
            Nothing ->
              throwIO err
  where
    enqueuePrompt prompt = do
      userMessage <- State.enqueuePromptMessage acpState $
        Session.SessionSend
          { sessionId = prompt.sessionId
          , text = prompt.text
          , imageUrls = prompt.imageUrls
          , attachments = []
          , replyToMessageId = Nothing
          }
      case userMessage of
        Left err ->
          throwIO (AcpPromptInvalid err)
        Right Nothing ->
          throwIO AcpPromptSessionNotFound
        Right (Just IncomingMessage{messageId}) ->
          for_ messageId \userMessageId ->
            writeMessageChunkUpdates
              queue
              prompt.sessionId
              "user_message_chunk"
              userMessageId
              (Content.messageContentBlocks prompt.text prompt.imageUrls [])

cancelAcpThread
  :: (Concurrency.Concurrency :> es, Storage.Storage :> es, KatipE :> es, Prim :> es, Concurrent :> es)
  => ThreadStore
  -> MessageId
  -> Eff es ()
cancelAcpThread threads messageId =
  void (haltThread threads Concurrency.cancel (acpThreadMessageKey messageId))

acpThreadMessageKey :: MessageId -> ThreadMessageKey
acpThreadMessageKey messageId =
  ThreadMessageKey
    { platform = PlatformACP
    , chatId = Nothing
    , messageId
    }

parseNewSessionParams :: Aeson.Value -> AesonTypes.Parser (Maybe Text)
parseNewSessionParams =
  Aeson.withObject "session/new params" \o ->
    o Aeson..:? "cwd"

parseInitializeParams :: Aeson.Value -> AesonTypes.Parser InitializeParams
parseInitializeParams =
  Aeson.withObject "initialize params" \o -> do
    capabilities <- o Aeson..:? "clientCapabilities" Aeson..!= Aeson.object []
    fs <- Aeson.withObject "clientCapabilities" (\co -> co Aeson..:? "fs" Aeson..!= Aeson.object []) capabilities
    readTextFile <- Aeson.withObject "clientCapabilities.fs" (\fo -> fo Aeson..:? "readTextFile" Aeson..!= False) fs
    writeTextFile <- Aeson.withObject "clientCapabilities.fs" (\fo -> fo Aeson..:? "writeTextFile" Aeson..!= False) fs
    terminal <- Aeson.withObject "clientCapabilities" (\co -> co Aeson..:? "terminal" >>= parseTerminalCapability) capabilities
    pure InitializeParams
      { clientCapabilities = State.AcpClientCapabilities{readTextFile, writeTextFile, terminal}
      }

parseTerminalCapability :: Maybe Aeson.Value -> AesonTypes.Parser Bool
parseTerminalCapability = \case
  Nothing ->
    pure False
  Just (Aeson.Bool supported) ->
    pure supported
  Just (Aeson.Object _) ->
    pure True
  Just value ->
    AesonTypes.typeMismatch "terminal capability" value

parseSessionIdParams :: Aeson.Value -> AesonTypes.Parser State.AcpSessionId
parseSessionIdParams =
  Aeson.withObject "session params" \o ->
    Session.SessionId <$> o Aeson..: "sessionId"

parseListSessionsParams :: Aeson.Value -> AesonTypes.Parser ListSessionsParams
parseListSessionsParams Aeson.Null =
  pure ListSessionsParams{cwd = Nothing, cursor = Nothing}
parseListSessionsParams value =
  Aeson.withObject "session/list params" parseObject value
  where
    parseObject o =
      ListSessionsParams
        <$> o Aeson..:? "cwd"
        <*> o Aeson..:? "cursor"

parsePromptParams :: Aeson.Value -> AesonTypes.Parser PromptParams
parsePromptParams =
  Aeson.withObject "session/prompt params" \o -> do
    sessionId <- Session.SessionId <$> o Aeson..: "sessionId"
    prompt <- o Aeson..: "prompt"
    content <- Content.parsePromptContent prompt
    pure PromptParams{sessionId, text = content.text, imageUrls = content.imageUrls}

sessionUpdateNotification :: State.AcpSessionId -> Aeson.Value -> JSONRPC.JSONRPCMessage
sessionUpdateNotification sessionId update =
  JSONRPC.NotificationMessage $
    JSONRPC.JSONRPCNotification
      JSONRPC.rPC_VERSION
      "session/update"
      ( Aeson.object
          [ "sessionId" Aeson..= State.acpSessionIdText sessionId
          , "update" Aeson..= update
          ]
      )

writeSessionReplay
  :: Concurrent :> es
  => State.AcpClientQueue
  -> Session.SessionMessage
  -> Eff es ()
writeSessionReplay queue message =
  writeMessageChunkUpdates
    queue
    message.sessionId
    (sessionUpdateKind message.sender)
    message.messageId
    (Content.messageContentBlocks message.text message.imageUrls message.attachments)

writeMessageChunkUpdates
  :: Concurrent :> es
  => State.AcpClientQueue
  -> State.AcpSessionId
  -> Text
  -> MessageId
  -> [Content.AcpContentBlock]
  -> Eff es ()
writeMessageChunkUpdates queue sessionId updateKind messageId blocks =
  traverse_
    ( State.writeClient queue
        . Aeson.toJSON
        . sessionUpdateNotification sessionId
        . messageChunkUpdate updateKind messageId
    )
    blocks

messageChunkUpdate :: Text -> MessageId -> Content.AcpContentBlock -> Aeson.Value
messageChunkUpdate updateKind messageId content =
  Aeson.object
    [ "sessionUpdate" Aeson..= updateKind
    , "messageId" Aeson..= messageId
    , "content" Aeson..= Content.contentBlockValue content
    ]

sessionUpdateKind :: Text -> Text
sessionUpdateKind sender
  | sender == "user" =
      "user_message_chunk"
  | otherwise =
      "agent_message_chunk"

sessionInfo :: Session.Session -> Aeson.Value
sessionInfo session =
  Aeson.object
    ( [ "sessionId" Aeson..= State.acpSessionIdText session.sessionId
      , "cwd" Aeson..= sessionCwd session
      ]
        <> maybe [] (\title -> ["title" Aeson..= title]) session.label
    )

matchesCwd :: Maybe Text -> Session.Session -> Bool
matchesCwd Nothing _ =
  True
matchesCwd (Just cwd) session =
  sessionCwd session == cwd

sessionCwd :: Session.Session -> Text
sessionCwd session =
  fromMaybe "/" session.label

notificationToRequest :: RPC.JsonRpcNotification -> RPC.JsonRpcRequest
notificationToRequest notification_ =
  JSONRPC.JSONRPCRequest JSONRPC.rPC_VERSION (JSONRPC.RequestId Aeson.Null) notification_.method notification_.params

requestIsAuthorized :: Config.Config -> WS.RequestHead -> Bool
requestIsAuthorized cfg request =
  authorizationBearer request == Just expectedToken
  where
    Config.Config{token} = cfg
    expectedToken = TextEncoding.encodeUtf8 token

requestIsAcpPath :: WS.RequestHead -> Bool
requestIsAcpPath request =
  path == "/acp"
  where
    (path, _) = ByteString.break (== questionMark) request.requestPath

httpApp :: Wai.Application
httpApp request respond =
  case Wai.requestMethod request of
    "GET" ->
      respond $
        textResponse Http.status404 "not found"
    "HEAD" ->
      respond $
        Wai.responseLBS Http.status404 (baseSecurityHeaders []) ""
    _ ->
      respond $
        textResponse Http.status405 "method not allowed"

authorizationBearer :: WS.RequestHead -> Maybe ByteString
authorizationBearer request =
  ByteString.stripPrefix bearerPrefix =<< (snd <$> find ((== "Authorization") . fst) request.requestHeaders)

textResponse :: Http.Status -> LazyByteString.ByteString -> Wai.Response
textResponse status body =
  Wai.responseLBS status (baseSecurityHeaders [("Content-Type", "text/plain; charset=utf-8")]) body

baseSecurityHeaders :: Http.ResponseHeaders -> Http.ResponseHeaders
baseSecurityHeaders headers =
  headers
    <> [ ("Referrer-Policy", "no-referrer")
       , ("X-Content-Type-Options", "nosniff")
       , ("X-Frame-Options", "DENY")
       ]

questionMark :: Word8
questionMark = 63

bearerPrefix :: ByteString
bearerPrefix = "Bearer "
