{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.RPC.Server
Description : Local JSON-RPC websocket server
Stability   : experimental
-}

module Bot.RPC.Server
  ( RpcServerCallbacks (..)
  , noRpcServerCallbacks
  , withManagerRpcCallbacks
  , runRpcServer
  , rpcServerApplication
  , rpcServerApp
  , dispatchRpcRequest
  , dispatchRpcRequestWithConfig
  )
where

import Bot.Prelude
import Bot.Core.Message (ChatPlatform (PlatformRPC), IncomingMessage (..), MessageId)
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Resource as Resource
import Bot.Effect.Media (MediaObject (..))
import qualified Bot.Effect.Storage as Storage
import qualified Bot.RPC.Config as Config
import qualified Bot.JSONRPC as RPC
import qualified Bot.RPC.State as State
import qualified Bot.Session as Session
import qualified Bot.Storage.RPC as RpcStorage
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Streaming.ByteString as Q
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.FileSystem as FileSystem
import qualified JSONRPC
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

data RpcServerCallbacks es = RpcServerCallbacks
  { auditMethod :: RPC.RpcRequest -> Eff es (Maybe (Either RPC.RpcError Aeson.Value))
  , managerMethod :: RPC.RpcRequest -> Eff es (Maybe RPC.RpcResponse)
  }

data RpcAttachmentUpload = RpcAttachmentUpload
  { name :: !Text
  , mediaType :: !Text
  , kind :: !Text
  , bytes :: !ByteString
  }
  deriving (Eq, Show)

newtype MediaStatsParams = MediaStatsParams
  { limit :: Int
  }

newtype MediaGcParams = MediaGcParams
  { maxAgeSeconds :: Int
  }

newtype RpcClientDisconnected = RpcClientDisconnected Text
  deriving (Show)

instance Exception RpcClientDisconnected

noRpcServerCallbacks :: RpcServerCallbacks es
noRpcServerCallbacks = RpcServerCallbacks
  { auditMethod = \_ -> pure Nothing
  , managerMethod = \_ -> pure Nothing
  }

withManagerRpcCallbacks
  :: (Concurrency.Concurrency :> es, Resource.Resource :> es)
  => RpcServerCallbacks es
  -> RpcServerCallbacks es
withManagerRpcCallbacks callbacks = callbacks
  { managerMethod = fmap Just . dispatchManagerRequest
  }

runRpcServer
  :: (IOE :> es, KatipE :> es, Concurrency.Concurrency :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => Config.Config
  -> State.RpcState
  -> RpcServerCallbacks es
  -> Eff es ()
runRpcServer cfg@Config.Config{enabled} rpcState callbacks = do
  if enabled
    then runRpcServer' cfg rpcState callbacks
    else pure ()

runRpcServer'
  :: (IOE :> es, KatipE :> es, Concurrency.Concurrency :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => Config.Config
  -> State.RpcState
  -> RpcServerCallbacks es
  -> Eff es ()
runRpcServer' cfg rpcState callbacks = do
  let Config.Config{host, port} = cfg
      settings =
        Warp.setHost (fromString host) $
          Warp.setPort port Warp.defaultSettings
  logInfo [i|RPC server listening on #{host}:#{port}; websocket endpoint /rpc|]
  withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
    liftIO $
      Warp.runSettings settings (rpcServerApplication runInIO cfg rpcState callbacks)

rpcServerApplication
  :: (IOE :> es, KatipE :> es, Concurrency.Concurrency :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => (forall a. Eff es a -> IO a)
  -> Config.Config
  -> State.RpcState
  -> RpcServerCallbacks es
  -> Wai.Application
rpcServerApplication runInIO cfg rpcState callbacks =
  WaiWS.websocketsOr WS.defaultConnectionOptions websocketApp (httpApp runInIO cfg)
  where
    websocketApp pending =
      runInIO (rpcServerApp cfg rpcState callbacks pending)

rpcServerApp
  :: (IOE :> es, KatipE :> es, Concurrency.Concurrency :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => Config.Config
  -> State.RpcState
  -> RpcServerCallbacks es
  -> WS.PendingConnection
  -> Eff es ()
rpcServerApp cfg rpcState callbacks pending
  | not (requestIsRpcPath (WS.pendingRequest pending)) =
      liftIO $
        WS.rejectRequestWith pending $
          WS.defaultRejectRequest
            { WS.rejectCode = 404
            , WS.rejectMessage = "Not Found"
            , WS.rejectBody = "not found"
            }
  | requestIsAuthorized cfg (WS.pendingRequest pending) = do
      conn <- liftIO (WS.acceptRequest pending)
      serveAcceptedClient cfg rpcState callbacks conn
  | otherwise =
      liftIO $
        WS.rejectRequestWith pending $
          WS.defaultRejectRequest
            { WS.rejectCode = 401
            , WS.rejectMessage = "Unauthorized"
            , WS.rejectBody = "unauthorized"
            }

serveAcceptedClient
  :: (IOE :> es, KatipE :> es, Concurrency.Concurrency :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => Config.Config
  -> State.RpcState
  -> RpcServerCallbacks es
  -> WS.Connection
  -> Eff es ()
serveAcceptedClient cfg rpcState callbacks conn = do
  (clientId, queue) <- State.registerClient rpcState
  logDebug [i|RPC client #{clientId} connected|]
  (Concurrency.raceTasks_
      [i|rpc.client.#{clientId}.writer|]
      (writeQueuedFrames queue conn)
      [i|rpc.client.#{clientId}.reader|]
      (readRequestFrames cfg rpcState callbacks queue conn)
    `catchSync` \err ->
      logDebug [i|RPC client #{clientId} disconnected: #{displayException err}|])
    `finally` do
      State.unregisterClient rpcState clientId
      logDebug [i|RPC client #{clientId} unregistered|]

writeQueuedFrames
  :: (IOE :> es, Concurrent :> es)
  => State.RpcClientQueue
  -> WS.Connection
  -> Eff es ()
writeQueuedFrames queue conn =
  forever do
    State.readClient queue >>= \case
      State.RpcClientSend value ->
        liftIO (WS.sendTextData conn (Aeson.encode value))
      State.RpcClientDisconnect reason -> do
        liftIO (WS.sendClose conn reason)
        throwIO (RpcClientDisconnected reason)

readRequestFrames
  :: (IOE :> es, Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => Config.Config
  -> State.RpcState
  -> RpcServerCallbacks es
  -> State.RpcClientQueue
  -> WS.Connection
  -> Eff es ()
readRequestFrames cfg rpcState callbacks queue conn =
  forever do
    bytes <- liftIO (WS.receiveData conn :: IO ByteString)
    response <- case Aeson.eitherDecodeStrict bytes of
      Left err ->
        pure (Just (RPC.parseErrorResponse (Text.pack err)))
      Right value ->
        case Aeson.fromJSON value of
          Aeson.Success (JSONRPC.RequestMessage request) ->
            Just <$> dispatchRpcRequestWithConfig rpcState cfg callbacks request
          Aeson.Success (JSONRPC.NotificationMessage notification_) -> do
            _ <- dispatchRpcRequestWithConfig rpcState cfg callbacks (notificationToRequest notification_)
            pure Nothing
          Aeson.Error err ->
            pure (Just (RPC.invalidRequestResponse (Text.pack err)))
          Aeson.Success _ ->
            pure (Just (RPC.invalidRequestResponse "Expected request or notification"))
    traverse_ (State.writeClient queue . Aeson.toJSON) response

dispatchRpcRequest
  :: (Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => State.RpcState
  -> RpcServerCallbacks es
  -> RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchRpcRequest rpcState callbacks request =
  dispatchRpcRequestWithConfig rpcState defaultDispatchConfig callbacks request

dispatchRpcRequestWithConfig
  :: (Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => State.RpcState
  -> Config.Config
  -> RpcServerCallbacks es
  -> RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchRpcRequestWithConfig rpcState cfg callbacks request =
  dispatchRpcRequestUnsafe rpcState cfg callbacks request
    `catchSync` \err ->
      pure $
        RPC.errorResponse
          (RPC.requestId request)
          "internal_error"
          [i|RPC request failed: #{exceptionFirstLine err}|]

exceptionFirstLine :: Exception err => err -> Text
exceptionFirstLine =
  Text.takeWhile (/= '\n') . toText . displayException

dispatchRpcRequestUnsafe
  :: (Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => State.RpcState
  -> Config.Config
  -> RpcServerCallbacks es
  -> RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchRpcRequestUnsafe rpcState _cfg callbacks request =
  case RPC.requestMethod request of
    "chat.open_session" ->
      dispatchOpenSession rpcState request
    "chat.list_sessions" ->
      dispatchListSessions request
    "chat.get_session" ->
      dispatchGetSession request
    "chat.history" ->
      dispatchHistory request
    "chat.fork" ->
      dispatchFork rpcState request
    "chat.rename_session" ->
      dispatchRenameSession request
    "chat.delete_session" ->
      dispatchDeleteSession request
    "chat.upload_attachment" ->
      dispatchUploadAttachment request
    "chat.send" ->
      dispatchChatSend rpcState request
    "media.resolve_source" ->
      dispatchMediaResolveSource request
    "media.get" ->
      dispatchMediaGet request
    "media.delete" ->
      dispatchMediaDelete request
    "media.stats" ->
      dispatchMediaStats request
    "media.gc" ->
      dispatchMediaGc request
    method
      | "audit." `Text.isPrefixOf` method ->
          dispatchAudit callbacks request
      | "concurrency." `Text.isPrefixOf` method || "resource." `Text.isPrefixOf` method ->
          dispatchManager callbacks request
      | otherwise ->
          pure (methodNotFound (RPC.requestId request) method)

dispatchOpenSession
  :: (Concurrent :> es, Storage.Storage :> es)
  => State.RpcState
  -> RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchOpenSession rpcState request =
  case AesonTypes.parseEither parseOpenSessionParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right label -> do
      session <- State.openChatSession rpcState label
      pure $
        RPC.successResponse (RPC.requestId request) $
          Aeson.object
            [ "sessionId" Aeson..= rpcSessionIdText session.sessionId
            , "session" Aeson..= session
            ]

dispatchListSessions
  :: Storage.Storage :> es
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchListSessions request = do
  sessions <- State.listChatSessions
  pure $
    RPC.successResponse (RPC.requestId request) $
      Aeson.object ["sessions" Aeson..= sessions]

dispatchGetSession
  :: Storage.Storage :> es
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchGetSession request =
  case AesonTypes.parseEither parseSessionIdParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right sessionId -> do
      session <- State.getChatSession sessionId
      history <- maybe (pure []) (const (State.chatHistory sessionId)) session
      pure $
        RPC.successResponse (RPC.requestId request) $
          Aeson.object
            [ "session" Aeson..= session
            , "messages" Aeson..= history
            ]

dispatchHistory
  :: Storage.Storage :> es
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchHistory request =
  case AesonTypes.parseEither parseSessionIdParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right sessionId -> do
      messages <- State.chatHistory sessionId
      pure $
        RPC.successResponse (RPC.requestId request) $
          Aeson.object
            [ "sessionId" Aeson..= rpcSessionIdText sessionId
            , "messages" Aeson..= messages
            ]

dispatchFork
  :: (Concurrent :> es, Storage.Storage :> es)
  => State.RpcState
  -> RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchFork rpcState request =
  case AesonTypes.parseEither parseForkParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right (sessionId, messageId, label) -> do
      forked <- State.forkChatSession rpcState sessionId messageId label
      case forked of
        Nothing ->
          pure (RPC.errorResponse (RPC.requestId request) "not_found" "Session or message not found")
        Just session ->
          pure $
            RPC.successResponse (RPC.requestId request) $
              Aeson.object
                [ "sessionId" Aeson..= rpcSessionIdText session.sessionId
                , "session" Aeson..= session
                ]

dispatchRenameSession
  :: Storage.Storage :> es
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchRenameSession request =
  case AesonTypes.parseEither parseRenameSessionParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right (sessionId, label) -> do
      renamed <- State.renameChatSession sessionId label
      case renamed of
        Nothing ->
          pure (RPC.errorResponse (RPC.requestId request) "not_found" "Session not found")
        Just session ->
          pure $
            RPC.successResponse (RPC.requestId request) $
              Aeson.object ["session" Aeson..= session]

dispatchDeleteSession
  :: (Storage.Storage :> es, FileSystem.FileSystem :> es)
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchDeleteSession request =
  case AesonTypes.parseEither parseSessionIdParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right sessionId -> do
      deleted <- State.deleteChatSession sessionId
      pure $
        RPC.successResponse (RPC.requestId request) $
          Aeson.object
            [ "sessionId" Aeson..= rpcSessionIdText sessionId
            , "deleted" Aeson..= deleted
            ]

dispatchUploadAttachment
  :: Media.Media :> es
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchUploadAttachment request =
  case AesonTypes.parseEither (parseAttachmentUploadParams defaultUploadMaxBytes) (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right upload -> do
      storedRef <- Media.storeMediaObject $
        MediaObject
          { bytes = Q.fromStrict upload.bytes
          , mimeType = upload.mediaType
          , sourceName = Just upload.name
          }
      case storedRef of
        Nothing ->
          pure (RPC.errorResponse (RPC.requestId request) "internal_error" "Media storage did not return a media ref")
        Just mediaRef ->
          case parseMediaRef mediaRef of
            Nothing ->
              pure (RPC.errorResponse (RPC.requestId request) "internal_error" "Media storage did not return a media ref")
            Just fileId ->
              Media.mediaFileInfo fileId >>= \case
                Nothing ->
                  pure (RPC.errorResponse (RPC.requestId request) "internal_error" "Stored media file could not be loaded")
                Just media -> do
                  url <- Media.publicMediaRef mediaRef
                  pure $
                    RPC.successResponse (RPC.requestId request) $
                      attachmentResponse upload media url

dispatchChatSend
  :: (Concurrent :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => State.RpcState
  -> RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchChatSend rpcState request =
  case AesonTypes.parseEither parseChatSendParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right chatSend -> do
      message <- State.enqueueChatMessage rpcState chatSend
      case message of
        Left err ->
          pure (RPC.errorResponse (RPC.requestId request) "invalid_params" err)
        Right Nothing ->
          pure (RPC.errorResponse (RPC.requestId request) "not_found" "Session not found")
        Right (Just IncomingMessage{messageId}) ->
          pure $
            RPC.successResponse (RPC.requestId request) $
              Aeson.object
                [ "sessionId" Aeson..= rpcSessionIdText chatSend.sessionId
                , "messageId" Aeson..= messageId
                ]

dispatchMediaStats
  :: Media.Media :> es
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchMediaStats request = do
  case AesonTypes.parseEither parseMediaStatsParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right params ->
      do
        stats <- Media.mediaCacheStats
        files <- Media.listMediaFiles
        pure $
          RPC.successResponse (RPC.requestId request) $
            Aeson.object
              [ "stats" Aeson..= stats
              , "files" Aeson..= take params.limit files
              ]

dispatchMediaResolveSource
  :: Media.Media :> es
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchMediaResolveSource request =
  case AesonTypes.parseEither parseMediaSourceParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right sourceRef -> do
      mediaRef <- Media.mediaRefForSource sourceRef
      pure $
        RPC.successResponse (RPC.requestId request) $
          Aeson.object
            [ "sourceRef" Aeson..= sourceRef
            , "mediaId" Aeson..= mediaRef
            , "fileId" Aeson..= (mediaRef >>= parseMediaRef)
            ]

dispatchMediaGet
  :: Media.Media :> es
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchMediaGet request =
  case AesonTypes.parseEither parseMediaIdParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right fileId -> do
      Media.mediaCacheEntry fileId >>= \case
        Nothing ->
          pure (RPC.errorResponse (RPC.requestId request) "not_found" [i|Media file not found: #{fileId}|])
        Just entry -> do
          let mediaRef = entry.file.ref
          publicUrl <- Media.publicMediaRef mediaRef
          localPath <- Media.localMediaPath mediaRef
          pure $
            RPC.successResponse (RPC.requestId request) $
              mediaEntryResponse entry publicUrl localPath

dispatchMediaDelete
  :: Media.Media :> es
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchMediaDelete request =
  case AesonTypes.parseEither parseMediaIdParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right fileId -> do
      deleted <- Media.deleteMediaFile fileId
      pure $
        RPC.successResponse (RPC.requestId request) $
          Aeson.object
            [ "fileId" Aeson..= fileId
            , "mediaId" Aeson..= ("media:" <> fileId)
            , "deleted" Aeson..= deleted
            ]

dispatchMediaGc
  :: (Storage.Storage :> es, Media.Media :> es)
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchMediaGc request =
  case AesonTypes.parseEither parseMediaGcParams (RPC.requestParams request) of
    Left err ->
      pure (RPC.errorResponse (RPC.requestId request) "invalid_params" (Text.pack err))
    Right params -> do
      retained <- Set.fromList <$> RpcStorage.referencedMediaFileIds
      deleted <- Media.gcMediaCache params.maxAgeSeconds retained
      pure $
        RPC.successResponse (RPC.requestId request) $
          Aeson.object
            [ "deleted" Aeson..= deleted
            , "retainedReferencedFiles" Aeson..= Set.size retained
            ]

dispatchManagerRequest
  :: (Concurrency.Concurrency :> es, Resource.Resource :> es)
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchManagerRequest request =
  case RPC.requestMethod request of
    "concurrency.list" -> dispatchConcurrencyList request
    "concurrency.lookup" -> dispatchConcurrencyLookup request
    "concurrency.cancel" -> dispatchConcurrencyCancel request
    "concurrency.await" -> dispatchConcurrencyAwait request
    "resource.list" -> dispatchResourceList request
    "resource.detail" -> dispatchResourceDetail request
    "resource.destroy" -> dispatchResourceDestroy request
    "resource.rename" -> dispatchResourceRename request
    "resource.keep_alive" -> dispatchResourceLifetime "refreshed" Resource.keepAlive request
    "resource.make_permanent" -> dispatchResourceLifetime "permanent" Resource.makePermanent request
    "resource.destroy_associated" -> dispatchResourceDestroyAssociated request
    method -> pure (methodNotFound (RPC.requestId request) method)

dispatchConcurrencyList
  :: Concurrency.Concurrency :> es
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchConcurrencyList request = do
  snapshot <- Concurrency.list
  pure $ RPC.successResponse (RPC.requestId request) $
    Aeson.object ["entries" Aeson..= map concurrencyInfoValue snapshot.entries]

dispatchConcurrencyLookup
  :: Concurrency.Concurrency :> es
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchConcurrencyLookup request =
  withConcurrencyId request \workerId -> do
    entry <- Concurrency.lookup workerId
    pure $ RPC.successResponse (RPC.requestId request) $
      Aeson.object ["entry" Aeson..= fmap concurrencyInfoValue entry]

dispatchConcurrencyCancel
  :: Concurrency.Concurrency :> es
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchConcurrencyCancel request =
  withConcurrencyId request \workerId -> do
    cancelled <- Concurrency.cancel workerId
    pure $ RPC.successResponse (RPC.requestId request) $ Aeson.object
      [ "id" Aeson..= workerId.unId
      , "cancelled" Aeson..= cancelled
      ]

dispatchConcurrencyAwait
  :: Concurrency.Concurrency :> es
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchConcurrencyAwait request =
  withConcurrencyId request \workerId ->
    Concurrency.lookup workerId >>= \case
      Nothing -> pure (RPC.errorResponse (RPC.requestId request) "not_found" "Concurrency task not found.")
      Just _ -> do
        Concurrency.await (Concurrency.Handle workerId)
        pure $ RPC.successResponse (RPC.requestId request) $ Aeson.object
          [ "id" Aeson..= workerId.unId
          , "awaited" Aeson..= True
          ]

withConcurrencyId
  :: RPC.RpcRequest
  -> (Concurrency.Id -> Eff es RPC.RpcResponse)
  -> Eff es RPC.RpcResponse
withConcurrencyId request action =
  case AesonTypes.parseEither parseConcurrencyId (RPC.requestParams request) of
    Left err -> pure (invalidParams request err)
    Right workerId -> action workerId

parseConcurrencyId :: Aeson.Value -> AesonTypes.Parser Concurrency.Id
parseConcurrencyId =
  Aeson.withObject "concurrency params" \o -> do
    workerId <- o Aeson..: "id"
    when (workerId < 1) (fail "id must be positive")
    pure (Concurrency.Id workerId)

concurrencyInfoValue :: Concurrency.Info -> Aeson.Value
concurrencyInfoValue info = Aeson.object
  [ "id" Aeson..= info.id.unId
  , "label" Aeson..= info.label
  , "status" Aeson..= concurrencyStatusName info.status
  , "error" Aeson..= concurrencyStatusError info.status
  , "startedAt" Aeson..= info.startedAt
  , "finishedAt" Aeson..= info.finishedAt
  ]

concurrencyStatusName :: Concurrency.Status -> Text
concurrencyStatusName = \case
  Concurrency.Running -> "running"
  Concurrency.Completed -> "completed"
  Concurrency.Failed{} -> "failed"
  Concurrency.Cancelled -> "cancelled"

concurrencyStatusError :: Concurrency.Status -> Maybe Text
concurrencyStatusError = \case
  Concurrency.Failed err -> Just err
  _ -> Nothing

dispatchResourceList
  :: Resource.Resource :> es
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchResourceList request = do
  resources <- Resource.list rpcResourceAccess
  pure $ RPC.successResponse (RPC.requestId request) $
    Aeson.object ["resources" Aeson..= map resourceValue resources]

dispatchResourceDetail
  :: Resource.Resource :> es
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchResourceDetail request =
  withResourceId request \resourceId ->
    respondResource request (\detail -> Aeson.object ["id" Aeson..= resourceId, "detail" Aeson..= detail])
      =<< Resource.detail rpcResourceAccess resourceId

dispatchResourceDestroy
  :: Resource.Resource :> es
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchResourceDestroy request =
  withResourceId request \resourceId ->
    respondResource request (const (Aeson.object ["id" Aeson..= resourceId, "destroyed" Aeson..= True]))
      =<< Resource.destroy rpcResourceAccess resourceId

dispatchResourceRename
  :: Resource.Resource :> es
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchResourceRename request =
  case AesonTypes.parseEither parseResourceRename (RPC.requestParams request) of
    Left err -> pure (invalidParams request err)
    Right (resourceId, newId) ->
      respondResource request (\renamedId -> Aeson.object ["id" Aeson..= renamedId])
        =<< Resource.rename rpcResourceAccess resourceId newId

dispatchResourceLifetime
  :: Resource.Resource :> es
  => Text
  -> (Resource.ResourceAccess -> Resource.ResourceId -> Eff es (Either Resource.ResourceError ()))
  -> RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchResourceLifetime resultField action request =
  withResourceId request \resourceId ->
    respondResource request (const (Aeson.object ["id" Aeson..= resourceId, AesonKey.fromText resultField Aeson..= True]))
      =<< action rpcResourceAccess resourceId

dispatchResourceDestroyAssociated
  :: Resource.Resource :> es
  => RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchResourceDestroyAssociated request =
  withConcurrencyId request \workerId -> do
    results <- Resource.destroyAssociated (Concurrency.Handle workerId)
    pure $ RPC.successResponse (RPC.requestId request) $ Aeson.object
      [ "id" Aeson..= workerId.unId
      , "results" Aeson..= map resourceOperationValue results
      ]

withResourceId
  :: RPC.RpcRequest
  -> (Resource.ResourceId -> Eff es RPC.RpcResponse)
  -> Eff es RPC.RpcResponse
withResourceId request action =
  case AesonTypes.parseEither parseResourceId (RPC.requestParams request) of
    Left err -> pure (invalidParams request err)
    Right resourceId -> action resourceId

parseResourceId :: Aeson.Value -> AesonTypes.Parser Resource.ResourceId
parseResourceId =
  Aeson.withObject "resource params" \o ->
    o Aeson..: "id" >>= nonEmptyText "id"

parseResourceRename :: Aeson.Value -> AesonTypes.Parser (Resource.ResourceId, Resource.ResourceId)
parseResourceRename =
  Aeson.withObject "resource.rename params" \o ->
    (,)
      <$> (o Aeson..: "id" >>= nonEmptyText "id")
      <*> ((o Aeson..: "newId" <|> o Aeson..: "new_id") >>= nonEmptyText "newId")

nonEmptyText :: String -> Text -> AesonTypes.Parser Text
nonEmptyText label value
  | Text.null clean = fail (label <> " must be non-empty")
  | otherwise = pure clean
  where
    clean = Text.strip value

resourceValue :: Resource.SomeResourceObject -> Aeson.Value
resourceValue resource = Aeson.object
  [ "id" Aeson..= resource.resourceId
  , "type" Aeson..= resource.resourceType
  , "sessionId" Aeson..= resource.sessionId
  , "description" Aeson..= resource.description
  , "probe" Aeson..= either
      (\err -> Aeson.object ["ok" Aeson..= False, "error" Aeson..= err])
      (\result -> Aeson.object ["ok" Aeson..= True, "result" Aeson..= result])
      resource.probeResult
  , "remainingLifeMinutes" Aeson..= resource.remainingLifeMinutes
  ]

respondResource
  :: RPC.RpcRequest
  -> (a -> Aeson.Value)
  -> Either Resource.ResourceError a
  -> Eff es RPC.RpcResponse
respondResource request success =
  pure . either
    (\err -> RPC.errorResponse (RPC.requestId request) (resourceErrorCode err) (resourceErrorMessage err))
    (RPC.successResponse (RPC.requestId request) . success)

resourceErrorCode :: Resource.ResourceError -> Text
resourceErrorCode = \case
  Resource.ResourceNotFoundOrNotOwned -> "not_found"
  Resource.InvalidResourceName -> "invalid_params"
  Resource.ResourceNameAlreadyExists -> "already_exists"
  Resource.ResourceUnavailable -> "unavailable"
  _ -> "resource_error"

resourceErrorMessage :: Resource.ResourceError -> Text
resourceErrorMessage = \case
  Resource.MissingResourceIdentity -> "Resource identity is missing."
  Resource.ResourceNotFoundOrNotOwned -> "Resource not found or not owned."
  Resource.ResourceTypeMismatch -> "Resource has the wrong type."
  Resource.ResourceUnavailable -> "Resource is currently unavailable."
  Resource.InvalidResourceName -> "Invalid resource name."
  Resource.ResourceNameAlreadyExists -> "Resource name already exists."
  Resource.ResourceCreationFailed err -> err
  Resource.ResourceRenameFailed err -> err
  Resource.ResourceLifetimeUpdateFailed err -> err
  Resource.ResourceCleanupFailed err -> err

resourceOperationValue :: Either Resource.ResourceError () -> Aeson.Value
resourceOperationValue = \case
  Right () -> Aeson.object ["ok" Aeson..= True]
  Left err -> Aeson.object
    [ "ok" Aeson..= False
    , "code" Aeson..= resourceErrorCode err
    , "error" Aeson..= resourceErrorMessage err
    ]

rpcResourceAccess :: Resource.ResourceAccess
rpcResourceAccess = Resource.ResourceAccess
  { owner = Resource.ResourceOwner PlatformRPC "rpc" "rpc-user"
  , superuser = True
  }

invalidParams :: RPC.RpcRequest -> String -> RPC.RpcResponse
invalidParams request =
  RPC.errorResponse (RPC.requestId request) "invalid_params" . toText

dispatchAudit
  :: RpcServerCallbacks es
  -> RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchAudit callbacks request =
  callbacks.auditMethod request >>= \case
    Nothing ->
      pure (methodNotFound (RPC.requestId request) (RPC.requestMethod request))
    Just (Left err) ->
      pure (JSONRPC.ErrorMessage (JSONRPC.JSONRPCError JSONRPC.rPC_VERSION (RPC.requestId request) err))
    Just (Right value) ->
      pure (RPC.successResponse (RPC.requestId request) value)

dispatchManager
  :: RpcServerCallbacks es
  -> RPC.RpcRequest
  -> Eff es RPC.RpcResponse
dispatchManager callbacks request =
  callbacks.managerMethod request >>= \case
    Nothing -> pure (methodNotFound (RPC.requestId request) (RPC.requestMethod request))
    Just response -> pure response

parseOpenSessionParams :: Aeson.Value -> AesonTypes.Parser (Maybe Text)
parseOpenSessionParams =
  Aeson.withObject "chat.open_session params" \o ->
    o Aeson..:? "label"

parseChatSendParams :: Aeson.Value -> AesonTypes.Parser State.RpcChatSend
parseChatSendParams =
  Aeson.withObject "chat.send params" \o -> do
    sessionText <- o Aeson..: "sessionId" <|> o Aeson..: "session_id"
    text <- o Aeson..: "text"
    imageUrls <-
      o Aeson..:? "imageUrls" >>= \case
        Just value -> pure value
        Nothing -> fromMaybe [] <$> o Aeson..:? "image_urls"
    replyToMessageId <-
      o Aeson..:? "replyToMessageId" >>= \case
        Just value -> pure (Just value)
        Nothing -> o Aeson..:? "reply_to_message_id"
    attachments <- fromMaybe [] <$> o Aeson..:? "attachments"
    pure State.RpcChatSend
      { sessionId = Session.SessionId sessionText
      , text
      , imageUrls
      , attachments
      , replyToMessageId
      }

parseAttachmentUploadParams :: Int -> Aeson.Value -> AesonTypes.Parser RpcAttachmentUpload
parseAttachmentUploadParams maxBytes =
  Aeson.withObject "chat.upload_attachment params" \o -> do
    name <- fromMaybe "attachment" <$> o Aeson..:? "name"
    mediaType <-
      o Aeson..:? "mediaType" >>= \case
        Just value -> pure value
        Nothing -> fromMaybe "application/octet-stream" <$> o Aeson..:? "media_type"
    let cleanType = cleanMediaType mediaType
    kind <- fromMaybe (kindFromMediaType cleanType) <$> o Aeson..:? "kind"
    expectedSize <- o Aeson..:? "size"
    encodedText <- o Aeson..: "data"
    traverse_ (\size -> when (size > maxBytes) (fail "attachment size exceeds configured limit")) expectedSize
    when (Text.length encodedText > maxBase64Length maxBytes) $
      fail "encoded attachment exceeds configured limit"
    bytes <-
      case Base64.decode (TextEncoding.encodeUtf8 encodedText) of
        Left err -> fail err
        Right decoded -> pure decoded
    when (ByteString.length bytes > maxBytes) $
      fail "attachment size exceeds configured limit"
    traverse_ (\size -> when (size /= ByteString.length bytes) (fail "size does not match decoded attachment bytes")) expectedSize
    pure RpcAttachmentUpload{name, mediaType = cleanType, kind, bytes}

maxBase64Length :: Int -> Int
maxBase64Length maxBytes =
  ((maxBytes + 2) `div` 3) * 4

parseSessionIdParams :: Aeson.Value -> AesonTypes.Parser State.RpcSessionId
parseSessionIdParams =
  Aeson.withObject "session params" \o ->
    Session.SessionId <$> (o Aeson..: "sessionId" <|> o Aeson..: "session_id")

parseForkParams :: Aeson.Value -> AesonTypes.Parser (State.RpcSessionId, MessageId, Maybe Text)
parseForkParams =
  Aeson.withObject "chat.fork params" \o -> do
    sessionId <- Session.SessionId <$> (o Aeson..: "sessionId" <|> o Aeson..: "session_id")
    messageId <- o Aeson..: "messageId" <|> o Aeson..: "message_id"
    label <- o Aeson..:? "label"
    pure (sessionId, messageId, label)

parseRenameSessionParams :: Aeson.Value -> AesonTypes.Parser (State.RpcSessionId, Text)
parseRenameSessionParams =
  Aeson.withObject "chat.rename_session params" \o -> do
    sessionId <- Session.SessionId <$> (o Aeson..: "sessionId" <|> o Aeson..: "session_id")
    label <- o Aeson..: "label"
    pure (sessionId, label)

parseMediaStatsParams :: Aeson.Value -> AesonTypes.Parser MediaStatsParams
parseMediaStatsParams = \case
  Aeson.Null ->
    pure MediaStatsParams{limit = 50}
  value ->
    Aeson.withObject "media.stats params" parse value
  where
    parse o = do
      limit <- fromMaybe 50 <$> o Aeson..:? "limit"
      when (limit < 0) $
        fail "limit must be non-negative"
      pure MediaStatsParams{limit}

parseMediaSourceParams :: Aeson.Value -> AesonTypes.Parser Text
parseMediaSourceParams =
  Aeson.withObject "media.resolve_source params" \o -> do
    sourceRef <-
      o Aeson..:? "sourceRef" >>= \case
        Just value -> pure value
        Nothing ->
          o Aeson..:? "source_ref" >>= \case
            Just value -> pure value
            Nothing -> o Aeson..: "source"
    let clean = Text.strip sourceRef
    when (Text.null clean) $
      fail "sourceRef must be non-empty"
    pure clean

parseMediaIdParams :: Aeson.Value -> AesonTypes.Parser Text
parseMediaIdParams =
  Aeson.withObject "media id params" \o -> do
    ref <-
      o Aeson..:? "mediaId" >>= \case
        Just value -> pure value
        Nothing ->
          o Aeson..:? "media_id" >>= \case
            Just value -> pure value
            Nothing ->
              o Aeson..:? "fileId" >>= \case
                Just value -> pure value
                Nothing -> o Aeson..: "file_id"
    case parseMediaIdOrFileId ref of
      Nothing ->
        fail "mediaId must be media:<file_id> or a non-empty fileId"
      Just fileId ->
        pure fileId

parseMediaGcParams :: Aeson.Value -> AesonTypes.Parser MediaGcParams
parseMediaGcParams = \case
  Aeson.Null ->
    pure MediaGcParams{maxAgeSeconds = 0}
  value ->
    Aeson.withObject "media.gc params" parse value
  where
    parse o = do
      maxAgeSeconds <-
        o Aeson..:? "maxAgeSeconds" >>= \case
          Just value -> pure value
          Nothing -> fromMaybe 0 <$> o Aeson..:? "max_age_seconds"
      when (maxAgeSeconds < 0) $
        fail "maxAgeSeconds must be non-negative"
      pure MediaGcParams{maxAgeSeconds}

methodNotFound :: RPC.RequestId -> Text -> RPC.RpcResponse
methodNotFound requestId method =
  RPC.errorResponse requestId "method_not_found" [i|Unknown RPC method: #{method}|]

notificationToRequest :: RPC.RpcNotification -> RPC.RpcRequest
notificationToRequest notification_ =
  JSONRPC.JSONRPCRequest JSONRPC.rPC_VERSION (JSONRPC.RequestId Aeson.Null) notification_.method notification_.params

rpcSessionIdText :: State.RpcSessionId -> Text
rpcSessionIdText =
  State.unRpcSessionId

requestIsAuthorized :: Config.Config -> WS.RequestHead -> Bool
requestIsAuthorized cfg request =
  authorizationBearer request == Just expectedToken
  where
    Config.Config{token} = cfg
    expectedToken = TextEncoding.encodeUtf8 token

requestIsRpcPath :: WS.RequestHead -> Bool
requestIsRpcPath request =
  path == "/rpc"
  where
    (path, _) = ByteString.break (== questionMark) request.requestPath

httpApp :: (forall a. Eff es a -> IO a) -> Config.Config -> Wai.Application
httpApp _runInIO _cfg request respond =
  case (Wai.requestMethod request, Wai.pathInfo request) of
    ("GET", _) ->
      respond $
        textResponse Http.status404 "not found"
    ("HEAD", _) ->
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

attachmentResponse :: RpcAttachmentUpload -> Media.MediaFileInfo -> Text -> Aeson.Value
attachmentResponse upload media url =
  let mediaRef = media.ref
  in
  Aeson.object
    [ "id" Aeson..= mediaRef
    , "attachmentId" Aeson..= mediaRef
    , "mediaRef" Aeson..= mediaRef
    , "fileId" Aeson..= media.fileId
    , "name" Aeson..= upload.name
    , "mediaType" Aeson..= media.mimeType
    , "media_type" Aeson..= media.mimeType
    , "kind" Aeson..= upload.kind
    , "size" Aeson..= media.size
    , "url" Aeson..= url
    ]

kindFromMediaType :: Text -> Text
kindFromMediaType mediaType
  | "image/" `Text.isPrefixOf` media = "image"
  | "audio/" `Text.isPrefixOf` media = "audio"
  | otherwise = "file"
  where
    media = Text.toLower mediaType

cleanMediaType :: Text -> Text
cleanMediaType value =
  case Text.strip value of
    stripped
      | validMediaType stripped -> stripped
      | otherwise -> "application/octet-stream"

validMediaType :: Text -> Bool
validMediaType value =
  case Text.splitOn "/" value of
    [mainType, subtype] ->
      validToken mainType && validToken subtype
    _ ->
      False

validToken :: Text -> Bool
validToken value =
  not (Text.null value) && Text.all validTokenChar value

validTokenChar :: Char -> Bool
validTokenChar char =
  (char >= 'a' && char <= 'z')
    || (char >= 'A' && char <= 'Z')
    || (char >= '0' && char <= '9')
    || char `elem` ("!#$&^_.+-" :: String)

defaultDispatchConfig :: Config.Config
defaultDispatchConfig =
  Config.toRuntimeConfig Config.defaultFileConfig

defaultUploadMaxBytes :: Int
defaultUploadMaxBytes =
  25 * 1024 * 1024

parseMediaRef :: Text -> Maybe Text
parseMediaRef ref = do
  fileId <- Text.stripPrefix "media:" (Text.strip ref)
  guard (not (Text.null fileId))
  pure fileId

parseMediaIdOrFileId :: Text -> Maybe Text
parseMediaIdOrFileId ref =
  case parseMediaRef ref of
    Just fileId ->
      Just fileId
    Nothing ->
      let fileId = Text.strip ref
      in if Text.null fileId then Nothing else Just fileId

mediaEntryResponse :: Media.MediaCacheEntry -> Text -> Maybe FilePath -> Aeson.Value
mediaEntryResponse entry publicUrl localPath =
  Aeson.object
    [ "mediaId" Aeson..= entry.file.ref
    , "fileId" Aeson..= entry.file.fileId
    , "file" Aeson..= entry.file
    , "sourceRefs" Aeson..= entry.sourceRefs
    , "platformRefs" Aeson..= entry.platformRefs
    , "publicUrl" Aeson..= publicUrl
    , "localPath" Aeson..= localPath
    ]
