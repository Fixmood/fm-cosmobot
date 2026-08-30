module Main (main) where

import Bot.Prelude
import Bot.Chat.Driver.Types
import qualified Bot.Chat.Driver.RPC as RPCDriver
import qualified Bot.Concurrency.Manager as ConcurrencyManager
import Bot.Core.Message
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.HTTP as EffectHTTP
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Resource as Resource
import qualified Bot.Effect.Storage as Storage
import qualified Bot.HTTP as BotHTTP
import qualified Bot.Media.Config as MediaConfig
import qualified Bot.Media.Interpreter as MediaInterpreter
import qualified Bot.Media.Object as MediaObject
import qualified Bot.RPC.Config as RPCConfig
import qualified Bot.JSONRPC as JSONRPC
import qualified Bot.RPC.Server as RPCServer
import qualified Bot.RPC.State as RPC
import qualified Bot.Resource as ResourceManager
import qualified Bot.Storage.SQLite as StorageSQLite
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Unique (hashUnique, newUnique)
import Effectful.FileSystem (runFileSystem)
import qualified Effectful.Concurrent.MVar as MVar
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import Effectful.Process (Process, runProcess)
import qualified Effectful.Timeout as EffectfulTimeout
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as Http
import qualified JSONRPC as WireJSONRPC
import qualified Network.Socket as Socket
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as S
import System.FilePath ((</>), takeDirectory, takeExtension)
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit
import qualified Toml
import Toml.Schema

newtype TestRpcException = TestRpcException Text
  deriving (Show)

instance Exception TestRpcException

main :: IO ()
main =
  defaultMain $
    testGroup "rpc"
      [ testCase "request params default to empty object" testRequestParamsDefaultToEmptyObject
      , testCase "enabled config requires token" testEnabledConfigRequiresToken
      , testCase "chat.open_session returns generated session id" testOpenSessionReturnsGeneratedSessionId
      , testCase "chat.send constructs PlatformRPC incoming message" testChatSendConstructsIncomingMessage
      , testCase "chat.send rejects missing sessions without persisting orphan messages" testChatSendRejectsMissingSession
      , testCase "chat.send broadcasts user chat notification" testChatSendBroadcastsNotification
      , testCase "client notification queue overflow disconnects slow client" testClientNotificationQueueOverflowDisconnects
      , testCase "sync request exception returns JSON-RPC error" testSyncRequestExceptionReturnsJsonRpcError
      , testCase "resource and concurrency manager RPC methods" testManagerRpcMethods
      , testCase "media upload, send, history, and stats" testAttachmentLifecycle
      , testCase "media cache can resolve, inspect, and delete cached media" testMediaCacheResolveInspectDelete
      , testCase "media cache sniffs streamed image content" testMediaCacheSniffsStreamedImageContent
      , testCase "media cache uses the JPEG extension" testMediaCacheUsesJpegExtension
      , testCase "remote media MIME is probed with range GET" testRemoteMediaMimeUsesRangeGetProbe
      , testCase "chat sessions and messages persist across RPC state restart" testChatSessionsPersistAcrossRestart
      , testCase "rpc driver persists assistant replies and edited stream text" testRpcDriverPersistsAssistantRepliesAndEdits
      , testCase "rpc driver stores local image replies as attachments" testRpcDriverStoresLocalImageRepliesAsAttachments
      , testCase "chat.fork stores immutable parent link and inherited history" testChatForkStoresParentLink
      , testCase "chat.rename_session and chat.delete_session update durable storage" testRenameAndDeleteSession
      , testCase "chat.delete_session cascades fork descendants" testDeleteSessionCascadesForkDescendants
      , testCase "websocket server authenticates and handles JSON-RPC requests" testWebSocketServerAuthenticatesAndHandlesRequests
      , testCase "HTTP server rejects non-RPC paths" testHttpServerRejectsNonRpcPaths
      ]

testRequestParamsDefaultToEmptyObject :: IO ()
testRequestParamsDefaultToEmptyObject = do
  let encoded = "{\"jsonrpc\":\"2.0\",\"id\":\"1\",\"method\":\"audit.recent\"}"
  request <- either assertFailure pure (Aeson.eitherDecodeStrict' encoded :: Either String JSONRPC.RpcRequest)
  request.jsonrpc @?= "2.0"
  request.id @?= WireJSONRPC.RequestId (Aeson.String "1")
  request.method @?= "audit.recent"
  request.params @?= Aeson.Null

testEnabledConfigRequiresToken :: IO ()
testEnabledConfigRequiresToken =
  case Toml.decode "[rpc]\nenabled = true\n" of
    Toml.Failure errors ->
      assertBool
        "expected rpc.token validation failure"
        ("rpc.token must be non-empty" `Text.isInfixOf` Text.unlines (map toText errors))
    Toml.Success _warnings (config_ :: RpcClientConfig) ->
      assertFailure [i|expected parse failure, got #{show config_ :: String}|]

testOpenSessionReturnsGeneratedSessionId :: IO ()
testOpenSessionReturnsGeneratedSessionId = do
  response <- runRpcStorage ":memory:" do
    rpcState <- RPC.newRpcState
    RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
      rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("local" :: Text)])
  response @?=
    responseResult
      ( Aeson.object
          [ "sessionId" Aeson..= ("local-1" :: Text)
          , "session" Aeson..= sessionValue "local-1" (Just "local") Nothing Nothing
          ]
      )

testChatSendConstructsIncomingMessage :: IO ()
testChatSendConstructsIncomingMessage = do
  (response, incoming) <- runRpcStorage ":memory:" do
    rpcState <- RPC.newRpcState
    _open <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
      rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("local" :: Text)])
    response <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
      rpcRequest "chat.send" $
        Aeson.object
          [ "session_id" Aeson..= ("local-1" :: Text)
          , "text" Aeson..= ("hello" :: Text)
          , "image_urls" Aeson..= ["https://example.test/image.png" :: Text]
          , "reply_to_message_id" Aeson..= ("session-0" :: Text)
          ]
    incoming <- fromMaybe (error "expected one incoming RPC message") <$> S.head_ (RPC.incomingMessages rpcState)
    pure (response, incoming)

  response @?= responseResult (Aeson.object ["sessionId" Aeson..= ("local-1" :: Text), "messageId" Aeson..= ("session-1" :: Text)])
  incoming.platform @?= PlatformRPC
  incoming.kind @?= ChatPrivate
  incoming.chatAliases @?= ["local-1"]
  incoming.senderId @?= Just "rpc-user"
  incoming.text @?= "hello"
  incoming.imageUrls @?= ["https://example.test/image.png"]
  incoming.replyToMessageId @?= Just "session-0"
  incoming.digest.senderIsAllowed @?= True
  incoming.digest.senderIsSuperuser @?= True
  incoming.digest.mentionsBot @?= True

testChatSendRejectsMissingSession :: IO ()
testChatSendRejectsMissingSession =
  withSQLiteTempPath "rpc-missing-send" \path -> do
    (sendResponse, openResponse, historyResponse) <- runRpcStorage path do
      rpcState <- RPC.newRpcState
      sendResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" $
          Aeson.object
            [ "sessionId" Aeson..= ("missing-1" :: Text)
            , "text" Aeson..= ("orphan" :: Text)
            ]
      openResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("missing" :: Text)])
      historyResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.history" (Aeson.object ["sessionId" Aeson..= ("missing-1" :: Text)])
      pure (sendResponse, openResponse, historyResponse)

    sendResponse @?= responseError "not_found" "Session not found"
    openResponse @?=
      responseResult
        ( Aeson.object
            [ "sessionId" Aeson..= ("missing-1" :: Text)
            , "session" Aeson..= sessionValue "missing-1" (Just "missing") Nothing Nothing
            ]
        )
    historyResponse @?=
      responseResult
        ( Aeson.object
            [ "sessionId" Aeson..= ("missing-1" :: Text)
            , "messages" Aeson..= ([] :: [Aeson.Value])
            ]
        )

testChatSendBroadcastsNotification :: IO ()
testChatSendBroadcastsNotification = do
  notificationValue <- runRpcStorage ":memory:" do
    rpcState <- RPC.newRpcState
    (_clientId, queue) <- RPC.registerClient rpcState
    _open <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
      rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("local" :: Text)])
    _response <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
      rpcRequest "chat.send" $
        Aeson.object
          [ "sessionId" Aeson..= ("local-1" :: Text)
          , "text" Aeson..= ("hello" :: Text)
          ]
    RPC.readClient queue >>= \case
      RPC.RpcClientSend value ->
        pure value
      RPC.RpcClientDisconnect reason ->
        liftIO (assertFailure [i|unexpected RPC client disconnect: #{reason}|])

  notification <- parseJson notificationValue :: IO JSONRPC.RpcNotification
  notification.method @?= "chat.message"
  notification.params @?=
    Aeson.object
      [ "sessionId" Aeson..= ("local-1" :: Text)
      , "messageId" Aeson..= ("session-1" :: Text)
      , "sender" Aeson..= ("user" :: Text)
      , "text" Aeson..= ("hello" :: Text)
      , "imageUrls" Aeson..= ([] :: [Text])
      , "attachments" Aeson..= ([] :: [Aeson.Value])
      , "replyToMessageId" Aeson..= (Nothing :: Maybe Text)
      , "parentMessageId" Aeson..= (Nothing :: Maybe Text)
      ]

testClientNotificationQueueOverflowDisconnects :: IO ()
testClientNotificationQueueOverflowDisconnects = do
  event <- runRpcStorage ":memory:" do
    rpcState <- RPC.newRpcState
    (_clientId, queue) <- RPC.registerClient rpcState
    replicateM_ 257 $
      RPC.broadcast rpcState (Aeson.object ["event" Aeson..= ("notification" :: Text)])
    RPC.readClient queue

  case event of
    RPC.RpcClientDisconnect reason ->
      reason @?= "RPC notification queue overflow"
    RPC.RpcClientSend value ->
      assertFailure [i|expected queue overflow disconnect, got #{Aeson.encode value}|]

testSyncRequestExceptionReturnsJsonRpcError :: IO ()
testSyncRequestExceptionReturnsJsonRpcError = do
  response <- runRpcStorage ":memory:" do
    rpcState <- RPC.newRpcState
    let callbacks =
          RPCServer.noRpcServerCallbacks
            { RPCServer.auditMethod = \_ ->
                throwIO (TestRpcException "audit exploded")
            }
    RPCServer.dispatchRpcRequest rpcState callbacks $
      rpcRequest "audit.recent" Aeson.Null

  case response of
    WireJSONRPC.ErrorMessage err -> do
      err.id @?= WireJSONRPC.RequestId (Aeson.String "test-1")
      WireJSONRPC.code err.error @?= WireJSONRPC.iNTERNAL_ERROR
      WireJSONRPC.message err.error @?= "RPC request failed: TestRpcException \"audit exploded\""
    _ ->
      assertFailure [i|expected JSON-RPC error response, got #{Aeson.encode response}|]

testManagerRpcMethods :: IO ()
testManagerRpcMethods = runRpcManager do
  rpcState <- RPC.newRpcState
  worker <- Concurrency.fork "rpc-test-worker" never
  lookupResponse <- dispatch rpcState "concurrency.lookup" (Aeson.object ["id" Aeson..= worker.handleId.unId])
  cancelResponse <- dispatch rpcState "concurrency.cancel" (Aeson.object ["id" Aeson..= worker.handleId.unId])
  awaitResponse <- dispatch rpcState "concurrency.await" (Aeson.object ["id" Aeson..= worker.handleId.unId])
  associatedResponse <- dispatch rpcState "resource.destroy_associated" (Aeson.object ["id" Aeson..= worker.handleId.unId])
  resourceListResponse <- dispatch rpcState "resource.list" Aeson.Null
  missingResourceResponse <- dispatch rpcState "resource.detail" (Aeson.object ["id" Aeson..= ("missing" :: Text)])
  liftIO do
    (responseField lookupResponse "entry" >>= responseObjectText "label") @?= Just "rpc-test-worker"
    cancelResponse @?= responseResult (Aeson.object ["id" Aeson..= worker.handleId.unId, "cancelled" Aeson..= True])
    awaitResponse @?= responseResult (Aeson.object ["id" Aeson..= worker.handleId.unId, "awaited" Aeson..= True])
    associatedResponse @?= responseResult (Aeson.object ["id" Aeson..= worker.handleId.unId, "results" Aeson..= ([] :: [Aeson.Value])])
    resourceListResponse @?= responseResult (Aeson.object ["resources" Aeson..= ([] :: [Aeson.Value])])
    responseErrorCode missingResourceResponse @?= Just "not_found"
  where
    dispatch rpcState method params =
      RPCServer.dispatchRpcRequest rpcState (RPCServer.withManagerRpcCallbacks RPCServer.noRpcServerCallbacks) (rpcRequest method params)

    responseObjectText :: Text -> Aeson.Value -> Maybe Text
    responseObjectText field value =
      AesonTypes.parseMaybe (Aeson.withObject "response object" (Aeson..: AesonKey.fromText field)) value

    never = threadDelay maxBound

testAttachmentLifecycle :: IO ()
testAttachmentLifecycle =
  withSQLiteTempPath "rpc-attachments" \path -> do
    let cfg :: RPCConfig.Config
        cfg = RPCConfig.Config
          { enabled = False
          , host = "127.0.0.1"
          , port = 38765
          , token = ""
          }
    (uploadResponse, imageUploadResponse, unsafeMediaResponse, oversizedResponse, sendResponse, historyResponse, incoming, mediaStatsResponse) <- runRpcStorage path do
      rpcState <- RPC.newRpcState
      uploadResponse <- RPCServer.dispatchRpcRequestWithConfig rpcState cfg RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.upload_attachment" $
          Aeson.object
            [ "name" Aeson..= ("notes.txt" :: Text)
            , "mediaType" Aeson..= ("text/plain" :: Text)
            , "kind" Aeson..= ("file" :: Text)
            , "size" Aeson..= (5 :: Int)
            , "data" Aeson..= ("aGVsbG8=" :: Text)
            ]
      let attachment = responseAttachmentUnsafe uploadResponse
      imageUploadResponse <- RPCServer.dispatchRpcRequestWithConfig rpcState cfg RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.upload_attachment" $
          Aeson.object
            [ "name" Aeson..= ("pixel.png" :: Text)
            , "mediaType" Aeson..= ("image/png" :: Text)
            , "kind" Aeson..= ("image" :: Text)
            , "size" Aeson..= (1 :: Int)
            , "data" Aeson..= ("AA==" :: Text)
            ]
      let imageAttachment = responseAttachmentUnsafe imageUploadResponse
      unsafeMediaResponse <- RPCServer.dispatchRpcRequestWithConfig rpcState cfg RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.upload_attachment" $
          Aeson.object
            [ "name" Aeson..= ("unsafe.html" :: Text)
            , "mediaType" Aeson..= ("text/html\r\nx" :: Text)
            , "kind" Aeson..= ("file" :: Text)
            , "size" Aeson..= (1 :: Int)
            , "data" Aeson..= ("AQ==" :: Text)
            ]
      oversizedResponse <- RPCServer.dispatchRpcRequestWithConfig rpcState cfg RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.upload_attachment" $
          Aeson.object
            [ "name" Aeson..= ("large.bin" :: Text)
            , "mediaType" Aeson..= ("application/octet-stream" :: Text)
            , "kind" Aeson..= ("file" :: Text)
            , "size" Aeson..= (25 * 1024 * 1024 + 1 :: Int)
            , "data" Aeson..= (Text.replicate 8 "A" :: Text)
            ]
      _open <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("local" :: Text)])
      sendResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" $
          Aeson.object
            [ "sessionId" Aeson..= ("local-1" :: Text)
            , "text" Aeson..= ("see attached" :: Text)
            , "imageUrls" Aeson..= [imageAttachment.url, "https://example.test/context.png"]
            , "attachments" Aeson..= [attachment, imageAttachment]
            ]
      incoming <- fromMaybe (error "expected one incoming RPC message") <$> S.head_ (RPC.incomingMessages rpcState)
      historyResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.history" (Aeson.object ["sessionId" Aeson..= ("local-1" :: Text)])
      mediaStatsResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "media.stats" (Aeson.object ["limit" Aeson..= (10 :: Int)])
      pure (uploadResponse, imageUploadResponse, unsafeMediaResponse, oversizedResponse, sendResponse, historyResponse, incoming, mediaStatsResponse)

    attachment <- responseAttachment uploadResponse
    imageAttachment <- responseAttachment imageUploadResponse
    unsafeMediaAttachment <- responseAttachment unsafeMediaResponse
    attachment.name @?= "notes.txt"
    attachment.mediaType @?= "text/plain"
    attachment.kind @?= "file"
    imageAttachment.kind @?= "image"
    unsafeMediaAttachment.mediaType @?= "application/octet-stream"
    oversizedResponse @?= responseError "invalid_params" "Error in $: attachment size exceeds configured limit"
    sendResponse @?= responseResult (Aeson.object ["sessionId" Aeson..= ("local-1" :: Text), "messageId" Aeson..= Just ("session-1" :: Text)])
    assertBool "non-image attachment should be visible in incoming context" ("Attachments:" `Text.isInfixOf` incoming.text)
    incoming.imageUrls @?= [imageAttachment.url, "https://example.test/context.png", "data:image/png;base64,AA=="]
    assertEqual [i|history response: #{show historyResponse :: String}|] [[attachment.attachmentId, imageAttachment.attachmentId]] (responseMessageAttachments historyResponse)
    responseMediaStatsFiles mediaStatsResponse @?= 3

testMediaCacheResolveInspectDelete :: IO ()
testMediaCacheResolveInspectDelete =
  withSQLiteTempPath "rpc-media-cache" \path -> do
    (mediaRef, resolveResponse, getResponse, deleteResponse, getAfterDeleteResponse, fileExistsAfterDelete) <- runRpcStorage path do
      rpcState <- RPC.newRpcState
      let sourceRef = "telegram:file-123"
      mediaRef <- fromMaybe (error "expected stored media ref") <$> Media.storeMediaObjectFromSource sourceRef Media.MediaObject
        { Media.bytes = Q.fromStrict (TextEncoding.encodeUtf8 "hello")
        , Media.mimeType = "text/plain"
        , Media.sourceName = Just "hello.txt"
        }
      Media.storePlatformMediaRef "telegram" "chat-42" mediaRef "file-123"
      resolveResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "media.resolve_source" (Aeson.object ["sourceRef" Aeson..= sourceRef])
      getResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "media.get" (Aeson.object ["mediaId" Aeson..= mediaRef])
      let localPath = responseMediaLocalPath getResponse
      deleteResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "media.delete" (Aeson.object ["mediaId" Aeson..= mediaRef])
      getAfterDeleteResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "media.get" (Aeson.object ["mediaId" Aeson..= mediaRef])
      fileExistsAfterDelete <- maybe (pure False) FileSystem.doesFileExist localPath
      pure (mediaRef, resolveResponse, getResponse, deleteResponse, getAfterDeleteResponse, fileExistsAfterDelete)

    responseField resolveResponse "mediaId" @?= Just mediaRef
    responseField getResponse "mediaId" @?= Just mediaRef
    assertBool "public url should use configured media base" (maybe False ("https://media.example.test/cosmobot-media/" `Text.isPrefixOf`) (responseField getResponse "publicUrl"))
    assertBool "public url should keep source extension" (maybe False (".txt" `Text.isSuffixOf`) (responseField getResponse "publicUrl"))
    responseTextList getResponse "sourceRefs" @?= ["telegram:file-123"]
    responsePlatformRefs getResponse @?= [("telegram", "chat-42", "file-123")]
    assertBool "media get response should not duplicate source refs in snake_case" (not (responseHasField getResponse "source_refs"))
    assertBool "media get response should not duplicate public url in snake_case" (not (responseHasField getResponse "public_url"))
    assertBool "media get response should not duplicate local path in snake_case" (not (responseHasField getResponse "local_path"))
    responseBool deleteResponse "deleted" @?= Just True
    responseErrorCode getAfterDeleteResponse @?= Just "not_found"
    fileExistsAfterDelete @?= False

testMediaCacheSniffsStreamedImageContent :: IO ()
testMediaCacheSniffsStreamedImageContent =
  withSQLiteTempPath "rpc-media-sniff" \path -> do
    info <- runRpcStorage path do
      mediaRef <- fromMaybe (error "expected stored media ref") <$> Media.storeMediaObject Media.MediaObject
        { Media.bytes = Q.fromStrict "\x89PNG\r\n\x1a\ncontent"
        , Media.mimeType = "application/octet-stream"
        , Media.sourceName = Just "image.bin"
        }
      fromMaybe (error "expected stored media info") <$> Media.mediaFileInfoByRef mediaRef
    info.mimeType @?= "image/png"
    takeExtension info.path @?= ".png"

testMediaCacheUsesJpegExtension :: IO ()
testMediaCacheUsesJpegExtension =
  withSQLiteTempPath "rpc-media-jpeg-extension" \path -> do
    publicUrl <- runRpcStorage path do
      mediaRef <- fromMaybe (error "expected stored media ref") <$> Media.storeMediaObject Media.MediaObject
        { Media.bytes = Q.fromStrict "\xff\xd8\xff\&content"
        , Media.mimeType = "application/octet-stream"
        , Media.sourceName = Nothing
        }
      Media.publicMediaRef mediaRef
    assertBool "public URL should use .jpg rather than .jpe" (".jpg" `Text.isSuffixOf` publicUrl)

testChatSessionsPersistAcrossRestart :: IO ()
testChatSessionsPersistAcrossRestart =
  withSQLiteTempPath "rpc-persist" \path -> do
    firstResponse <- runRpcStorage path do
      rpcState <- RPC.newRpcState
      _open <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("local" :: Text)])
      RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" $
          Aeson.object
            [ "sessionId" Aeson..= ("local-1" :: Text)
            , "text" Aeson..= ("persisted" :: Text)
            ]

    firstResponse @?= responseResult (Aeson.object ["sessionId" Aeson..= ("local-1" :: Text), "messageId" Aeson..= Just ("session-1" :: Text)])

    (listResponse, historyResponse, sendResponse) <- runRpcStorage path do
      rpcState <- RPC.newRpcState
      listResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.list_sessions" Aeson.Null
      historyResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.history" (Aeson.object ["sessionId" Aeson..= ("local-1" :: Text)])
      sendResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" $
          Aeson.object
            [ "sessionId" Aeson..= ("local-1" :: Text)
            , "text" Aeson..= ("after restart" :: Text)
            ]
      pure (listResponse, historyResponse, sendResponse)

    listResponse @?=
      responseResult
        (Aeson.object ["sessions" Aeson..= [sessionValue "local-1" (Just "local") Nothing Nothing]])
    historyResponse @?=
      responseResult
        ( Aeson.object
            [ "sessionId" Aeson..= ("local-1" :: Text)
            , "messages" Aeson..= [messageValue "local-1" "session-1" "persisted" Nothing]
            ]
        )
    sendResponse @?= responseResult (Aeson.object ["sessionId" Aeson..= ("local-1" :: Text), "messageId" Aeson..= Just ("session-2" :: Text)])

testRpcDriverPersistsAssistantRepliesAndEdits :: IO ()
testRpcDriverPersistsAssistantRepliesAndEdits =
  withSQLiteTempPath "rpc-assistant" \path -> do
    (replyId, edited) <- runRpcStorage path do
      rpcState <- RPC.newRpcState
      _open <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("local" :: Text)])
      _sent <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" $
          Aeson.object
            [ "sessionId" Aeson..= ("local-1" :: Text)
            , "text" Aeson..= ("question" :: Text)
            ]
      incoming <- fromMaybe (error "expected one incoming RPC message") <$> S.head_ (RPC.incomingMessages rpcState)
      let driver = RPCDriver.rpcChatDriver testRpcConfig rpcState
      replyId <- fromMaybe (error "expected rpc reply id") . rightToMaybe <$> sendReplyMessage driver incoming "draft answer"
      edited <- editMessage driver incoming replyId "final answer"
      pure (replyId, edited)

    replyId @?= "session-2"
    edited @?= True

    historyResponse <- runRpcStorage path do
      rpcState <- RPC.newRpcState
      RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.history" (Aeson.object ["sessionId" Aeson..= ("local-1" :: Text)])

    responseMessageSummaries historyResponse @?=
      [ ("user", "session-1", "question")
      , ("assistant", "session-2", "final answer")
      ]

testRpcDriverStoresLocalImageRepliesAsAttachments :: IO ()
testRpcDriverStoresLocalImageRepliesAsAttachments =
  withSQLiteTempPath "rpc-assistant-image" \path -> do
    historyResponse <- runRpcStorage path do
      let dir = takeDirectory path
          imagePath = dir </> "generated.webp"
          cfg :: RPCConfig.Config
          cfg = RPCConfig.Config
            { enabled = True
            , host = "127.0.0.1"
            , port = 38765
            , token = "secret"
            }
      FileSystemByteString.writeFile imagePath "fake-webp"
      rpcState <- RPC.newRpcState
      _open <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("local" :: Text)])
      _sent <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" $
          Aeson.object
            [ "sessionId" Aeson..= ("local-1" :: Text)
            , "text" Aeson..= ("make an image" :: Text)
            ]
      incoming <- fromMaybe (error "expected one incoming RPC message") <$> S.head_ (RPC.incomingMessages rpcState)
      let driver = RPCDriver.rpcChatDriver cfg rpcState
      _reply <- sendReplyMessage driver incoming ("done\n[image] file://" <> Text.pack imagePath)
      RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.history" (Aeson.object ["sessionId" Aeson..= ("local-1" :: Text)])

    responseMessageSummaries historyResponse @?=
      [ ("user", "session-1", "make an image")
      , ("assistant", "session-2", "done")
      ]
    case responseMessageAttachments historyResponse of
      [[], [attachmentId]] ->
        assertBool "expected stored image media ref" ("media:mf_" `Text.isPrefixOf` attachmentId)
      other ->
        assertFailure [i|expected one image attachment on assistant reply, got #{show other :: String}|]

testChatForkStoresParentLink :: IO ()
testChatForkStoresParentLink =
  withSQLiteTempPath "rpc-fork" \path -> do
    (forkResponse, forkHistory) <- runRpcStorage path do
      rpcState <- RPC.newRpcState
      _open <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("root" :: Text)])
      _first <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" (Aeson.object ["sessionId" Aeson..= ("root-1" :: Text), "text" Aeson..= ("first" :: Text)])
      _second <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" (Aeson.object ["sessionId" Aeson..= ("root-1" :: Text), "text" Aeson..= ("second" :: Text)])
      forkResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.fork" $
          Aeson.object
            [ "sessionId" Aeson..= ("root-1" :: Text)
            , "messageId" Aeson..= ("session-1" :: Text)
            , "label" Aeson..= ("branch" :: Text)
            ]
      _branch <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" (Aeson.object ["sessionId" Aeson..= ("branch-1" :: Text), "text" Aeson..= ("branch only" :: Text)])
      forkHistory <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.history" (Aeson.object ["sessionId" Aeson..= ("branch-1" :: Text)])
      pure (forkResponse, forkHistory)

    forkResponse @?=
      responseResult
        ( Aeson.object
            [ "sessionId" Aeson..= ("branch-1" :: Text)
            , "session" Aeson..= sessionValue "branch-1" (Just "branch") (Just "root-1") (Just "session-1")
            ]
        )
    responseMessageTexts forkHistory @?= ["first", "branch only"]

testRenameAndDeleteSession :: IO ()
testRenameAndDeleteSession =
  withSQLiteTempPath "rpc-delete" \path -> do
    (renameResponse, deleteResponse, listResponse) <- runRpcStorage path do
      rpcState <- RPC.newRpcState
      _open <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("old" :: Text)])
      _sent <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" (Aeson.object ["sessionId" Aeson..= ("old-1" :: Text), "text" Aeson..= ("gone" :: Text)])
      renameResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.rename_session" (Aeson.object ["sessionId" Aeson..= ("old-1" :: Text), "label" Aeson..= ("new" :: Text)])
      deleteResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.delete_session" (Aeson.object ["sessionId" Aeson..= ("old-1" :: Text)])
      listResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.list_sessions" Aeson.Null
      pure (renameResponse, deleteResponse, listResponse)

    responseSessionLabel renameResponse @?= Just "new"
    deleteResponse @?= responseResult (Aeson.object ["sessionId" Aeson..= ("old-1" :: Text), "deleted" Aeson..= True])
    listResponse @?= responseResult (Aeson.object ["sessions" Aeson..= ([] :: [Aeson.Value])])

testDeleteSessionCascadesForkDescendants :: IO ()
testDeleteSessionCascadesForkDescendants =
  withSQLiteTempPath "rpc-delete-fork" \path -> do
    (deleteResponse, listResponse, branchSendResponse) <- runRpcStorage path do
      rpcState <- RPC.newRpcState
      _open <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("root" :: Text)])
      _first <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" (Aeson.object ["sessionId" Aeson..= ("root-1" :: Text), "text" Aeson..= ("first" :: Text)])
      _fork <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.fork" $
          Aeson.object
            [ "sessionId" Aeson..= ("root-1" :: Text)
            , "messageId" Aeson..= ("session-1" :: Text)
            , "label" Aeson..= ("branch" :: Text)
            ]
      _branch <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" (Aeson.object ["sessionId" Aeson..= ("branch-1" :: Text), "text" Aeson..= ("branch only" :: Text)])
      deleteResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.delete_session" (Aeson.object ["sessionId" Aeson..= ("root-1" :: Text)])
      listResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.list_sessions" Aeson.Null
      branchSendResponse <- RPCServer.dispatchRpcRequest rpcState RPCServer.noRpcServerCallbacks $
        rpcRequest "chat.send" (Aeson.object ["sessionId" Aeson..= ("branch-1" :: Text), "text" Aeson..= ("after delete" :: Text)])
      pure (deleteResponse, listResponse, branchSendResponse)

    deleteResponse @?= responseResult (Aeson.object ["sessionId" Aeson..= ("root-1" :: Text), "deleted" Aeson..= True])
    listResponse @?= responseResult (Aeson.object ["sessions" Aeson..= ([] :: [Aeson.Value])])
    branchSendResponse @?= responseError "not_found" "Session not found"

testWebSocketServerAuthenticatesAndHandlesRequests :: IO ()
testWebSocketServerAuthenticatesAndHandlesRequests = do
  result <- timeout 10_000_000 $ runRpcServerTest do
    rpcState <- RPC.newRpcState
    listenSocket <- liftIO (WS.makeListenSocket "127.0.0.1" 0)
    port <- (fromIntegral :: Socket.PortNumber -> Int) <$> liftIO (Socket.socketPort listenSocket)
    let cfg = RPCConfig.Config
          { enabled = True
        , host = "127.0.0.1"
        , port
        , token = "secret"
        }
        server =
          finally
            (forever do
              (clientSocket, _) <- liftIO (Socket.accept listenSocket)
              pending <- liftIO (WS.makePendingConnection clientSocket WS.defaultConnectionOptions)
              RPCServer.rpcServerApp cfg rpcState RPCServer.noRpcServerCallbacks pending)
            (liftIO (Socket.close listenSocket))
        client = do
          unauthorized <- trySync (liftIO (WS.runClient "127.0.0.1" port "/rpc" \_ -> pure ()))
          response <- liftIO (openSessionClient port "secret")
          pure (unauthorized, response)
    raceEff server client

  case result of
    Nothing ->
      assertFailure "RPC websocket integration test timed out"
    Just (Left ()) ->
      assertFailure "RPC server exited before client completed"
    Just (Right (unauthorized, response)) -> do
      assertUnauthorizedRejected unauthorized
      response @?=
        responseResult
          ( Aeson.object
              [ "sessionId" Aeson..= ("integration-1" :: Text)
              , "session" Aeson..= sessionValue "integration-1" (Just "integration") Nothing Nothing
              ]
          )

testHttpServerRejectsNonRpcPaths :: IO ()
testHttpServerRejectsNonRpcPaths = do
  result <- timeout 2_000_000 $ runRpcServerTest do
    rpcState <- RPC.newRpcState
    listenSocket <- liftIO (WS.makeListenSocket "127.0.0.1" 0)
    port <- fromIntegral <$> liftIO (Socket.socketPort listenSocket)
    let cfg = RPCConfig.Config
          { enabled = True
          , host = "127.0.0.1"
          , port
          , token = "secret"
          }
        server =
          withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
            liftIO $
              Warp.runSettingsSocket Warp.defaultSettings listenSocket $
                RPCServer.rpcServerApplication runInIO cfg rpcState RPCServer.noRpcServerCallbacks
        client = liftIO do
          manager <- HTTP.newManager HTTP.defaultManagerSettings
          root <- httpGet manager [i|http://127.0.0.1:#{port}/|]
          mediaWithoutAuth <- httpGet manager [i|http://127.0.0.1:#{port}/media/missing|]
          mediaWithAuth <- httpGetWithBearer manager "secret" [i|http://127.0.0.1:#{port}/media/missing|]
          response <- openSessionClient port "secret"
          pure (root, mediaWithoutAuth, mediaWithAuth, response)
    raceEff server client

  case result of
    Nothing ->
      assertFailure "RPC HTTP integration test timed out"
    Just (Left ()) ->
      assertFailure "RPC HTTP server exited before client completed"
    Just (Right (root, mediaWithoutAuth, mediaWithAuth, response)) -> do
      HTTP.responseStatus root @?= Http.status404
      HTTP.responseStatus mediaWithoutAuth @?= Http.status404
      HTTP.responseStatus mediaWithAuth @?= Http.status404
      response @?=
        responseResult
          ( Aeson.object
              [ "sessionId" Aeson..= ("integration-1" :: Text)
              , "session" Aeson..= sessionValue "integration-1" (Just "integration") Nothing Nothing
              ]
          )

testRemoteMediaMimeUsesRangeGetProbe :: IO ()
testRemoteMediaMimeUsesRangeGetProbe = do
  result <- timeout 2_000_000 $ runEff $ runConcurrent do
    listenSocket <- liftIO (WS.makeListenSocket "127.0.0.1" 0)
    port <- (fromIntegral :: Socket.PortNumber -> Int) <$> liftIO (Socket.socketPort listenSocket)
    let server =
          liftIO $
            Warp.runSettingsSocket Warp.defaultSettings listenSocket remoteMediaProbeApp
        client = do
          manager <- liftIO (HTTP.newManager HTTP.defaultManagerSettings)
          MediaObject.downloadObject manager [i|http://127.0.0.1:#{port}/download?file=image|]
    raceEff server client

  case result of
    Nothing ->
      assertFailure "remote media probe test timed out"
    Just (Left ()) ->
      assertFailure "remote media probe server exited before client completed"
    Just (Right mediaObject) ->
      mediaObject.mimeType @?= "image/png"

remoteMediaProbeApp :: Wai.Application
remoteMediaProbeApp request respond =
  respond case Wai.requestMethod request of
    "GET" ->
      Wai.responseLBS Http.status206 [(Http.hContentType, "image/png")] "\x89PNG\r\n\x1a\n"
    "HEAD" ->
      Wai.responseLBS Http.status200 [(Http.hContentType, "application/json")] ""
    _ ->
      Wai.responseLBS Http.status405 [] ""

data RpcClientConfig = RpcClientConfig
  { rpc :: RPCConfig.FileConfig
  }
  deriving (Show)

instance FromValue RpcClientConfig where
  fromValue = parseTableFromValue $
    RpcClientConfig
      <$> fmap (fromMaybe RPCConfig.defaultFileConfig) (optKey "rpc")

rpcRequest :: Text -> Aeson.Value -> JSONRPC.RpcRequest
rpcRequest method params =
  JSONRPC.rpcRequest method params "test-1"

responseResult :: Aeson.Value -> JSONRPC.RpcResponse
responseResult =
  JSONRPC.successResponse (WireJSONRPC.RequestId (Aeson.String "test-1"))

responseError :: Text -> Text -> JSONRPC.RpcResponse
responseError code message =
  JSONRPC.errorResponse (WireJSONRPC.RequestId (Aeson.String "test-1")) code message

parseJson :: Aeson.FromJSON a => Aeson.Value -> IO a
parseJson value =
  case AesonTypes.parseEither Aeson.parseJSON value of
    Left err -> assertFailure err
    Right parsed -> pure parsed

responseAttachment :: JSONRPC.RpcResponse -> IO RPC.RpcChatAttachmentRef
responseAttachment = \case
  WireJSONRPC.ResponseMessage result ->
    parseJson result.result
  other ->
    assertFailure [i|expected attachment response, got #{show other :: String}|]

responseAttachmentUnsafe :: JSONRPC.RpcResponse -> RPC.RpcChatAttachmentRef
responseAttachmentUnsafe = \case
  WireJSONRPC.ResponseMessage result ->
    fromMaybe (error "expected attachment response") (AesonTypes.parseMaybe Aeson.parseJSON result.result)
  _ ->
    error "expected attachment response"

openSessionClient :: Int -> Text -> IO JSONRPC.RpcResponse
openSessionClient port token =
  WS.runClientWith "127.0.0.1" port "/rpc" WS.defaultConnectionOptions [("Authorization", "Bearer " <> TextEncoding.encodeUtf8 token)] \conn -> do
    WS.sendTextData conn $
      Aeson.encode $
        JSONRPC.rpcRequest "chat.open_session" (Aeson.object ["label" Aeson..= ("integration" :: Text)]) "test-1"
    bytes <- WS.receiveData conn :: IO ByteString
    case Aeson.eitherDecodeStrict' bytes of
      Left err -> fail [i|RPC websocket response was not JSON-RPC: #{err}|]
      Right response -> pure response

httpGet :: HTTP.Manager -> String -> IO (HTTP.Response LazyByteString.ByteString)
httpGet manager url = do
  request <- HTTP.parseRequest url
  HTTP.httpLbs
    request
      { HTTP.checkResponse = \_ _ -> pure ()
      }
    manager

httpGetWithBearer :: HTTP.Manager -> ByteString -> String -> IO (HTTP.Response LazyByteString.ByteString)
httpGetWithBearer manager token url = do
  request <- HTTP.parseRequest url
  HTTP.httpLbs
    request
      { HTTP.checkResponse = \_ _ -> pure ()
      , HTTP.requestHeaders = [("Authorization", "Bearer " <> token)]
      }
    manager

assertUnauthorizedRejected :: Either SomeException () -> IO ()
assertUnauthorizedRejected = \case
  Left err
    | Just (WS.RequestRejected _ response) <- fromException err ->
        WS.responseCode response @?= 401
    | Just (WS.MalformedResponse response _) <- fromException err ->
        WS.responseCode response @?= 401
    | otherwise ->
        assertFailure [i|expected websocket 401 rejection, got #{show err :: String}|]
  Right () ->
    assertFailure "expected unauthenticated websocket connection to fail"

runTestLog :: IOE :> es => Eff (KatipE : es) a -> Eff es a
runTestLog action = startKatipE "rpc-spec" "test" action

raceEff
  :: (Concurrent :> es, IOE :> es)
  => Eff es a
  -> Eff es b
  -> Eff es (Either a b)
raceEff left right = do
  done <- MVar.newEmptyMVar
  leftThread <- forkIO (finishRace done (Left <$> left))
  rightThread <- forkIO (finishRace done (Right <$> right))
  result <- MVar.takeMVar done
  killThread leftThread
  killThread rightThread
  either throwIO pure result

finishRace
  :: (Concurrent :> es, IOE :> es)
  => MVar.MVar (Either SomeException a)
  -> Eff es a
  -> Eff es ()
finishRace done action =
  try action >>= void . MVar.tryPutMVar done

runRpcServerTest
  :: Eff '[Media.Media, Storage.Storage, KatipE, FileSystem.FileSystem, Concurrency.Concurrency, Prim, Concurrent, IOE] a
  -> IO a
runRpcServerTest action =
  runEff $
    ( runConcurrent
    . runPrim
    . ConcurrencyManager.runConcurrencyManager
    . runFileSystem
    . runTestLog
    . StorageSQLite.runStorageSQLitePath ":memory:"
    . Media.runMediaPassthrough
    ) action

runRpcStorage :: FilePath -> Eff '[Media.Media, EffectfulTimeout.Timeout, EffectHTTP.HTTP, Storage.Storage, KatipE, Process, FileSystem.FileSystem, Concurrent, Fail, IOE] a -> IO a
runRpcStorage path action =
  runEff $
  runFailIO $
  runConcurrent $
  runFileSystem $
  runProcess $
  runTestLog $
  StorageSQLite.runStorageSQLitePath path $
  BotHTTP.runHTTP $
  EffectfulTimeout.runTimeout $
  MediaInterpreter.runMedia (testMediaConfig path) $ action

runRpcManager
  :: Eff '[Resource.Resource, Media.Media, Storage.Storage, KatipE, FileSystem.FileSystem, Concurrency.Concurrency, Prim, Concurrent, IOE] a
  -> IO a
runRpcManager action =
  runEff $
  runConcurrent $
  runPrim $
  ConcurrencyManager.runConcurrencyManager $
  runFileSystem $
  runTestLog $
  StorageSQLite.runStorageSQLitePath ":memory:" $
  Media.runMediaPassthrough $
  ResourceManager.runResourceManager action

testMediaConfig :: FilePath -> MediaConfig.Config
testMediaConfig path =
  MediaConfig.defaultConfig
    { MediaConfig.cacheDir = takeDirectory path </> "media-cache"
    , MediaConfig.publicBaseUrl = Just "https://media.example.test/cosmobot-media"
    }

withSQLiteTempPath :: String -> (FilePath -> IO a) -> IO a
withSQLiteTempPath label action =
  runEff $ runFileSystem do
    root <- FileSystem.getTemporaryDirectory
    unique <- liftIO (hashUnique <$> newUnique)
    let dir = root </> [i|cosmobot-#{label}-#{unique}|]
        path = dir </> "rpc.sqlite"
    bracket
      (FileSystem.createDirectory dir $> path)
      (\_ -> FileSystem.removeDirectoryRecursive dir)
      (liftIO . action)

sessionValue :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Aeson.Value
sessionValue sessionId label parentSessionId parentMessageId =
  Aeson.object
    [ "sessionId" Aeson..= sessionId
    , "label" Aeson..= label
    , "parentSessionId" Aeson..= parentSessionId
    , "parentMessageId" Aeson..= parentMessageId
    ]

messageValue :: Text -> Text -> Text -> Maybe Text -> Aeson.Value
messageValue sessionId messageId body parentMessageId =
  Aeson.object
    [ "sessionId" Aeson..= sessionId
    , "messageId" Aeson..= messageId
    , "sender" Aeson..= ("user" :: Text)
    , "text" Aeson..= body
    , "imageUrls" Aeson..= ([] :: [Text])
    , "attachments" Aeson..= ([] :: [Aeson.Value])
    , "replyToMessageId" Aeson..= parentMessageId
    , "parentMessageId" Aeson..= parentMessageId
    ]

responseMessageTexts :: JSONRPC.RpcResponse -> [Text]
responseMessageTexts response =
  case response of
    WireJSONRPC.ResponseMessage result ->
      fromMaybe [] do
        messages <- AesonTypes.parseMaybe (Aeson.withObject "history" (Aeson..: "messages")) result.result
        traverse (AesonTypes.parseMaybe (Aeson.withObject "message" (Aeson..: "text"))) messages
    _ ->
      []

responseMessageSummaries :: JSONRPC.RpcResponse -> [(Text, Text, Text)]
responseMessageSummaries response =
  case response of
    WireJSONRPC.ResponseMessage result ->
      fromMaybe [] do
        messages <- AesonTypes.parseMaybe (Aeson.withObject "history" (Aeson..: "messages")) result.result
        traverse messageSummary messages
    _ ->
      []
  where
    messageSummary =
      AesonTypes.parseMaybe $
        Aeson.withObject "message" \o -> do
          sender <- o Aeson..: "sender"
          messageId <- o Aeson..: "messageId"
          body <- o Aeson..: "text"
          pure (sender, messageId, body)

testRpcConfig :: RPCConfig.Config
testRpcConfig = RPCConfig.Config
  { enabled = True
  , host = "127.0.0.1"
  , port = 38765
  , token = "secret"
  }

responseMessageAttachments :: JSONRPC.RpcResponse -> [[Text]]
responseMessageAttachments response =
  case response of
    WireJSONRPC.ResponseMessage result ->
      fromMaybe [] do
        messages <- AesonTypes.parseMaybe (Aeson.withObject "history" (Aeson..: "messages")) result.result
        traverse messageAttachments messages
    _ ->
      []
  where
    messageAttachments =
      AesonTypes.parseMaybe $
        Aeson.withObject "message" \o -> do
          attachments <- o Aeson..: "attachments"
          traverse (Aeson.withObject "attachment" \attachment -> attachment Aeson..: "attachmentId" <|> attachment Aeson..: "id") attachments

responseMediaStatsFiles :: JSONRPC.RpcResponse -> Int
responseMediaStatsFiles response =
  case response of
    WireJSONRPC.ResponseMessage result ->
      fromMaybe 0 do
        stats <- AesonTypes.parseMaybe (Aeson.withObject "media stats" (Aeson..: "stats")) result.result
        AesonTypes.parseMaybe (Aeson.withObject "stats" (Aeson..: "files")) stats
    _ ->
      0

responseField :: Aeson.FromJSON a => JSONRPC.RpcResponse -> Text -> Maybe a
responseField response field =
  case response of
    WireJSONRPC.ResponseMessage result ->
      AesonTypes.parseMaybe (Aeson.withObject "response" (Aeson..: AesonKey.fromText field)) result.result
    _ ->
      Nothing

responseHasField :: JSONRPC.RpcResponse -> Text -> Bool
responseHasField response field =
  case response of
    WireJSONRPC.ResponseMessage result ->
      fromMaybe False $
        AesonTypes.parseMaybe (Aeson.withObject "response" (pure . AesonKeyMap.member (AesonKey.fromText field))) result.result
    _ ->
      False

responseBool :: JSONRPC.RpcResponse -> Text -> Maybe Bool
responseBool =
  responseField

responseTextList :: JSONRPC.RpcResponse -> Text -> [Text]
responseTextList response field =
  fromMaybe [] (responseField response field)

responsePlatformRefs :: JSONRPC.RpcResponse -> [(Text, Text, Text)]
responsePlatformRefs response =
  case response of
    WireJSONRPC.ResponseMessage result ->
      fromMaybe [] do
        refs <- AesonTypes.parseMaybe (Aeson.withObject "media" (Aeson..: "platformRefs")) result.result
        traverse platformRef refs
    _ ->
      []
  where
    platformRef =
      AesonTypes.parseMaybe $
        Aeson.withObject "platform ref" \o -> do
          platform <- o Aeson..: "platform"
          scope <- o Aeson..: "scope"
          ref <- o Aeson..: "platformRef"
          pure (platform, scope, ref)

responseMediaLocalPath :: JSONRPC.RpcResponse -> Maybe FilePath
responseMediaLocalPath response =
  responseField response "localPath"

responseErrorCode :: JSONRPC.RpcResponse -> Maybe Text
responseErrorCode = \case
  WireJSONRPC.ErrorMessage err ->
    AesonTypes.parseMaybe
      (Aeson.withObject "error data" (Aeson..: "code"))
      =<< err.error.errorData
  _ ->
    Nothing

responseSessionLabel :: JSONRPC.RpcResponse -> Maybe Text
responseSessionLabel response =
  case response of
    WireJSONRPC.ResponseMessage result -> do
      session <- AesonTypes.parseMaybe (Aeson.withObject "rename" (Aeson..: "session")) result.result
      AesonTypes.parseMaybe (Aeson.withObject "session" (Aeson..: "label")) session
    _ ->
      Nothing
