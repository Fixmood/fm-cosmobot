module Main (main) where

import qualified Bot.ACP.Config as ACPConfig
import qualified Bot.ACP.Client as ACPClient
import qualified Bot.ACP.Server as ACPServer
import qualified Bot.ACP.State as ACPState
import qualified Bot.JSONRPC as RPC
import qualified Bot.Chat.Driver.ACP as ACPDriver
import qualified Bot.Chat.Driver.Types as Driver
import qualified Bot.Concurrency.Manager as ConcurrencyManager
import Bot.Core.Message (ChatKind (ChatPrivate), ChatPlatform (PlatformACP), IncomingMessage (..), IncomingMessageEventKind (IncomingMessageCreated), MessageDigest (..))
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.ACP as ACPEffect
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Session as Session
import Bot.Prelude
import qualified Bot.Storage.SQLite as StorageSQLite
import qualified Bot.Storage.Thread as ThreadStore
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.Async as Async
import qualified Effectful.Concurrent.MVar as MVar
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified JSONRPC
import qualified Network.Socket as Socket
import qualified Network.WebSockets as WS
import qualified Streaming.Prelude as S
import System.IO.Error (userError)
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit
import qualified Toml
import Toml.Schema

newtype AcpClientConfig = AcpClientConfig
  { acp :: ACPConfig.FileConfig
  }
  deriving (Show)

instance FromValue AcpClientConfig where
  fromValue = parseTableFromValue $
    AcpClientConfig
      <$> fmap (fromMaybe ACPConfig.defaultFileConfig) (optKey "acp")

main :: IO ()
main =
  defaultMain $
    testGroup "acp"
      [ testCase "enabled config requires token" testEnabledConfigRequiresToken
      , testCase "session/new creates session and session/delete deletes it" testSessionNewAndDelete
      , testCase "session/list returns durable sessions with cwd filter" testSessionList
      , testCase "session/load replays durable text history" testSessionLoadReplaysHistory
      , testCase "session/resume close and cancel check existing sessions" testSessionExistingRequests
      , testCase "session/cancel resolves active prompt as cancelled" testSessionCancelResolvesPrompt
      , testCase "session/prompt streams text response updates" testSessionPromptStreamsTextResponse
      , testCase "session/prompt clears completed prompt cancellation state" testSessionPromptClearsCompletedPrompt
      , testCase "session/prompt does not complete on nonfinal reply" testSessionPromptIgnoresNonfinalReply
      , testCase "session/prompt accepts and streams image content" testSessionPromptAcceptsAndStreamsImageContent
      , testCase "session/prompt streams generated file images as image content" testSessionPromptStreamsGeneratedFileImageContent
      , testCase "ACP client file read routes to uninitialized active client" testClientFileReadRoutesToActiveClient
      , testCase "ACP client file write routes to active client" testClientFileWriteRoutesToActiveClient
      , testCase "ACP client file request checks advertised capability" testClientFileRequestChecksAdvertisedCapability
      , testCase "ACP client terminal routes to active client" testClientTerminalRoutesToActiveClient
      , testCase "ACP client terminal rejects uninitialized active client" testClientTerminalRejectsUninitializedClient
      , testCase "ACP client terminal defaults to unsupported for partial capabilities" testClientTerminalDefaultsToUnsupported
      , testCase "ACP client terminal checks advertised capability" testClientTerminalChecksAdvertisedCapability
      , testCase "websocket server authenticates and handles initialize" testWebSocketServerAuthenticatesAndHandlesInitialize
      ]

testEnabledConfigRequiresToken :: IO ()
testEnabledConfigRequiresToken =
  case Toml.decode "[acp]\nenabled = true\n" of
    Toml.Failure errors ->
      assertBool
        "expected acp.token validation failure"
        ("acp.token must be non-empty" `Text.isInfixOf` Text.unlines (map toText errors))
    Toml.Success _warnings (config_ :: AcpClientConfig) ->
      assertFailure [i|expected parse failure, got #{show config_ :: String}|]

testSessionNewAndDelete :: IO ()
testSessionNewAndDelete =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    newResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/new" $
        Aeson.object
          [ "cwd" Aeson..= ("/tmp/cosmobot-acp-spec" :: Text)
          , "mcpServers" Aeson..= ([] :: [Aeson.Value])
          ]
    sessionId <- liftIO $
      case responseField newResponse "sessionId" of
        Nothing ->
          assertFailure "expected sessionId"
        Just (value :: Text) ->
          pure value
    deleteResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/delete" $
        Aeson.object
          [ "sessionId" Aeson..= sessionId
          ]
    liftIO do
      responseHasObjectResult deleteResponse @?= True

testSessionList :: IO ()
testSessionList =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    _local <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/new" $
        Aeson.object
          [ "cwd" Aeson..= ("/tmp/cosmobot-acp-spec" :: Text)
          , "mcpServers" Aeson..= ([] :: [Aeson.Value])
          ]
    _other <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/new" $
        Aeson.object
          [ "cwd" Aeson..= ("/tmp/cosmobot-acp-other" :: Text)
          , "mcpServers" Aeson..= ([] :: [Aeson.Value])
          ]
    listResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/list" $
        Aeson.object
          [ "cwd" Aeson..= ("/tmp/cosmobot-acp-spec" :: Text)
          ]
    liftIO do
      responseField listResponse "sessions" @?=
        Just
          [ Aeson.object
              [ "sessionId" Aeson..= ("/tmp/cosmobot-acp-spec-1" :: Text)
              , "cwd" Aeson..= ("/tmp/cosmobot-acp-spec" :: Text)
              , "title" Aeson..= Just ("/tmp/cosmobot-acp-spec" :: Text)
              ]
          ]

testSessionLoadReplaysHistory :: IO ()
testSessionLoadReplaysHistory =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    session <- ACPState.openSession (Just "/tmp/cosmobot-acp-spec")
    userMessage <- ACPState.enqueueUserMessage acpState $
      Session.SessionSend
        { sessionId = session.sessionId
        , text = "question"
        , imageUrls = []
        , attachments = []
        , replyToMessageId = Nothing
        }
    incoming <- liftIO $
      case userMessage of
        Left err ->
          assertFailure [i|unexpected ACP user message error: #{err}|]
        Right Nothing ->
          assertFailure "expected stored ACP user message"
        Right (Just incoming) ->
          pure incoming
    let driver = ACPDriver.acpChatDriver acpState
    Driver.sendReplyMessage driver incoming "answer" >>= \case
      Left err ->
        liftIO (assertFailure [i|unexpected ACP reply error: #{err}|])
      Right _ ->
        pure ()
    (_clientId, queue) <- ACPState.registerClient acpState
    loadResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/load" $
        Aeson.object
          [ "sessionId" Aeson..= ACPState.acpSessionIdText session.sessionId
          , "cwd" Aeson..= ("/tmp/cosmobot-acp-spec" :: Text)
          , "mcpServers" Aeson..= ([] :: [Aeson.Value])
          ]
    events <- replicateM 2 (ACPState.readClient queue)
    liftIO do
      responseResult loadResponse @?= Just Aeson.Null
      acpEventUpdateKinds events @?=
        [ "user_message_chunk"
        , "agent_message_chunk"
        ]

testSessionExistingRequests :: IO ()
testSessionExistingRequests =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    newResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/new" $
        Aeson.object
          [ "cwd" Aeson..= ("/tmp/cosmobot-acp-spec" :: Text)
          , "mcpServers" Aeson..= ([] :: [Aeson.Value])
          ]
    sessionId <- liftIO $
      case responseField newResponse "sessionId" of
        Nothing ->
          assertFailure "expected sessionId"
        Just (value :: Text) ->
          pure value
    responses <- traverse (dispatchSessionIdRequest acpState queue sessionId)
      [ "session/resume"
      , "session/close"
      , "session/cancel"
      ]
    missingResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/resume" $
        Aeson.object
          [ "sessionId" Aeson..= ("missing" :: Text)
          ]
    liftIO do
      map responseHasObjectResult responses @?= [True, True, True]
      responseErrorCode missingResponse @?= Just "not_found"

testSessionCancelResolvesPrompt :: IO ()
testSessionCancelResolvesPrompt =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    newResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/new" $
        Aeson.object
          [ "cwd" Aeson..= ("/tmp/cosmobot-acp-spec" :: Text)
          , "mcpServers" Aeson..= ([] :: [Aeson.Value])
          ]
    sessionId <- liftIO $
      case responseField newResponse "sessionId" of
        Nothing ->
          assertFailure "expected sessionId"
        Just (value :: Text) ->
          pure value
    promptDone <- MVar.newEmptyMVar
    _promptTask <- Concurrency.fork "acp-spec.cancel.prompt" do
      response <- ACPServer.dispatchAcpRequest acpState queue $
        acpRequest "session/prompt" $
          Aeson.object
            [ "sessionId" Aeson..= sessionId
            , "prompt" Aeson..=
                [ Aeson.object
                    [ "type" Aeson..= ("text" :: Text)
                    , "text" Aeson..= ("wait" :: Text)
                    ]
                ]
            ]
      MVar.putMVar promptDone response
    _incoming <- S.head_ (ACPState.incomingMessages acpState)
    cancelResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/cancel" $
        Aeson.object
          [ "sessionId" Aeson..= sessionId
          ]
    promptResponse <- MVar.takeMVar promptDone
    liftIO do
      responseHasObjectResult cancelResponse @?= True
      responseField promptResponse "stopReason" @?= Just ("cancelled" :: Text)

testSessionPromptStreamsTextResponse :: IO ()
testSessionPromptStreamsTextResponse =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    newResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/new" $
        Aeson.object
          [ "cwd" Aeson..= ("/tmp/cosmobot-acp-spec" :: Text)
          , "mcpServers" Aeson..= ([] :: [Aeson.Value])
          ]
    sessionId <- liftIO $
      case responseField newResponse "sessionId" of
        Nothing ->
          assertFailure "expected sessionId"
        Just (value :: Text) ->
          pure value
    responderDone <- MVar.newEmptyMVar
    _responder <- forkIO do
      S.head_ (ACPState.incomingMessages acpState) >>= \case
        Nothing ->
          throwIO (userError "expected ACP incoming message")
        Just incoming -> do
          let driver = ACPDriver.acpChatDriver acpState
          Driver.sendStreamingReplyMessage driver incoming "hel" >>= \case
            Left err ->
              throwIO (userError (Text.unpack err))
            Right messageId -> do
              _ <- Driver.editMessage driver incoming messageId "hello"
              _ <- Driver.completeMessageEdit driver incoming messageId
              MVar.putMVar responderDone ()
    promptResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/prompt" $
        Aeson.object
          [ "sessionId" Aeson..= sessionId
          , "prompt" Aeson..=
              [ Aeson.object
                  [ "type" Aeson..= ("text" :: Text)
                  , "text" Aeson..= ("say hello" :: Text)
                  ]
              ]
          ]
    MVar.takeMVar responderDone
    events <- replicateM 3 (ACPState.readClient queue)
    liftIO do
      responseField promptResponse "stopReason" @?= Just ("end_turn" :: Text)
      acpEventUpdateKinds events @?=
        [ "user_message_chunk"
        , "agent_message_chunk"
        , "agent_message_chunk"
        ]
      acpEventContentText events @?=
        [ "say hello"
        , "hel"
        , "lo"
        ]

testSessionPromptClearsCompletedPrompt :: IO ()
testSessionPromptClearsCompletedPrompt =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    newResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/new" $
        Aeson.object
          [ "cwd" Aeson..= ("/tmp/cosmobot-acp-spec" :: Text)
          , "mcpServers" Aeson..= ([] :: [Aeson.Value])
          ]
    sessionId <- liftIO $
      case responseField newResponse "sessionId" of
        Nothing ->
          assertFailure "expected sessionId"
        Just (value :: Text) ->
          pure value
    responderDone <- MVar.newEmptyMVar
    _responder <- forkIO do
      S.head_ (ACPState.incomingMessages acpState) >>= \case
        Nothing ->
          throwIO (userError "expected ACP incoming message")
        Just incoming -> do
          let driver = ACPDriver.acpChatDriver acpState
          Driver.sendStreamingReplyMessage driver incoming "done" >>= \case
            Left err ->
              throwIO (userError (Text.unpack err))
            Right messageId -> do
              _ <- Driver.completeMessageEdit driver incoming messageId
              MVar.putMVar responderDone ()
    promptResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/prompt" $
        Aeson.object
          [ "sessionId" Aeson..= sessionId
          , "prompt" Aeson..=
              [ Aeson.object
                  [ "type" Aeson..= ("text" :: Text)
                  , "text" Aeson..= ("say done" :: Text)
                  ]
              ]
          ]
    MVar.takeMVar responderDone
    cancelled <- ACPState.cancelSessionPrompts acpState (Session.SessionId sessionId)
    liftIO do
      responseField promptResponse "stopReason" @?= Just ("end_turn" :: Text)
      cancelled @?= []

testSessionPromptIgnoresNonfinalReply :: IO ()
testSessionPromptIgnoresNonfinalReply =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    newResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/new" $
        Aeson.object
          [ "cwd" Aeson..= ("/tmp/cosmobot-acp-spec" :: Text)
          , "mcpServers" Aeson..= ([] :: [Aeson.Value])
          ]
    sessionId <- liftIO $
      case responseField newResponse "sessionId" of
        Nothing ->
          assertFailure "expected sessionId"
        Just (value :: Text) ->
          pure value
    promptDone <- MVar.newEmptyMVar
    progressSent <- MVar.newEmptyMVar
    allowComplete <- MVar.newEmptyMVar
    _promptTask <- Concurrency.fork "acp-spec.nonfinal.prompt" do
      response <- ACPServer.dispatchAcpRequest acpState queue $
        acpRequest "session/prompt" $
          Aeson.object
            [ "sessionId" Aeson..= sessionId
            , "prompt" Aeson..=
                [ Aeson.object
                    [ "type" Aeson..= ("text" :: Text)
                    , "text" Aeson..= ("send progress then done" :: Text)
                    ]
                ]
            ]
      MVar.putMVar promptDone response
    _responder <- forkIO do
      S.head_ (ACPState.incomingMessages acpState) >>= \case
        Nothing ->
          throwIO (userError "expected ACP incoming message")
        Just incoming -> do
          let driver = ACPDriver.acpChatDriver acpState
          Driver.sendReplyMessage driver incoming "progress" >>= \case
            Left err ->
              throwIO (userError (Text.unpack err))
            Right _ ->
              MVar.putMVar progressSent ()
          MVar.takeMVar allowComplete
          Driver.sendStreamingReplyMessage driver incoming "done" >>= \case
            Left err ->
              throwIO (userError (Text.unpack err))
            Right messageId -> do
              _ <- Driver.completeMessageEdit driver incoming messageId
              pure ()
    MVar.takeMVar progressSent
    promptBeforeComplete <- MVar.tryReadMVar promptDone
    MVar.putMVar allowComplete ()
    promptResponse <- MVar.takeMVar promptDone
    events <- replicateM 3 (ACPState.readClient queue)
    liftIO do
      promptBeforeComplete @?= Nothing
      responseField promptResponse "stopReason" @?= Just ("end_turn" :: Text)
      acpEventContentText events @?=
        [ "send progress then done"
        , "progress"
        , "done"
        ]

testSessionPromptAcceptsAndStreamsImageContent :: IO ()
testSessionPromptAcceptsAndStreamsImageContent =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    newResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/new" $
        Aeson.object
          [ "cwd" Aeson..= ("/tmp/cosmobot-acp-spec" :: Text)
          , "mcpServers" Aeson..= ([] :: [Aeson.Value])
          ]
    sessionId <- liftIO $
      case responseField newResponse "sessionId" of
        Nothing ->
          assertFailure "expected sessionId"
        Just (value :: Text) ->
          pure value
    responderDone <- MVar.newEmptyMVar
    _responder <- forkIO do
      S.head_ (ACPState.incomingMessages acpState) >>= \case
        Nothing ->
          throwIO (userError "expected ACP incoming message")
        Just incoming -> do
          liftIO (incoming.imageUrls @?= ["data:image/png;base64,AAAA"])
          let driver = ACPDriver.acpChatDriver acpState
          Driver.sendStreamingReplyMessage driver incoming
            "done\n[image] data:image/png;base64,BBBB" >>= \case
            Left err ->
              throwIO (userError (Text.unpack err))
            Right messageId -> do
              _ <- Driver.completeMessageEdit driver incoming messageId
              MVar.putMVar responderDone ()
    promptResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/prompt" $
        Aeson.object
          [ "sessionId" Aeson..= sessionId
          , "prompt" Aeson..=
              [ Aeson.object
                  [ "type" Aeson..= ("text" :: Text)
                  , "text" Aeson..= ("describe this" :: Text)
                  ]
              , Aeson.object
                  [ "type" Aeson..= ("image" :: Text)
                  , "mimeType" Aeson..= ("image/png" :: Text)
                  , "data" Aeson..= ("AAAA" :: Text)
                  ]
              ]
          ]
    MVar.takeMVar responderDone
    events <- replicateM 4 (ACPState.readClient queue)
    liftIO do
      responseField promptResponse "stopReason" @?= Just ("end_turn" :: Text)
      acpEventContentTypes events @?= ["text", "image", "text", "image"]
      acpEventImageData events @?= ["AAAA", "BBBB"]

testSessionPromptStreamsGeneratedFileImageContent :: IO ()
testSessionPromptStreamsGeneratedFileImageContent =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    newResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/new" $
        Aeson.object
          [ "cwd" Aeson..= ("/tmp/cosmobot-acp-spec" :: Text)
          , "mcpServers" Aeson..= ([] :: [Aeson.Value])
          ]
    sessionId <- liftIO $
      case responseField newResponse "sessionId" of
        Nothing ->
          assertFailure "expected sessionId"
        Just (value :: Text) ->
          pure value
    let imagePath = "/tmp/cosmobot-acp-generated-image.png"
    FileSystemByteString.writeFile imagePath "png-bytes"
    responderDone <- MVar.newEmptyMVar
    _responder <- forkIO do
      S.head_ (ACPState.incomingMessages acpState) >>= \case
        Nothing ->
          throwIO (userError "expected ACP incoming message")
        Just incoming -> do
          let driver = ACPDriver.acpChatDriver acpState
          Driver.sendStreamingReplyMessage driver incoming
            ("done\n[image] file://" <> Text.pack imagePath) >>= \case
            Left err ->
              throwIO (userError (Text.unpack err))
            Right messageId -> do
              _ <- Driver.completeMessageEdit driver incoming messageId
              MVar.putMVar responderDone ()
    promptResponse <- ACPServer.dispatchAcpRequest acpState queue $
      acpRequest "session/prompt" $
        Aeson.object
          [ "sessionId" Aeson..= sessionId
          , "prompt" Aeson..=
              [ Aeson.object
                  [ "type" Aeson..= ("text" :: Text)
                  , "text" Aeson..= ("draw this" :: Text)
                  ]
              ]
          ]
    MVar.takeMVar responderDone
    events <- replicateM 3 (ACPState.readClient queue)
    liftIO do
      responseField promptResponse "stopReason" @?= Just ("end_turn" :: Text)
      acpEventContentTypes events @?= ["text", "text", "image"]
      acpEventImageData events @?= ["cG5nLWJ5dGVz"]

testClientFileReadRoutesToActiveClient :: IO ()
testClientFileReadRoutesToActiveClient =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    let sessionId = Session.SessionId "session-1"
        message = acpToolMessage sessionId
        requestAction =
          ACPClient.runACP acpState $
            ACPEffect.readClientFile message "notes.md" (Just 2) (Just 5)
        responseAction = do
          request <- readClientRequest queue
          liftIO do
            request.method @?= "fs/read_text_file"
            request.params @?=
              Aeson.object
                [ "sessionId" Aeson..= ("session-1" :: Text)
                , "path" Aeson..= ("notes.md" :: Text)
                , "line" Aeson..= (2 :: Int)
                , "limit" Aeson..= (5 :: Int)
                ]
          resolveClientRequest acpState queue request (Aeson.object ["content" Aeson..= ("hello" :: Text)])
    (readResult, ()) <- ACPState.withActiveSessionClient acpState queue sessionId $
      Async.concurrently requestAction responseAction
    liftIO $
      readResult @?= Right "hello"

testClientFileWriteRoutesToActiveClient :: IO ()
testClientFileWriteRoutesToActiveClient =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    initializeWithFs acpState queue True True
    let sessionId = Session.SessionId "session-1"
        message = acpToolMessage sessionId
        requestAction =
          ACPClient.runACP acpState $
            ACPEffect.writeClientFile message "notes.md" "hello"
        responseAction = do
          request <- readClientRequest queue
          liftIO do
            request.method @?= "fs/write_text_file"
            request.params @?=
              Aeson.object
                [ "sessionId" Aeson..= ("session-1" :: Text)
                , "path" Aeson..= ("notes.md" :: Text)
                , "content" Aeson..= ("hello" :: Text)
                ]
          resolveClientRequest acpState queue request Aeson.Null
    (writeResult, ()) <- ACPState.withActiveSessionClient acpState queue sessionId $
      Async.concurrently requestAction responseAction
    liftIO $
      writeResult @?= Right ()

testClientFileRequestChecksAdvertisedCapability :: IO ()
testClientFileRequestChecksAdvertisedCapability =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    initializeWithFs acpState queue False True
    let sessionId = Session.SessionId "session-1"
        message = acpToolMessage sessionId
    result <- ACPState.withActiveSessionClient acpState queue sessionId $
      ACPClient.runACP acpState $
        ACPEffect.readClientFile message "notes.md" Nothing Nothing
    liftIO $
      result @?= Left "ACP client does not support fs/read_text_file."

testClientTerminalRoutesToActiveClient :: IO ()
testClientTerminalRoutesToActiveClient =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    initializeWithTerminal acpState queue True
    let sessionId = Session.SessionId "session-1"
        message = acpToolMessage sessionId
        terminalCreate = ACPEffect.TerminalCreate
          { command = "printf"
          , args = ["hello"]
          , env = [("LANG", "C")]
          , cwd = Just "/tmp/project"
          , outputByteLimit = Just 1024
          }
    ACPState.withActiveSessionClient acpState queue sessionId do
      (createResult, ()) <- Async.concurrently
          (ACPClient.runACP acpState (ACPEffect.createClientTerminal message terminalCreate))
          do
            request <- readClientRequest queue
            liftIO do
              request.method @?= "terminal/create"
              request.params @?=
                Aeson.object
                  [ "sessionId" Aeson..= ("session-1" :: Text)
                  , "command" Aeson..= ("printf" :: Text)
                  , "args" Aeson..= ["hello" :: Text]
                  , "env" Aeson..=
                      [ Aeson.object
                          [ "name" Aeson..= ("LANG" :: Text)
                          , "value" Aeson..= ("C" :: Text)
                          ]
                      ]
                  , "cwd" Aeson..= ("/tmp/project" :: Text)
                  , "outputByteLimit" Aeson..= (1024 :: Int)
                  ]
            resolveClientRequest acpState queue request (Aeson.object ["terminalId" Aeson..= ("term-1" :: Text)])
      liftIO $ createResult @?= Right "term-1"

      (outputResult, ()) <- Async.concurrently
          (ACPClient.runACP acpState (ACPEffect.readClientTerminalOutput message "term-1"))
          do
            terminalRequestRoundTrip acpState queue "terminal/output" "term-1" $
              Aeson.object
                [ "output" Aeson..= ("hello" :: Text)
                , "truncated" Aeson..= False
                , "exitStatus" Aeson..=
                    Aeson.object
                      [ "exitCode" Aeson..= (0 :: Int)
                      , "signal" Aeson..= Aeson.Null
                      ]
                ]
      liftIO $
        outputResult @?=
          Right ACPEffect.TerminalOutput
            { output = "hello"
            , truncated = False
            , exitStatus = Just ACPEffect.TerminalExitStatus{exitCode = Just 0, signal = Nothing}
            }

      (waitResult, ()) <- Async.concurrently
          (ACPClient.runACP acpState (ACPEffect.waitForClientTerminalExit message "term-1"))
          do
            terminalRequestRoundTrip acpState queue "terminal/wait_for_exit" "term-1" $
              Aeson.object
                [ "exitCode" Aeson..= (0 :: Int)
                , "signal" Aeson..= Aeson.Null
                ]
      liftIO $
        waitResult @?= Right ACPEffect.TerminalExitStatus{exitCode = Just 0, signal = Nothing}

      (killResult, ()) <- Async.concurrently
          (ACPClient.runACP acpState (ACPEffect.killClientTerminal message "term-1"))
          (terminalRequestRoundTrip acpState queue "terminal/kill" "term-1" Aeson.Null)
      liftIO $ killResult @?= Right ()

      (releaseResult, ()) <- Async.concurrently
          (ACPClient.runACP acpState (ACPEffect.releaseClientTerminal message "term-1"))
          (terminalRequestRoundTrip acpState queue "terminal/release" "term-1" Aeson.Null)
      liftIO $ releaseResult @?= Right ()

testClientTerminalRejectsUninitializedClient :: IO ()
testClientTerminalRejectsUninitializedClient =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    let sessionId = Session.SessionId "session-1"
        message = acpToolMessage sessionId
    result <- ACPState.withActiveSessionClient acpState queue sessionId $
      ACPClient.runACP acpState $
        ACPEffect.readClientTerminalOutput message "term-1"
    liftIO $
      result @?= Left "ACP client does not support terminal/output."

testClientTerminalDefaultsToUnsupported :: IO ()
testClientTerminalDefaultsToUnsupported =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    initializeWithFs acpState queue True True
    let sessionId = Session.SessionId "session-1"
        message = acpToolMessage sessionId
    result <- ACPState.withActiveSessionClient acpState queue sessionId $
      ACPClient.runACP acpState $
        ACPEffect.readClientTerminalOutput message "term-1"
    liftIO $
      result @?= Left "ACP client does not support terminal/output."

testClientTerminalChecksAdvertisedCapability :: IO ()
testClientTerminalChecksAdvertisedCapability =
  runAcpStorage do
    acpState <- ACPState.newAcpState
    (_clientId, queue) <- ACPState.registerClient acpState
    initializeWithTerminal acpState queue False
    let sessionId = Session.SessionId "session-1"
        message = acpToolMessage sessionId
    result <- ACPState.withActiveSessionClient acpState queue sessionId $
      ACPClient.runACP acpState $
        ACPEffect.readClientTerminalOutput message "term-1"
    liftIO $
      result @?= Left "ACP client does not support terminal/output."

testWebSocketServerAuthenticatesAndHandlesInitialize :: IO ()
testWebSocketServerAuthenticatesAndHandlesInitialize = do
  result <- timeout 10_000_000 $ runAcpStorage do
    listenSocket <- liftIO (WS.makeListenSocket "127.0.0.1" 0)
    port <- (fromIntegral :: Socket.PortNumber -> Int) <$> liftIO (Socket.socketPort listenSocket)
    acpState <- ACPState.newAcpState
    threads <- ThreadStore.newThreadStore
    let cfg = ACPConfig.Config
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
              ACPServer.acpServerApp cfg threads acpState pending)
            (liftIO (Socket.close listenSocket))
        client = do
          unauthorized <- try @WS.HandshakeException (liftIO (WS.runClient "127.0.0.1" port "/acp" \_ -> pure ()))
          response <- liftIO (initializeClient port "secret")
          pure (unauthorized, response)
    Async.race server client

  case result of
    Nothing ->
      assertFailure "ACP websocket integration test timed out"
    Just (Left ()) ->
      assertFailure "ACP server exited before client completed"
    Just (Right (unauthorized, response)) -> do
      assertBool "expected unauthenticated websocket rejection" (isLeft unauthorized)
      response @?= initializeResponse

initializeClient :: Int -> Text -> IO RPC.JsonRpcResponse
initializeClient port token =
  WS.runClientWith "127.0.0.1" port "/acp" WS.defaultConnectionOptions [("Authorization", "Bearer " <> TextEncoding.encodeUtf8 token)] \conn -> do
    WS.sendTextData conn $
      Aeson.encode $
        JSONRPC.JSONRPCRequest
          JSONRPC.rPC_VERSION
          (JSONRPC.RequestId (Aeson.String "test-1"))
          "initialize"
          ( Aeson.object
              [ "protocolVersion" Aeson..= (1 :: Int)
              , "clientCapabilities" Aeson..= Aeson.object []
              , "clientInfo" Aeson..=
                  Aeson.object
                    [ "name" Aeson..= ("acp-spec" :: Text)
                    , "version" Aeson..= ("0.0.0" :: Text)
                    ]
              ]
          )
    bytes <- WS.receiveData conn :: IO ByteString.ByteString
    case Aeson.eitherDecodeStrict' bytes of
      Left err -> fail [i|ACP websocket response was not JSON-RPC: #{err}|]
      Right response -> pure response

initializeResponse :: RPC.JsonRpcResponse
initializeResponse =
  RPC.successResponse (JSONRPC.RequestId (Aeson.String "test-1")) $
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

acpRequest :: Text -> Aeson.Value -> RPC.JsonRpcRequest
acpRequest method params =
  JSONRPC.JSONRPCRequest JSONRPC.rPC_VERSION (JSONRPC.RequestId (Aeson.String "test-1")) method params

initializeWithFs
  :: (Concurrent :> es, Concurrency.Concurrency :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => ACPState.AcpState
  -> ACPState.AcpClientQueue
  -> Bool
  -> Bool
  -> Eff es ()
initializeWithFs acpState queue readTextFile writeTextFile = do
  response <- ACPServer.dispatchAcpRequest acpState queue $
    acpRequest "initialize" $
      Aeson.object
        [ "protocolVersion" Aeson..= (1 :: Int)
        , "clientCapabilities" Aeson..=
            Aeson.object
              [ "fs" Aeson..=
                  Aeson.object
                    [ "readTextFile" Aeson..= readTextFile
                    , "writeTextFile" Aeson..= writeTextFile
                    ]
              ]
        , "clientInfo" Aeson..= Aeson.object []
        ]
  liftIO $ response @?= initializeResponse

initializeWithTerminal
  :: (Concurrent :> es, Concurrency.Concurrency :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => ACPState.AcpState
  -> ACPState.AcpClientQueue
  -> Bool
  -> Eff es ()
initializeWithTerminal acpState queue terminal = do
  response <- ACPServer.dispatchAcpRequest acpState queue $
    acpRequest "initialize" $
      Aeson.object
        [ "protocolVersion" Aeson..= (1 :: Int)
        , "clientCapabilities" Aeson..=
            Aeson.object
              [ "terminal" Aeson..= terminal
              ]
        , "clientInfo" Aeson..= Aeson.object []
        ]
  liftIO $ response @?= initializeResponse

readClientRequest :: (Concurrent :> es, IOE :> es) => ACPState.AcpClientQueue -> Eff es RPC.JsonRpcRequest
readClientRequest queue =
  ACPState.readClient queue >>= \case
    ACPState.AcpClientSend value ->
      case Aeson.fromJSON value of
        Aeson.Success (JSONRPC.RequestMessage request) ->
          pure request
        other ->
          liftIO (assertFailure [i|expected ACP client request, got #{show other :: String}|])
    ACPState.AcpClientDisconnect reason ->
      liftIO (assertFailure [i|unexpected ACP client disconnect: #{reason}|])

resolveClientRequest
  :: (Concurrent :> es, IOE :> es)
  => ACPState.AcpState
  -> ACPState.AcpClientQueue
  -> RPC.JsonRpcRequest
  -> Aeson.Value
  -> Eff es ()
resolveClientRequest acpState queue request result = do
  resolved <- ACPState.resolveClientResponse acpState queue $
    JSONRPC.ResponseMessage $
      JSONRPC.JSONRPCResponse JSONRPC.rPC_VERSION request.id result
  liftIO $ assertBool "expected client response to resolve pending request" resolved

terminalRequestRoundTrip
  :: (Concurrent :> es, IOE :> es)
  => ACPState.AcpState
  -> ACPState.AcpClientQueue
  -> Text
  -> Text
  -> Aeson.Value
  -> Eff es ()
terminalRequestRoundTrip acpState queue method terminalId result = do
  request <- readClientRequest queue
  liftIO do
    request.method @?= method
    request.params @?=
      Aeson.object
        [ "sessionId" Aeson..= ("session-1" :: Text)
        , "terminalId" Aeson..= terminalId
        ]
  resolveClientRequest acpState queue request result

acpToolMessage :: ACPState.AcpSessionId -> IncomingMessage
acpToolMessage sessionId =
  IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = PlatformACP
    , kind = ChatPrivate
    , chatId = Nothing
    , chatAliases = [Session.sessionIdText sessionId]
    , digest = MessageDigest
        { chatIsAllowed = True
        , senderIsAllowed = True
        , senderIsSuperuser = True
        , mentionsBot = True
        , botId = Just "acp"
        }
    , senderId = Just "acp-user"
    , senderUsername = Just "ACP"
    , messageId = Just "message-1"
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , files = []
    , text = ""
    , raw = Aeson.Null
    }

responseField :: Aeson.FromJSON a => RPC.JsonRpcResponse -> Text -> Maybe a
responseField response name =
  case response of
    JSONRPC.ResponseMessage result -> do
      Aeson.Object object <- Just result.result
      AesonTypes.parseMaybe (Aeson..: fromString (Text.unpack name)) object
    JSONRPC.ErrorMessage{} ->
      Nothing
    JSONRPC.NotificationMessage{} ->
      Nothing
    JSONRPC.RequestMessage{} ->
      Nothing

responseHasObjectResult :: RPC.JsonRpcResponse -> Bool
responseHasObjectResult = \case
  JSONRPC.ResponseMessage result ->
    case result.result of
      Aeson.Object{} ->
        True
      _ ->
        False
  JSONRPC.ErrorMessage{} ->
    False
  JSONRPC.NotificationMessage{} ->
    False
  JSONRPC.RequestMessage{} ->
    False

responseResult :: RPC.JsonRpcResponse -> Maybe Aeson.Value
responseResult = \case
  JSONRPC.ResponseMessage result ->
    Just result.result
  JSONRPC.ErrorMessage{} ->
    Nothing
  JSONRPC.NotificationMessage{} ->
    Nothing
  JSONRPC.RequestMessage{} ->
    Nothing

responseErrorCode :: RPC.JsonRpcResponse -> Maybe Text
responseErrorCode = \case
  JSONRPC.ErrorMessage err -> do
    Aeson.Object object <- err.error.errorData
    AesonTypes.parseMaybe (Aeson..: "code") object
  JSONRPC.ResponseMessage{} ->
    Nothing
  JSONRPC.NotificationMessage{} ->
    Nothing
  JSONRPC.RequestMessage{} ->
    Nothing

dispatchSessionIdRequest
  :: (Concurrent :> es, Concurrency.Concurrency :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => ACPState.AcpState
  -> ACPState.AcpClientQueue
  -> Text
  -> Text
  -> Eff es RPC.JsonRpcResponse
dispatchSessionIdRequest acpState queue sessionId method =
  ACPServer.dispatchAcpRequest acpState queue $
    acpRequest method $
      Aeson.object
        [ "sessionId" Aeson..= sessionId
        ]

acpEventUpdateKinds :: [ACPState.AcpClientEvent] -> [Text]
acpEventUpdateKinds events =
  mapMaybe acpEventUpdateKind events

acpEventUpdateKind :: ACPState.AcpClientEvent -> Maybe Text
acpEventUpdateKind = \case
  ACPState.AcpClientSend value -> do
    JSONRPC.NotificationMessage notification_ <- Aeson.decode (Aeson.encode value)
    guard (notification_.method == "session/update")
    Aeson.Object params <- Just notification_.params
    Aeson.Object update <- AesonTypes.parseMaybe (Aeson..: "update") params
    AesonTypes.parseMaybe (Aeson..: "sessionUpdate") update
  ACPState.AcpClientDisconnect{} ->
    Nothing

acpEventContentTypes :: [ACPState.AcpClientEvent] -> [Text]
acpEventContentTypes events =
  mapMaybe (acpEventContentField "type") events

acpEventImageData :: [ACPState.AcpClientEvent] -> [Text]
acpEventImageData events =
  mapMaybe (acpEventContentField "data") events

acpEventContentText :: [ACPState.AcpClientEvent] -> [Text]
acpEventContentText events =
  mapMaybe (acpEventContentField "text") events

acpEventContentField :: Aeson.FromJSON a => Text -> ACPState.AcpClientEvent -> Maybe a
acpEventContentField field = \case
  ACPState.AcpClientSend value -> do
    JSONRPC.NotificationMessage notification_ <- Aeson.decode (Aeson.encode value)
    guard (notification_.method == "session/update")
    Aeson.Object params <- Just notification_.params
    Aeson.Object update <- AesonTypes.parseMaybe (Aeson..: "update") params
    Aeson.Object content <- AesonTypes.parseMaybe (Aeson..: "content") update
    AesonTypes.parseMaybe (Aeson..: fromString (Text.unpack field)) content
  ACPState.AcpClientDisconnect{} ->
    Nothing

runAcpStorage
  :: Eff '[Media.Media, Storage.Storage, KatipE, FileSystem.FileSystem, Concurrency.Concurrency, Prim, Concurrent, IOE] a
  -> IO a
runAcpStorage action =
  runEff $
    runConcurrent $
      runPrim $
        ConcurrencyManager.runConcurrencyManager $
          FileSystem.runFileSystem $
            runTestLog $
              StorageSQLite.runStorageSQLitePath ":memory:" $
                Media.runMediaPassthrough action

runTestLog :: IOE :> es => Eff (KatipE : es) a -> Eff es a
runTestLog =
  startKatipE "acp-spec" "test"
