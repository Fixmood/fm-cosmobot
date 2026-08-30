module Main (main) where

import Bot.Chat.Driver.Types (ChatDriverEffects)
import qualified Bot.Chat.Driver.Types as Driver
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Storage.SQLite as StorageSQLite
import qualified Data.Aeson as Aeson
import Bot.Core.Route
import Bot.Handler.Scratchpad
import Bot.Core.Message
import Bot.Prelude
import Effectful.FileSystem
import qualified Effectful.Prim.IORef as IORef
import System.FilePath ((</>))
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit

newtype ReplyChatDriver es =
  ReplyChatDriver (IncomingMessage -> Text -> Eff es (Either Text MessageId))

instance Driver.ChatDriver (ReplyChatDriver es0) where
  type ChatDriverEffects (ReplyChatDriver es0) es = es ~ es0
  driverPlatform _ = PlatformTelegram
  sendReplyMessage (ReplyChatDriver sendReply) =
    sendReply

main :: IO ()
main =
  defaultMain $
    testGroup "scratchpad"
      [ effTestCase "todo command flow" testScratchpadTodoFlow
      , effTestCase "todo lists are scoped by sender" testScratchpadSenderIsolation
      , effTestCase "todo list persists through SQLite" testScratchpadPersistence
      , effTestCase "invalid commands reply with usage or missing item" testScratchpadInvalidCommands
      , effTestCase "messages without sender are rejected" testScratchpadMissingSender
      ]

effTestCase :: TestName -> Eff '[Concurrent, Prim, FileSystem, IOE] () -> TestTree
effTestCase name =
  testCase name . runEff . runFileSystem . runPrim . runConcurrent

testScratchpadTodoFlow :: (Concurrent :> es, FileSystem :> es, IOE :> es, Prim :> es) => Eff es ()
testScratchpadTodoFlow = withScratchpadStore "flow" \store -> do
  replies <- IORef.newIORef ([] :: [Text])
  runScratchpad store replies (message "!todo buy milk")
  runScratchpad store replies (message "!todo write tests")
  runScratchpad store replies (message "!done 1")
  runScratchpad store replies (message "!list")
  runScratchpad store replies (message "!rm 1")
  runScratchpad store replies (message "!todo")
  runScratchpad store replies (message "!clear")
  runScratchpad store replies (message "!list")
  actual <- IORef.readIORef replies
  liftIO $
    actual @?=
    [ "已添加 #1: buy milk"
    , "已添加 #2: write tests"
    , "已完成 #1: buy milk"
    , "- [X] 1. buy milk\n- [ ] 2. write tests\n"
    , "已删除 1 项。"
    , "- [ ] 1. write tests\n"
    , "已清空 todo list。"
    , "todo list 为空。"
    ]

testScratchpadSenderIsolation :: (Concurrent :> es, FileSystem :> es, IOE :> es, Prim :> es) => Eff es ()
testScratchpadSenderIsolation = withScratchpadStore "sender-isolation" \store -> do
  replies <- IORef.newIORef ([] :: [Text])
  runScratchpad store replies (messageFrom "200" "!todo alice task")
  runScratchpad store replies (messageFrom "201" "!todo bob task")
  runScratchpad store replies (messageFrom "200" "!list")
  runScratchpad store replies (messageFrom "201" "!list")
  actual <- IORef.readIORef replies
  liftIO $
    actual @?=
    [ "已添加 #1: alice task"
    , "已添加 #1: bob task"
    , "- [ ] 1. alice task\n"
    , "- [ ] 1. bob task\n"
    ]

testScratchpadPersistence :: (Concurrent :> es, FileSystem :> es, IOE :> es, Prim :> es) => Eff es ()
testScratchpadPersistence = withScratchpadPath "persistence" \path -> do
  writeReplies <- IORef.newIORef ([] :: [Text])
  runScratchpad path writeReplies (message "!todo persists")
  readReplies <- IORef.newIORef ([] :: [Text])
  runScratchpad path readReplies (message "!list")
  actual <- IORef.readIORef readReplies
  liftIO $ actual @?= ["- [ ] 1. persists\n"]

testScratchpadInvalidCommands :: (Concurrent :> es, FileSystem :> es, IOE :> es, Prim :> es) => Eff es ()
testScratchpadInvalidCommands = withScratchpadStore "invalid-commands" \store -> do
  replies <- IORef.newIORef ([] :: [Text])
  runScratchpad store replies (message "!done")
  runScratchpad store replies (message "!done nope")
  runScratchpad store replies (message "!todo only task")
  runScratchpad store replies (message "!done 2")
  runScratchpad store replies (message "!rm")
  actual <- IORef.readIORef replies
  liftIO $
    actual @?=
    [ "用法：!done <todo编号>"
    , "用法：!done <todo编号>"
    , "已添加 #1: only task"
    , "没有编号 2 的 todo。"
    , "用法：!rm <编号1> <编号2> ..."
    ]

testScratchpadMissingSender :: (Concurrent :> es, FileSystem :> es, IOE :> es, Prim :> es) => Eff es ()
testScratchpadMissingSender = withScratchpadStore "missing-sender" \store -> do
  replies <- IORef.newIORef ([] :: [Text])
  runScratchpad store replies (messageWithoutSender "!todo unsaved")
  actual <- IORef.readIORef replies
  liftIO $ actual @?= ["无法识别发送者，不能保存 todo。"]

withScratchpadStore :: (Concurrent :> es, FileSystem :> es, IOE :> es) => String -> (FilePath -> Eff es ()) -> Eff es ()
withScratchpadStore label action =
  withScratchpadPath label action

withScratchpadPath :: (Concurrent :> es, FileSystem :> es, IOE :> es) => String -> (FilePath -> Eff es ()) -> Eff es ()
withScratchpadPath label action = do
  tmp <- getTemporaryDirectory
  let path = tmp </> ("cosmobot-scratchpad-spec-" <> label <> ".sqlite")
  removeFile path `catch` \(_ :: IOException) -> pure ()
  action path

runScratchpad :: (Concurrent :> es, IOE :> es, Prim :> es) => FilePath -> IORef.IORef [Text] -> IncomingMessage -> Eff es ()
runScratchpad path replies incoming =
  Chat.runChatWith (ReplyChatDriver reply) $
    StorageSQLite.runStorageSQLitePath path $
      runHandlers scratchpadHandlers incoming
  where
    reply _ body = do
      IORef.modifyIORef' replies (<> [body])
      pure (Right "1")

message :: Text -> IncomingMessage
message =
  messageFrom "200"

messageFrom :: Text -> Text -> IncomingMessage
messageFrom senderId text =
  IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just 100
    , chatAliases = []
    , digest = emptyMessageDigest
    , senderId = Just senderId
    , senderUsername = Just "alice"
    , messageId = Just "300"
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , files = []
    , text = text
    , raw = Aeson.Null
    }

messageWithoutSender :: Text -> IncomingMessage
messageWithoutSender text =
  (message text)
    { senderId = Nothing
    , senderUsername = Nothing
    }
