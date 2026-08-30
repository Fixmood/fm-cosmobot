module Main (main) where

import Bot.Chat.Driver.Types (ChatDriverEffects)
import qualified Bot.Concurrency.Manager as ConcurrencyManager
import qualified Bot.Chat.Driver.Types as Driver
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Lifecycle as LifecycleEffect
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Skills as Skills
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Skills as SkillsStore
import qualified Bot.Storage.SQLite as StorageSQLite
import Bot.Core.Message
import Bot.Core.Route
import Bot.Handler.Admin
import Bot.Handler.Admin.Config
import qualified Bot.Lifecycle as Lifecycle
import qualified Bot.Media.Config as MediaConfig
import Bot.Prelude
import qualified Bot.Storage.Lifecycle as LifecycleStorage
import qualified Data.Aeson as Aeson
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.FilePath ((</>))
import Effectful.FileSystem (runFileSystem)
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified Effectful.Concurrent.MVar as MVar
import qualified Effectful.Temporary as Temporary
import Effectful.Process (Process, runProcess)
import Test.Tasty
import Test.Tasty.HUnit

data AdminChatDriver es = AdminChatDriver
  { sendReply :: IncomingMessage -> Text -> Eff es (Either Text MessageId)
  , setTitle :: IncomingMessage -> Text -> Text -> Eff es Bool
  }

instance Driver.ChatDriver (AdminChatDriver es0) where
  type ChatDriverEffects (AdminChatDriver es0) es = es ~ es0
  driverPlatform _ = PlatformTelegram
  sendReplyMessage driver = driver.sendReply
  setMemberTitle driver = driver.setTitle

main :: IO ()
main =
  defaultMain $
    testGroup "admin"
      [ testCase "ping replies pong for any sender" testPingRepliesPong
      , testCase "lifecycle restart request restarts runtime" testLifecycleRestartRequestRestartsRuntime
      , testCase "lifecycle startup replies are deleted after drain" testLifecycleStartupRepliesAreDeletedAfterDrain
      ]

testPingRepliesPong :: IO ()
testPingRepliesPong = do
  replies <- IORef.newIORef ([] :: [Text])
  actions <- runAdmin defaultAdminConfig replies message
  IORef.readIORef replies >>= (@?= ["pong"])
  assertBool "no startup actions queued" (null actions)

testReloadReloadsSkillList :: IO ()
testReloadReloadsSkillList =
  runEff $
    runPrim $
    runConcurrent $
    runFileSystem $
    Temporary.runTemporary $
      Temporary.withSystemTempDirectory "cosmobot-admin-skills-" \tmp -> do
        replies <- liftIO $ IORef.newIORef ([] :: [Text])
        let skillsCfg = SkillsStore.SkillsConfig tmp
            skillDir = tmp </> "demo"
            skillPath = skillDir </> "SKILL.md"
        FileSystem.createDirectory skillDir
        writeTextFile skillPath "---\nname: old-skill\ndescription: old description\n---\n"
        prompt <-
          raise $
            runAdminWithSkills defaultAdminConfig skillsCfg replies (messageWith "!reload" emptyMessageDigest{senderIsSuperuser = True}) do
              writeTextFile skillPath "---\nname: new-skill\ndescription: new description\n---\n"
        liftIO $ IORef.readIORef replies >>= (@?= ["已重新载入 skill 列表。"])
        liftIO $ assertBool "reloaded prompt includes updated skill" ("new-skill" `Text.isInfixOf` prompt)
        liftIO $ assertBool "reloaded prompt drops old skill" (not ("old-skill" `Text.isInfixOf` prompt))

testRestartRejectsNonSuperuser :: IO ()
testRestartRejectsNonSuperuser = do
  replies <- IORef.newIORef ([] :: [Text])
  restarted <- IORef.newIORef False
  queued <- runAdminRestart replies restarted (messageWith "!restart" emptyMessageDigest)
  IORef.readIORef replies >>= (@?= ["只有 superuser 可以重启 cosmobot。"])
  IORef.readIORef restarted >>= (@?= False)
  queued @?= []

testRestartRequestsProcessRestart :: IO ()
testRestartRequestsProcessRestart = do
  replies <- IORef.newIORef ([] :: [Text])
  restarted <- IORef.newIORef False
  queued <- runAdminRestart replies restarted (messageWith "!restart" emptyMessageDigest{senderIsSuperuser = True})
  IORef.readIORef replies >>= (@?= ["正在重启 cosmobot。"])
  IORef.readIORef restarted >>= (@?= True)
  queued @?= ["cosmobot 重启完成啦 (｡•̀ᴗ-)✧"]

runAdminRestart :: IORef.IORef [Text] -> IORef.IORef Bool -> IncomingMessage -> IO [Text]
runAdminRestart replies restarted incoming =
  runEff $ runPrim $ runConcurrent $ runTestLog $
    StorageSQLite.runStorageSQLitePath ":memory:" $
      Chat.runChatWith (testChatDriver replies Nothing False) $
        runFileSystem $ runProcess $ runConcurrent $
          ConcurrencyManager.runConcurrencyManager $
            Skills.runSkills (SkillsStore.SkillsConfig "skills") do
              Lifecycle.runLifecycleEffect recordRestart $
                runHandlers (adminHandlers defaultAdminConfig) incoming
              map (.body) <$> LifecycleStorage.loadStartupActions
  where
    recordRestart incomingMessage body = do
      void $ LifecycleStorage.enqueueStartupReply "test-restart" incomingMessage body
      liftIO $ IORef.writeIORef restarted True

testTitleRejectsNonSuperuser :: IO ()
testTitleRejectsNonSuperuser = do
  replies <- IORef.newIORef ([] :: [Text])
  titleCalls <- IORef.newIORef ([] :: [(Maybe Integer, Text, Text)])
  actions <- runAdminWithTitle defaultAdminConfig replies titleCalls True (groupMessageWith "!title 200 cool" emptyMessageDigest)
  IORef.readIORef replies >>= (@?= ["只有 superuser 可以设置 title。"])
  IORef.readIORef titleCalls >>= (@?= [])
  assertBool "no startup actions queued" (null actions)

testTitleValidatesArguments :: IO ()
testTitleValidatesArguments = do
  replies <- IORef.newIORef ([] :: [Text])
  titleCalls <- IORef.newIORef ([] :: [(Maybe Integer, Text, Text)])
  actions <- runAdminWithTitle defaultAdminConfig replies titleCalls True (groupMessageWith "!title 0 cool" emptyMessageDigest{senderIsSuperuser = True})
  IORef.readIORef replies >>= (@?= ["用法：!title <id> <title>"])
  IORef.readIORef titleCalls >>= (@?= [])
  assertBool "no startup actions queued" (null actions)

testTitleSetsGroupMemberTitle :: IO ()
testTitleSetsGroupMemberTitle = do
  replies <- IORef.newIORef ([] :: [Text])
  titleCalls <- IORef.newIORef ([] :: [(Maybe Integer, Text, Text)])
  actions <- runAdminWithTitle defaultAdminConfig replies titleCalls True (groupMessageWith "!title 200 very cool" emptyMessageDigest{senderIsSuperuser = True})
  IORef.readIORef replies >>= (@?= ["已设置 200 的 title：very cool"])
  IORef.readIORef titleCalls >>= (@?= [(Just 100, "200", "very cool")])
  assertBool "no startup actions queued" (null actions)

testTitleReportsUnsupportedPlatformFailure :: IO ()
testTitleReportsUnsupportedPlatformFailure = do
  replies <- IORef.newIORef ([] :: [Text])
  titleCalls <- IORef.newIORef ([] :: [(Maybe Integer, Text, Text)])
  actions <- runAdminWithTitle defaultAdminConfig replies titleCalls False (groupMessageWith "!title 200 very cool" emptyMessageDigest{senderIsSuperuser = True})
  IORef.readIORef replies >>= (@?= ["设置 title 失败：当前平台可能不支持，或 bot 权限不足。"])
  IORef.readIORef titleCalls >>= (@?= [(Just 100, "200", "very cool")])
  assertBool "no startup actions queued" (null actions)

testUpgradeRejectsNonSuperuser :: IO ()
testUpgradeRejectsNonSuperuser = do
  replies <- IORef.newIORef ([] :: [Text])
  actions <- runAdmin upgradeConfig replies (messageWith "!upgrade" emptyMessageDigest)
  IORef.readIORef replies >>= (@?= ["只有 superuser 可以执行 upgrade。"])
  assertBool "no startup actions queued" (null actions)

testUpgradeReportsScriptExitStatus :: IO ()
testUpgradeReportsScriptExitStatus = do
  replies <- IORef.newIORef ([] :: [Text])
  actions <- runAdminSettled upgradeConfig replies (messageWith "!upgrade" emptyMessageDigest{senderIsSuperuser = True})
  captured <- IORef.readIORef replies
  case captured of
    [started, exited] -> do
      started @?= "已启动 upgrade 脚本：/bin/true"
      exited @?= "upgrade 脚本已退出，exitcode=0。"
    _ ->
      assertFailure [i|unexpected replies: #{show captured :: String}|]
  assertBool "startup action deleted after script return" (null actions)

testUpgradeReportsNonzeroScriptExitStatus :: IO ()
testUpgradeReportsNonzeroScriptExitStatus = do
  replies <- IORef.newIORef ([] :: [Text])
  actions <- runAdminSettled (upgradeConfigFor "/bin/false") replies (messageWith "!upgrade" emptyMessageDigest{senderIsSuperuser = True})
  captured <- IORef.readIORef replies
  case captured of
    [started, exited] -> do
      started @?= "已启动 upgrade 脚本：/bin/false"
      exited @?= "upgrade 脚本已退出，exitcode=1。"
    _ ->
      assertFailure [i|unexpected replies: #{show captured :: String}|]
  assertBool "startup action deleted after script failure" (null actions)

testLifecycleStartupRepliesAreDeletedAfterDrain :: IO ()
testLifecycleStartupRepliesAreDeletedAfterDrain = do
  replies <- IORef.newIORef ([] :: [Text])
  remaining <- runEff $
    runPrim $
    runConcurrent $
    runFileSystem $
    ConcurrencyManager.runConcurrencyManager $
    runTestLog $
      StorageSQLite.runStorageSQLitePath ":memory:" $
        Media.runMediaPassthrough $
          Chat.runChatWith (testChatDriver replies Nothing False) do
            void $ LifecycleStorage.enqueueStartupReply "test-startup-reply" message "cosmobot 重启完成啦 (｡•̀ᴗ-)✧"
            restartRequested <- newIORef False
            Lifecycle.runLifecycle MediaConfig.defaultConfig restartRequested (pure ())
            LifecycleStorage.loadStartupActions
  IORef.readIORef replies >>= (@?= ["cosmobot 重启完成啦 (｡•̀ᴗ-)✧"])
  assertBool "startup actions deleted after drain" (null remaining)

testLifecycleRestartRequestRestartsRuntime :: IO ()
testLifecycleRestartRequestRestartsRuntime = do
  restarted <- IORef.newIORef False
  stopped <- IORef.newIORef False
  replies <- IORef.newIORef []
  runEff $ runPrim $ runConcurrent $ runFileSystem do
    started <- MVar.newEmptyMVar
    ConcurrencyManager.runConcurrencyManager $
      runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" $
        Media.runMediaPassthrough $ Chat.runChatWith (testChatDriver replies Nothing False) $
          Lifecycle.runLifecycle MediaConfig.defaultConfig restarted do
            Concurrency.fire "restart-test.worker" $
              (MVar.putMVar started () >> threadDelay maxBound)
                `finally` liftIO (IORef.writeIORef stopped True)
            MVar.takeMVar started
            LifecycleEffect.requestRestart (messageWith "!restart" emptyMessageDigest) "restart complete"
  IORef.readIORef restarted >>= (@?= True)
  IORef.readIORef stopped >>= (@?= True)

writeTextFile :: FileSystem.FileSystem :> es => FilePath -> Text -> Eff es ()
writeTextFile path text =
  FileSystemByteString.writeFile path (TextEncoding.encodeUtf8 text)

upgradeConfig :: AdminConfig
upgradeConfig =
  upgradeConfigFor "/bin/true"

upgradeConfigFor :: FilePath -> AdminConfig
upgradeConfigFor script =
  defaultAdminConfig
    { upgrade = Just UpgradeConfig{script}
    }

runAdmin :: AdminConfig -> IORef.IORef [Text] -> IncomingMessage -> IO [LifecycleStorage.StoredStartupAction]
runAdmin cfg replies incoming =
  runAdminWithDelay 0 cfg replies incoming

runAdminSettled :: AdminConfig -> IORef.IORef [Text] -> IncomingMessage -> IO [LifecycleStorage.StoredStartupAction]
runAdminSettled cfg replies incoming =
  runAdminWithDelay 100_000 cfg replies incoming

runAdminWithTitle
  :: AdminConfig
  -> IORef.IORef [Text]
  -> IORef.IORef [(Maybe Integer, Text, Text)]
  -> Bool
  -> IncomingMessage
  -> IO [LifecycleStorage.StoredStartupAction]
runAdminWithTitle cfg replies titleCalls titleResult incoming =
  runAdminWithDelayAndTitle 0 cfg replies (Just titleCalls) titleResult incoming

runAdminWithDelay :: Int -> AdminConfig -> IORef.IORef [Text] -> IncomingMessage -> IO [LifecycleStorage.StoredStartupAction]
runAdminWithDelay delayMicros cfg replies incoming =
  runAdminWithDelayAndTitle delayMicros cfg replies Nothing False incoming

runAdminWithSkills
  :: (Concurrent :> es, FileSystem.FileSystem :> es, IOE :> es, Prim :> es)
  => AdminConfig
  -> SkillsStore.SkillsConfig
  -> IORef.IORef [Text]
  -> IncomingMessage
  -> Eff (Skills.Skills : Concurrency.Concurrency : Process : Chat.Chat : Storage.Storage : KatipE : es) ()
  -> Eff es Text
runAdminWithSkills cfg skillsCfg replies incoming beforeReload =
  runTestLog $
    StorageSQLite.runStorageSQLitePath ":memory:" $
      Chat.runChatWith (testChatDriver replies Nothing False) $
        runProcess $
          ConcurrencyManager.runConcurrencyManager $
            Skills.runSkills skillsCfg do
              beforeReload
              Lifecycle.runLifecycleEffect (\_ _ -> pure ()) $
                runHandlers (adminHandlers cfg) incoming
              Skills.skillsSystemPrompt

runAdminWithDelayAndTitle
  :: Int
  -> AdminConfig
  -> IORef.IORef [Text]
  -> Maybe (IORef.IORef [(Maybe Integer, Text, Text)])
  -> Bool
  -> IncomingMessage
  -> IO [LifecycleStorage.StoredStartupAction]
runAdminWithDelayAndTitle delayMicros cfg replies titleCalls titleResult incoming =
  runEff $
    runPrim $
    runConcurrent $
    runTestLog $
      StorageSQLite.runStorageSQLitePath ":memory:" $
        Chat.runChatWith (testChatDriver replies titleCalls titleResult) do
          runAdminStack do
            runHandlers (adminHandlers cfg) incoming
            when (delayMicros > 0) (threadDelay delayMicros)
          LifecycleStorage.loadStartupActions
  where
    runAdminStack =
      runFileSystem
        . runProcess
        . runConcurrent
        . ConcurrencyManager.runConcurrencyManager
        . Skills.runSkills (SkillsStore.SkillsConfig "skills")
        . Lifecycle.runLifecycleEffect (\_ _ -> pure ())

testChatDriver
  :: IOE :> es
  => IORef.IORef [Text]
  -> Maybe (IORef.IORef [(Maybe Integer, Text, Text)])
  -> Bool
  -> AdminChatDriver es
testChatDriver replies titleCalls titleResult =
  AdminChatDriver
    { sendReply = reply
    , setTitle = setMemberTitle
    }
  where
    reply _ body = do
      liftIO $ IORef.modifyIORef' replies (<> [body])
      pure (Right "1")
    setMemberTitle incoming userId title = do
      traverse_ (\ref -> liftIO $ IORef.modifyIORef' ref (<> [(incoming.chatId, userId, title)])) titleCalls
      pure titleResult

runTestLog :: IOE :> es => Eff (KatipE : es) a -> Eff es a
runTestLog action = startKatipE "admin-spec" "test" action

message :: IncomingMessage
message =
  messageWith "!ping" emptyMessageDigest

messageWith :: Text -> MessageDigest -> IncomingMessage
messageWith body digest =
  IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just 100
    , chatAliases = []
    , digest = digest
    , senderId = Just "200"
    , senderUsername = Just "alice"
    , messageId = Just "300"
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , files = []
    , text = body
    , raw = Aeson.Null
    }

groupMessageWith :: Text -> MessageDigest -> IncomingMessage
groupMessageWith body digest =
  (messageWith body digest)
    { platform = PlatformQQ
    , kind = ChatGroup
    , chatId = Just 100
    }
