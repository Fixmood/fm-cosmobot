{-# LANGUAGE TypeApplications #-}
{-|
Module      : Bot.Handler.Admin
Description : Public administrative utility commands
Stability   : experimental
-}

module Bot.Handler.Admin
  ( adminHandlers
  )
where

import Bot.Core.Message
import Bot.Core.Route
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Lifecycle as LifecycleEffect
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Effect.Skills as Skills
import Bot.Handler.Admin.Config
import Bot.Prelude
import qualified Bot.Storage.Lifecycle as Lifecycle
import qualified Bot.Util.Process as ProcessUtil
import qualified Data.Char as Char
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Unique as Unique
import qualified Effectful.Concurrent.STM as STM
import Effectful.FileSystem (FileSystem)
import qualified Effectful.Process.Typed as TypedProcess
import qualified System.Exit as Exit

adminHandlers :: (Chat.Chat :> es, Skills.Skills :> es, LifecycleEffect.Lifecycle :> es, Concurrency.Concurrency :> es, Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Storage.Storage :> es, KatipE :> es, IOE :> es) => AdminConfig -> [RouteHandler es]
adminHandlers cfg =
  [ pingRoute
  , reloadRoute
  , restartRoute
  , titleRoute
  , echoRoute
  , idRoute
  ] <> maybeToList (upgradeRoute <$> cfg.upgrade)

pingRoute :: Chat.Chat :> es => RouteHandler es
pingRoute =
  withHelp (RouteHelp "!ping" "Check whether the bot is responding.") $
    stopOn (command "!ping") handlePing

handlePing :: Chat.Chat :> es => IncomingMessage -> Text -> Eff es ()
handlePing message _ =
  void $ Chat.replyTo message "pong"

reloadRoute :: (Chat.Chat :> es, Skills.Skills :> es) => RouteHandler es
reloadRoute =
  withHelp (RouteHelp "!reload" "Reload skills (superuser only).") $
  requireAuth
    isSuperuser
    (\message -> void $ Chat.replyTo message "只有 superuser 可以 reload。")
    (stopOn (command "!reload") handleReload)

handleReload :: (Chat.Chat :> es, Skills.Skills :> es) => IncomingMessage -> Text -> Eff es ()
handleReload message _ = do
  Skills.reloadSkills
  void $ Chat.replyTo message "已重新载入 skill 列表。"

restartRoute :: (Chat.Chat :> es, LifecycleEffect.Lifecycle :> es) => RouteHandler es
restartRoute =
  withHelp (RouteHelp "!restart" "Restart cosmobot (superuser only).") $
  requireAuth
    isSuperuser
    (\message -> void $ Chat.replyTo message "只有 superuser 可以重启 cosmobot。")
    (stopOn (command "!restart") handleRestart)

handleRestart :: (Chat.Chat :> es, LifecycleEffect.Lifecycle :> es) => IncomingMessage -> Text -> Eff es ()
handleRestart message _ = do
  void $ Chat.replyTo message "正在重启 cosmobot。"
  LifecycleEffect.requestRestart message "cosmobot 重启完成啦 (｡•̀ᴗ-)✧"

echoRoute :: Chat.Chat :> es => RouteHandler es
echoRoute =
  withHelp (RouteHelp "!echo <text>" "Echo text back to the chat.") $
    stopOn (command "!echo") handleEcho

handleEcho :: Chat.Chat :> es => IncomingMessage -> Text -> Eff es ()
handleEcho message rawArgs = do
  void $ Chat.replyTo message rawArgs

titleRoute :: Chat.Chat :> es => RouteHandler es
titleRoute =
  withHelp (RouteHelp "!title <id> <title>" "Set a group member title (superuser only).") $
  requireAuth
    isSuperuser
    (\message -> void $ Chat.replyTo message "只有 superuser 可以设置 title。")
    (stopOn (command "!title") handleTitle)

handleTitle :: Chat.Chat :> es => IncomingMessage -> Text -> Eff es ()
handleTitle message rawArgs =
  case parseTitleArgs rawArgs of
    Nothing ->
      void $ Chat.replyTo message "用法：!title <id> <title>"
    Just (userId, title)
      | message.kind /= ChatGroup ->
          void $ Chat.replyTo message "只能在群聊中设置 title。"
      | isNothing message.chatId ->
          void $ Chat.replyTo message "无法识别当前群聊，不能设置 title。"
      | otherwise -> do
          set <- Chat.setMemberTitle message userId title
          void $ Chat.replyTo message $
            if set
              then [i|已设置 #{userId} 的 title：#{title}|]
              else "设置 title 失败：当前平台可能不支持，或 bot 权限不足。"

parseTitleArgs :: Text -> Maybe (Text, Text)
parseTitleArgs rawArgs = do
  let args = Text.strip rawArgs
      (rawUserId, rawTitle) = Text.break Char.isSpace args
      userId = Text.strip rawUserId
      title = Text.strip rawTitle
  guard (not (Text.null userId) && userId /= "0" && not (Text.null title))
  pure (userId, title)

upgradeRoute :: (Chat.Chat :> es, Concurrency.Concurrency :> es, Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Storage.Storage :> es, KatipE :> es, IOE :> es) => UpgradeConfig -> RouteHandler es
upgradeRoute cfg =
  withHelp (RouteHelp "!upgrade" "Run the configured upgrade script (superuser only).") $
  requireAuth
    isSuperuser
    (\message -> void $ Chat.replyTo message "只有 superuser 可以执行 upgrade。")
    (stopOn (command "!upgrade") \message _ -> handleUpgrade cfg message)

handleUpgrade :: (Chat.Chat :> es, Concurrency.Concurrency :> es, Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Storage.Storage :> es, KatipE :> es, IOE :> es) => UpgradeConfig -> IncomingMessage -> Eff es ()
handleUpgrade cfg message = do
  let scriptPath = cfg.script
  actionKey <- liftIO newLifecycleActionKey
  startupAction <- Lifecycle.enqueueStartupReply actionKey message "cosmobot 回来啦 (｡•̀ᴗ-)✧"
  result <- trySync (startUpgradeScript scriptPath)
  case result of
    Right running -> do
      void $ Chat.replyTo message [i|已启动 upgrade 脚本：#{Text.pack scriptPath}|]
      Concurrency.fire "admin.upgrade" (reportUpgradeScriptExit startupAction message running)
    Left err -> do
      Lifecycle.deleteStartupAction startupAction
      void $ Chat.replyTo message [i|upgrade 脚本启动失败：#{show err :: String}|]

data RunningUpgradeScript = RunningUpgradeScript
  { process :: !(TypedProcess.Process () (STM.STM LazyByteString.ByteString) (STM.STM LazyByteString.ByteString))
  }

newLifecycleActionKey :: IO Text
newLifecycleActionKey = do
  unique <- Unique.newUnique
  pure [i|upgrade-#{Unique.hashUnique unique}|]

startUpgradeScript :: TypedProcess.TypedProcess :> es => FilePath -> Eff es RunningUpgradeScript
startUpgradeScript scriptPath = do
  process <- TypedProcess.startProcess $
    TypedProcess.setStdin TypedProcess.closed .
    TypedProcess.setStdout TypedProcess.byteStringOutput .
    TypedProcess.setStderr TypedProcess.byteStringOutput $
    TypedProcess.proc scriptPath []
  pure RunningUpgradeScript{process}

reportUpgradeScriptExit
  :: (Chat.Chat :> es, Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Storage.Storage :> es, KatipE :> es, IOE :> es)
  => Lifecycle.StoredStartupAction
  -> IncomingMessage
  -> RunningUpgradeScript
  -> Eff es ()
reportUpgradeScriptExit startupAction message running = do
  (exitCode, stdoutText, stderrText) <- waitUpgradeScript running
  Lifecycle.deleteStartupAction startupAction
  void $ Chat.replyTo message (scriptExited exitCode stdoutText stderrText)

waitUpgradeScript :: (Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es) => RunningUpgradeScript -> Eff es (Exit.ExitCode, Text, Text)
waitUpgradeScript RunningUpgradeScript{process} = do
  exitCode <- TypedProcess.waitExitCode process
  stdoutText <- ProcessUtil.processOutputText (TypedProcess.getStdout process)
  stderrText <- ProcessUtil.processOutputText (TypedProcess.getStderr process)
  pure (exitCode, stdoutText, stderrText)

scriptExited :: Exit.ExitCode -> Text -> Text -> Text
scriptExited exitCode stdoutText stderrText =
  Text.strip [i|upgrade 脚本已退出，exitcode=#{exitCodeText exitCode}。#{scriptOutput stdoutText stderrText}|]

exitCodeText :: Exit.ExitCode -> Text
exitCodeText = \case
  Exit.ExitSuccess ->
    "0"
  Exit.ExitFailure code ->
    show code

scriptOutput :: Text -> Text -> Text
scriptOutput stdoutText stderrText =
  let sections =
        [ ("stdout", Text.strip stdoutText)
        , ("stderr", Text.strip stderrText)
        ]
  in Text.unlines
      [ name <> ":\n" <> Text.take 3000 body
      | (name, body) <- sections
      , not (Text.null body)
      ]

idRoute :: Chat.Chat :> es => RouteHandler es
idRoute =
  withHelp (RouteHelp "!id" "Find sender ID and chat ID.") $
    stopOn (command "!id") handleId

handleId :: Chat.Chat :> es => IncomingMessage -> Text -> Eff es ()
handleId message _ = do
  let result = "- Chat ID: " <> show message.chatId <> "\n"
        <> "- Sender ID: " <> show message.senderId
  void $ Chat.replyTo message result
