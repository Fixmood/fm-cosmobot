{-# LANGUAGE OverloadedLabels #-}
{-|
Module      : Bot.Handler.Status
Description : Answer "fm 干到哪了" instantly, without going through the model
Stability   : experimental
-}

module Bot.Handler.Status
  ( statusHandlers
  )
where

import Bot.Core.Message
import Bot.Core.Route
import Bot.Prelude
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Util.Process as ProcessUtil
import qualified Data.Text as Text
import qualified Effectful.Process.Typed as TypedProcess
import qualified Effectful.Timeout as Timeout
import Effectful.Timeout (Timeout)
import System.Exit (ExitCode (..))

-- | 读运行审计、印一行进度的小脚本。放在运行卷上，改它不用重新编译。
fmStatusScript :: FilePath
fmStatusScript = "/data/fm-status.py"

-- | 认哪些问法。**必须带 `fm ` 前缀、而且整句相等** —— 这是所有者 2026-10-03 定的：
-- 用 fm 的触发方式问，就不会在群里被别人闲聊的「你在干嘛」误触发。
fmStatusPhrases :: [Text]
fmStatusPhrases =
  [ "在干嘛"
  , "在干什么"
  , "干到哪了"
  , "干到哪"
  , "进度"
  , "还在吗"
  , "在忙吗"
  , "忙完了吗"
  , "干完了吗"
  ]

fmStatusTimeoutMicros :: Int
fmStatusTimeoutMicros = 10 * 1000000

-- | `fm 干到哪了` —— 立刻回答，**不进模型**。
--
-- 为什么必须绕开模型：问进度这件事本身如果走 agent，会**排在正在跑的那次运行后面** ——
-- 你越问它越慢。这条 route 直接读审计回话，不受那个队列影响。
statusHandlers
  :: ( Chat.Chat :> es
     , Timeout :> es
     , Concurrent :> es
     , IOE :> es
     , TypedProcess.TypedProcess :> es
     )
  => [RouteHandler es]
statusHandlers =
  [statusRoute]

statusRoute
  :: ( Chat.Chat :> es
     , Timeout :> es
     , Concurrent :> es
     , IOE :> es
     , TypedProcess.TypedProcess :> es
     )
  => RouteHandler es
statusRoute =
  withHelp
    (RouteHelp "fm 干到哪了" "用 fm 前缀问进度（在干嘛 / 干到哪了 / 进度 / 还在吗），立刻回它在不在干。")
    $ stopOn statusFilter \message _ -> do
      result <- Timeout.timeout fmStatusTimeoutMicros $
        ProcessUtil.readProcessGroupWithExitCode "python3" [fmStatusScript]
      let body = case result of
            Nothing -> "查进度超时了。"
            Just (ExitSuccess, out, _) ->
              let trimmed = Text.strip out
               in if Text.null trimmed then "查不到进度。" else trimmed
            Just (_, out, err) ->
              let trimmed = Text.strip (if Text.null (Text.strip out) then err else out)
               in if Text.null trimmed then "查进度失败了。" else trimmed
      -- 不带引用：这是对一句话的即时回执，引用反而挡视线（和 *fix 一致）。
      void $ Chat.replyTo message{messageId = Nothing} body

-- | 匹配 `fm <问法>`。整句相等，不做包含匹配 —— 免得群里闲聊被截。
statusFilter :: MessageFilter ()
statusFilter =
  MessageFilter \message -> do
    let stripped = Text.strip message.text
    afterPrefix <- Text.stripPrefix "fm" stripped
    let rest = Text.strip afterPrefix
    guard (rest `elem` fmStatusPhrases)
    pure ()
