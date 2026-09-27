{-|
Module      : Bot.Agent.Middleware.Typing
Description : Agent typing notification middleware
Stability   : experimental
-}

module Bot.Agent.Middleware.Typing
  ( withTypingNotification
  )
where

import Bot.Agent.Core
import Bot.Agent.Types (Context (..))
import Bot.Core.Message (IncomingMessage (..))
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Concurrency as Concurrency
import Bot.Prelude
import qualified Bot.Util.Stream as StreamUtil
import qualified Streaming as S

withTypingNotification
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, KatipE :> es)
  => Runtime context (Eff es)
  -> Runtime context (Eff es)
withTypingNotification program =
  program
    { aroundAgentRun = \context action ->
        withTypingScope message (program.aroundAgentRun context action)
    }
  where
    message =
      program.context.message

withTypingScope
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, KatipE :> es)
  => IncomingMessage
  -> Stream (Of Output) (Eff es) Result
  -> Stream (Of Output) (Eff es) Result
withTypingScope message stream = do
  S.lift (safeSetTyping message typingNotificationTimeoutMillis)
  StreamUtil.bracketStream
    (Concurrency.fork "agent.typing" (typingNotificationLoop message))
    cancelAndAwaitTyping
    \_ -> stream

cancelAndAwaitTyping :: Concurrency.Concurrency :> es => Concurrency.Handle -> Eff es ()
cancelAndAwaitTyping typingHandle = do
  void (Concurrency.cancel typingHandle.handleId)
  Concurrency.await typingHandle

typingNotificationLoop
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, KatipE :> es)
  => IncomingMessage
  -> Eff es ()
typingNotificationLoop message = go 0
  where
    go elapsedMicros = do
      Concurrency.sleepMicroseconds typingNotificationRefreshMicroseconds
      let next = elapsedMicros + typingNotificationRefreshMicroseconds
      if next >= typingNotificationMaxLifetimeMicroseconds
        then
          logInfo
            [i|Typing refresh stopped after #{typingNotificationMaxLifetimeMicroseconds `div` 1000000}s (run still in flight); the indicator will expire on its own. platform=#{message.platform} chat_id=#{message.chatId}|]
        else do
          safeSetTyping message typingNotificationTimeoutMillis
          go next

safeSetTyping
  :: (Chat.Chat :> es, KatipE :> es)
  => IncomingMessage
  -> Int
  -> Eff es ()
safeSetTyping message timeoutMillis =
  Chat.setTyping message timeoutMillis
    `catchSync` \err -> do
      let platform = message.platform
          chatId = message.chatId
          chatAliases = message.chatAliases
      logWarning [i|Typing notification failed: platform=#{platform} chat_id=#{chatId} chat_aliases=#{chatAliases} error=#{displayException err}|]

typingNotificationTimeoutMillis :: Int
typingNotificationTimeoutMillis =
  30000

typingNotificationRefreshMicroseconds :: Int
typingNotificationRefreshMicroseconds =
  20 * 1000000

-- 这条循环**最多活这么久**，到点就自己停（停的只是「刷新」，不是对话）。
--
-- 为什么要加上限：它原本是无限递归，唯一的正常停止方式是 withTypingScope 里
-- bracketStream 的释放钩子 cancelAndAwaitTyping。那只在**所属 agent run 正常收尾**时
-- 才会执行 —— 一旦 run 卡死（例如图片请求永不返回），钩子永不触发，
-- typing 就会每 20 秒重新点亮一次，**永远亮下去**。
-- 实测 2026-09-27：4 个 run 卡在 image_generate / image_edit 上，其中两个从
-- 09-25 05:05 起连续亮了 2 天，对端 Matrix 一直显示「fm 正在输入…」。
--
-- 30 分钟：正常的长任务（多轮工具调用）够用；真挂死时最多 30 分钟后停止刷新，
-- 指示器随即自然过期。到达上限只记一条 Info，不打断 run。
typingNotificationMaxLifetimeMicroseconds :: Int
typingNotificationMaxLifetimeMicroseconds =
  30 * 60 * 1000000
