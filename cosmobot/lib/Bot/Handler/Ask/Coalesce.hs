{-| 合并窗口：把短时间内的多条触发消息并成一次提问。

2026-11-26 所有者：原来每条消息各自开一次 agent 回合，于是「同一个人补充一句」
「几个人同时提要求」都会被逐条重复回答。这里给每个会话加一个很短的缓冲窗口：
窗口内到达的触发消息先攒起来，窗口结束时合并成一次提问，只回答一次。
-}
module Bot.Handler.Ask.Coalesce
  ( coalesceKey
  , mergeWindowMessages
  , enqueueCoalesced
  ) where

import Bot.Core.Message
import Bot.Prelude hiding (atomicModifyIORef', newIORef, readIORef)
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import System.IO.Unsafe (unsafePerformIO)

-- | 每个会话的待合并消息（新消息在前）与「是否已挂定时器」标记。
data CoalesceState = CoalesceState
  { pendingMessages :: IORef (Map.Map Text [IncomingMessage])
  , scheduledKeys :: IORef (Set.Set Text)
  }

{-# NOINLINE coalesceState #-}
coalesceState :: CoalesceState
coalesceState = unsafePerformIO do
  pending <- newIORef Map.empty
  scheduled <- newIORef Set.empty
  pure CoalesceState{pendingMessages = pending, scheduledKeys = scheduled}

-- | 会话键：平台 + 会话 id（私聊没有会话 id 时退回发送者）。
coalesceKey :: IncomingMessage -> Text
coalesceKey message =
  Text.pack (show message.platform)
    <> ":"
    <> maybe ("sender:" <> fromMaybe "-" message.senderId) (Text.pack . show) message.chatId

-- | 把一条消息并入会话缓冲。窗口内第一条消息负责挂定时器，到点合并并调用 runBatch。
enqueueCoalesced
  :: (Concurrency.Concurrency :> es, IOE :> es, KatipE :> es)
  => Int
  -- ^ 窗口毫秒数
  -> Int
  -- ^ 一次最多合并多少条
  -> Text
  -- ^ 会话键
  -> IncomingMessage
  -> ([IncomingMessage] -> Eff es ())
  -- ^ 窗口结束后的执行动作（批量消息，按到达顺序）
  -> Eff es ()
enqueueCoalesced windowMs maxMessages key message runBatch = do
  size <- liftIO do
    atomicModifyIORef' coalesceState.pendingMessages \current ->
      let updated = Map.insertWith (++) key [message] current
      in (updated, length (Map.findWithDefault [] key updated))
  firstInWindow <- liftIO do
    atomicModifyIORef' coalesceState.scheduledKeys \scheduled ->
      if Set.member key scheduled
        then (scheduled, False)
        else (Set.insert key scheduled, True)
  logInfo [i|FM coalesce: buffered key=#{key} count=#{size}|]
  when firstInWindow do
    void $ Concurrency.fork [i|ask.coalesce|] do
      Concurrency.sleepMicroseconds (max 0 windowMs * 1000)
      flushCoalesced key runBatch
  when (size >= maxMessages) do
    flushCoalesced key runBatch

flushCoalesced
  :: (IOE :> es, KatipE :> es)
  => Text
  -> ([IncomingMessage] -> Eff es ())
  -> Eff es ()
flushCoalesced key runBatch = do
  batch <- liftIO do
    atomicModifyIORef' coalesceState.pendingMessages \current ->
      (Map.delete key current, maybe [] List.reverse (Map.lookup key current))
  liftIO do
    atomicModifyIORef' coalesceState.scheduledKeys \scheduled ->
      (Set.delete key scheduled, ())
  unless (null batch) do
    logInfo [i|FM coalesce: flush key=#{key} count=#{length batch}|]
    runBatch batch

-- | 把同一窗口里的多条消息并成一条「提问」，正文带说话人，附图/附件合并。
mergeWindowMessages :: [IncomingMessage] -> IncomingMessage
mergeWindowMessages batch =
  case List.reverse batch of
    [] -> error "mergeWindowMessages: empty batch"
    base : _ ->
      let described = filter (not . Text.null) (map describe batch)
          mergedText =
            Text.unlines
              ( "（同一次提问：下面几条是同一个短窗口里收到的消息，"
                  <> "请合并成一次回答，括号里是说话人）"
                  : described
              )
          images = List.nub (concatMap (.imageUrls) batch)
          files = concatMap (.files) batch
      in base{text = mergedText, imageUrls = images, files = files}
  where
    describe message =
      let who = fromMaybe "?" message.senderUsername
          body = Text.strip message.text
          marker
            | not (null message.files) = "（附件）"
            | not (null message.imageUrls) = "（附图）"
            | otherwise = ""
      in if Text.null body && Text.null marker
           then ""
           else "「" <> who <> "」：" <> body <> marker
