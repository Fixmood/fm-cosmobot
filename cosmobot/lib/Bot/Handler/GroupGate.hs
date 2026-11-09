{-|
Module      : Bot.Handler.GroupGate
Description : Let FM join a group conversation without being addressed
Stability   : experimental

The gate sits between chat-log recording and route dispatch. A group message
that nobody aimed at FM is normally ignored; here the gate may treat it as
addressed and answer. It only rewrites the message into the existing
"addressed" shape, so the rest of the pipeline (agent, tools, persona,
splitting, stickers) runs unchanged.

Only groups with humanlike mode enabled are gated. Every rule is derived from
the chat log — how long since FM last spoke, how many turns in a row it took,
how many in the past hour — so there is no extra state and a restart cannot
lose the budget.

Phase 1: rules plus a small probability, no model call, so it cannot add
latency to the incoming stream.
-}

module Bot.Handler.GroupGate
  ( applyGroupGate
  , loadGateGroups
  , gateHourlyLimit
  , gateMaxConsecutive
  , gateCooldownSeconds
  , gateInterjectPercent
  ) where

import Bot.Core.Message
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import Data.Time (addUTCTime, diffUTCTime, getCurrentTime)
import qualified Streaming.Prelude as S
import Text.Read (readMaybe)

gateHourlyLimit :: Int
gateHourlyLimit = 6

gateMaxConsecutive :: Int
gateMaxConsecutive = 3

gateCooldownSeconds :: Int
gateCooldownSeconds = 40

gateInterjectPercent :: Int
gateInterjectPercent = 4

gateMinChars :: Int
gateMinChars = 4

-- | 接话时最多附带几张最近的图片。
gateMaxImages :: Int
gateMaxImages = 3

-- | 续聊窗口：FM 最后一条回复之后这段时间内，同一个人接着说不必再触发。
gateFollowupWindowSeconds :: Int
gateFollowupWindowSeconds = 120

-- | 窗内最多连续几条（含当前这条）仍然算续聊，超出就回到普通门控。
gateFollowupMaxTurns :: Int
gateFollowupMaxTurns = 3

gateScopesPath :: FilePath
gateScopesPath = "humanlike/scopes.txt"

-- | 需要门控的群（从拟人模式会话清单里筛出群）。
loadGateGroups :: (IOE :> es, FileSystem.FileSystem :> es) => Eff es [Integer]
loadGateGroups = do
  exists <- FileSystem.doesFileExist gateScopesPath
  if not exists
    then pure []
    else do
      raw <- FileSystemByteString.readFile gateScopesPath
      pure (mapMaybe parse (Text.lines (TextEncoding.decodeUtf8 raw)))
  where
    parse line = do
      rest <- Text.stripPrefix "qq:group:" (Text.strip line)
      readMaybe (Text.unpack rest)

-- | 把符合条件的群消息改写成"被点名"的形式。
applyGroupGate
  :: (ChatLog.ChatLog :> es, IOE :> es, KatipE :> es, FileSystem.FileSystem :> es, LLM.LLM :> es)
  => Stream (Of IncomingMessage) (Eff es) ()
  -> Stream (Of IncomingMessage) (Eff es) ()
applyGroupGate = S.mapMaybeM consider
  where
    consider message
      | message.eventKind /= IncomingMessageCreated = pure (Just message)
      | message.kind /= ChatGroup = pure (Just message)
      | otherwise = do
          -- 每次现读：刚在某个群开启拟人模式时，不必重启也能立刻生效。
          groups <- loadGateGroups
          if maybe False (`elem` groups) message.chatId
            then gated message
            else pure (Just message)

    -- 只有"没被点名、也不是在叫 FM"的消息才需要门控判断。
    -- 短消息也不能直接跳过：续聊里的「谁呀」「然后呢」就是正常对话，跳过就断线了。
    gated message
      | message.digest.mentionsBot || startsWithFm message.text = pure (Just message)
    gated message = do
          now <- liftIO getCurrentTime
          recent <- ChatLog.queryChat message Nothing 40 True
            (ChatLog.ChatLogTimeRange (Just (addUTCTime (-3600) now)) Nothing)
          let botTurns = filter (.isBot) recent
              -- 续聊窗口（followup window）：FM 最后一条回复之后，如果只有这同一个人
              -- 在说话、且距今不远，那就是"话还没说完"，不必再触发。
              -- 各层返回顺序不保证；先按时间排好再判断，否则"最后一条是谁说的"
              -- 会完全判错（实测续聊窗口因此一直不生效）。
              ordered = sortOn (\entry -> entry.recordedAt) recent
              newestFirst = reverse ordered
              -- 多人场景：不去要求"FM 之后只有同一个人在说"（别人插一句就断了），
              -- 而是判断"FM 最后那条回复，回的是谁"——那条消息之前最近的非 bot
              -- 发言者就是它的对话对象。别人的插话不影响这个判断。
              olderThanLastBot = drop 1 (dropWhile (\entry -> not entry.isBot) newestFirst)
              lastBotEntry = viaNonEmpty head (filter (.isBot) newestFirst)
              targetSender = listToMaybe [entry.senderId | entry <- olderThanLastBot, not entry.isBot]
              sameTarget = case (targetSender, message.senderId) of
                (Just (Just left), Just right) -> left == right
                _ -> False
              continuation = case lastBotEntry >>= (.recordedAt) of
                Just at -> diffUTCTime now at <= fromIntegral gateFollowupWindowSeconds && sameTarget
                Nothing -> False
              lastBotAt = listToMaybe (mapMaybe (.recordedAt) (reverse botTurns))
              cooldownOk = case lastBotAt of
                Nothing -> True
                Just at -> diffUTCTime now at >= fromIntegral gateCooldownSeconds
              hourlyOk = length botTurns < gateHourlyLimit
              consecutive = length (takeWhile (\entry -> entry.isBot) newestFirst)
              consecutiveOk = consecutive < gateMaxConsecutive
              seed = Text.pack (show (fromMaybe 0 message.chatId)) <> message.text
              roll :: Int
              roll = Text.foldl' (\acc char -> (acc * 31 + fromEnum char) `mod` 100) 7 seed
              probabilityOk = roll < gateInterjectPercent
          -- 续聊候选才问模型；普通插话只走概率，省一次调用。
          approved <- if continuation then judgeContinue message ordered else pure False
          let skipReason :: Maybe Text
              skipReason
                -- 续聊时交给模型自己判断该不该接：话题已经结束、在跟别人说话、
                -- 或者只是语气词，它都可以选择不接，而不是被规则硬顶着回。
                | approved = Nothing
                | continuation = Just "judge_no"
                | not hourlyOk = Just "hourly_limit"
                | not cooldownOk = Just "cooldown"
                | not consecutiveOk = Just "consecutive"
                | not probabilityOk = Just [i|roll=#{roll}|]
                | otherwise = Nothing
          case skipReason of
            Nothing -> do
              -- 接话时把最近两分钟内别人发的图一起挂上：只把当前这条消息的图
              -- 交给模型，它就看不懂"刚才那张图"指的是什么。
              let recentImages = take gateMaxImages
                    (concatMap (.imageUrls)
                      (filter (\entry -> maybe False (\at -> diffUTCTime now at <= 120) entry.recordedAt) recent))
                  attached = take gateMaxImages (message.imageUrls <> recentImages)
              logInfo [i|group gate: JOIN #{message.chatId} followup=#{continuation} judged=#{approved} images=#{length attached} #{Text.take 30 (Text.unwords (Text.words message.text))}|]
              pure (Just message
                { digest = message.digest{mentionsBot = True}
                , imageUrls = attached
                })
            Just reason -> do
              logInfo [i|group gate: skip #{message.chatId} reason=#{reason} followup=#{continuation}|]
              pure Nothing

    isCandidate groups message =
      message.kind == ChatGroup
        && maybe False (`elem` groups) message.chatId
        && not message.digest.mentionsBot
        && not message.digest.senderIsSuperuser
        && (Text.length (Text.strip message.text) >= gateMinChars || not (null message.imageUrls))
        && not (startsWithFm message.text)

startsWithFm :: Text -> Bool
startsWithFm text =
  let compact = Text.toCaseFold (Text.filter (not . (`elem` [' ', '\t', '\x3000'])) text)
  in "fm" `Text.isPrefixOf` compact

-- | 让模型判断"这一条还要不要接"。
--
-- 规则只能看出"你在跟我说话"，看不出"话题已经完了"或者"这句其实是在回别人"。
-- 所以续聊候选交给模型自己判：YES 才接，NO 就安静（并记日志，便于调参）。
judgeContinue :: LLM.LLM :> es => IncomingMessage -> [ChatLog.ChatLogEntry] -> Eff es Bool
judgeContinue message recent = do
  let history = Text.unlines
        [ (if entry.isBot then "FM: " else "对方: ") <> Text.take 60 (Text.unwords (Text.words entry.text))
        | entry <- takeLast 8 recent
        , not (Text.null (Text.strip entry.text))
        ]
  answer <- LLM.ask (Text.unlines
    [ "下面是群里的最近对话，最后一行是刚收到的一条消息。"
    , ""
    , history
    , ""
    , "刚收到：" <> Text.take 80 (Text.unwords (Text.words message.text))
    , ""
    , "你是群里的 FM。要不要接这一条？只回答 YES 或 NO，不要解释。"
    , "回答 NO 的情况：话题已经结束；这句明显是在跟别人说话；只是无意义的语气词；你刚说完不需要再补。"
    ])
  let normalized = Text.toCaseFold (Text.strip answer)
  pure ("yes" `Text.isPrefixOf` normalized || "是" `Text.isPrefixOf` answer)

takeLast :: Int -> [a] -> [a]
takeLast count values = drop (max 0 (length values - count)) values
