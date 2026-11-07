{-|
Module      : Bot.Handler.Humanlike.Archive
Description : Durable relationship record for conversations that use the mode
Stability   : experimental

The rolling chat background only covers a few hours, so a role or a nickname
agreed during a humanlike session would fade. This module keeps a small,
durable record per conversation instead:

@
memory/qq/humanlike/<scope>.md           -- injected every turn, capped
memory/qq/humanlike_archive/<scope>.md   -- records pushed out, never injected
@

The record is a memory of the relationship, not of a speaking style: it is
injected in both modes, and only the style instructions differ. That is what
"two speaking styles, one shared memory" means in practice.

Layout of the injected file. Everything outside the AUTO markers belongs to the
owner and is never touched by the model:

@
## 我手写（模型不覆盖）
...

<!-- AUTO:BEGIN -->
## 长期要点
- ...

## 最近记录
- 2026-11-07: ...
<!-- AUTO:END -->
@

Retention is intentionally boring: the newest N day records stay injected,
older ones move to the archive file, and (only when something is pushed out) an
extraction-only call folds the pushed-out facts into 长期要点. Nothing is ever
deleted, and a failed call leaves the previous text untouched.
-}

module Bot.Handler.Humanlike.Archive
  ( humanlikeRecordScope
  , humanlikeArchiveScope
  , loadHumanlikeRecordText
  , mergeAutoRegion
  , recordHumanlikeSession
  , autoBlockBegin
  , autoBlockEnd
  )
where

import Bot.Core.Message
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Handler.Humanlike as Humanlike
import Bot.Prelude
import qualified Bot.Memory as MemoryStore
import qualified Data.Text as Text
import Data.Time (UTCTime, defaultTimeLocale, formatTime, utcToLocalTime, hoursToTimeZone)

autoBlockBegin :: Text
autoBlockBegin = "<!-- AUTO:BEGIN -->"

autoBlockEnd :: Text
autoBlockEnd = "<!-- AUTO:END -->"

handWrittenHeader :: Text
handWrittenHeader = "## 我手写（模型不覆盖）"

longTermHeader :: Text
longTermHeader = "## 长期要点"

recentHeader :: Text
recentHeader = "## 最近记录"

emptyFile :: Text
emptyFile =
  Text.unlines
    [ "# 拟人档案"
    , ""
    , handWrittenHeader
    , ""
    , autoBlockBegin
    , longTermHeader
    , ""
    , recentHeader
    , ""
    , autoBlockEnd
    ]

-- | Scope id used for the record files, e.g. @private-2822751355@.
humanlikeRecordKey :: IncomingMessage -> Maybe (ChatPlatform, Text)
humanlikeRecordKey message =
  case (message.kind, message.chatId, message.senderId) of
    (ChatGroup, Just chatId, _) -> Just (message.platform, [i|group-#{chatId}|])
    (ChatPrivate, _, Just sender) -> Just (message.platform, [i|private-#{sender}|])
    _ -> Nothing

humanlikeRecordScope :: IncomingMessage -> Maybe MemoryStore.MemoryScope
humanlikeRecordScope message =
  uncurry MemoryStore.HumanlikeMemory <$> humanlikeRecordKey message

humanlikeArchiveScope :: IncomingMessage -> Maybe MemoryStore.MemoryScope
humanlikeArchiveScope message =
  uncurry MemoryStore.HumanlikeArchiveMemory <$> humanlikeRecordKey message

-- | The text to inject, or empty when this conversation has no record yet.
loadHumanlikeRecordText :: Memory.Memory :> es => IncomingMessage -> Eff es Text
loadHumanlikeRecordText message =
  case humanlikeRecordScope message of
    Nothing -> pure ""
    Just scope -> fromMaybe "" <$> Memory.loadMemory scope

-- | One dated record: the day it belongs to plus its text.
type DayRecord = (Text, Text)

data RecordParts = RecordParts
  { beforeAuto :: !Text
  , longTerm :: ![Text]
  , recent :: ![DayRecord]
  , afterAuto :: !Text
  }

-- | Split the file into the owner's part and the maintained part.
splitRecord :: Text -> RecordParts
splitRecord raw =
  case Text.breakOn autoBlockBegin raw of
    (before, rest)
      | Text.null rest ->
          RecordParts{ beforeAuto = Text.stripEnd raw, longTerm = [], recent = [], afterAuto = "" }
      | otherwise ->
          let inner = Text.drop (Text.length autoBlockBegin) rest
              (block, tailPart) = Text.breakOn autoBlockEnd inner
              (term, rev) = parseAutoBlock block
          in RecordParts
              { beforeAuto = Text.stripEnd before
              , longTerm = term
              , recent = rev
              , afterAuto = Text.strip tailPart
              }

parseAutoBlock :: Text -> ([Text], [DayRecord])
parseAutoBlock block =
  let (termSection, rest) = Text.breakOn recentHeader block
      termLines = bulletLines termSection
      recentLines = mapMaybe parseDayRecord (bulletLines rest)
  in (termLines, recentLines)
  where
    bulletLines =
      filter (not . Text.null)
        . map (Text.strip . Text.drop 1 . Text.strip)
        . filter (Text.isPrefixOf "- " . Text.strip)
        . Text.lines
    parseDayRecord line =
      case Text.breakOn ": " line of
        (day, rest)
          | Text.null rest -> Nothing
          | otherwise -> Just (Text.strip day, Text.strip (Text.drop 2 rest))

renderRecord :: RecordParts -> Text
renderRecord parts =
  Text.unlines $
    filter (not . Text.null)
      [ parts.beforeAuto
      , if Text.null parts.beforeAuto then "" else ""
      , autoBlockBegin
      , longTermHeader
      , Text.unlines (map (\line -> "- " <> line) parts.longTerm)
      , ""
      , recentHeader
      , Text.unlines [ "- " <> day <> ": " <> body | (day, body) <- parts.recent ]
      , autoBlockEnd
      , parts.afterAuto
      ]

-- | Insert one day's record, returning the rewritten file and the records that
-- were pushed out by the retention limit.
mergeAutoRegion
  :: Int
  -- ^ How many day records stay injected.
  -> Text
  -- ^ Existing file (may be empty).
  -> DayRecord
  -- ^ The record to add or replace.
  -> (Text, [DayRecord])
mergeAutoRegion maxDays existing incoming =
  let base = if Text.null (Text.strip existing) then emptyFile else existing
      parts = splitRecord base
      -- Same day replaces its own record so repeated toggling does not fill
      -- the budget with fragments of one day.
      combined = incoming : filter ((/= fst incoming) . fst) parts.recent
      ordered = sortOn fst combined
      keep = max 1 maxDays
      overflowCount = max 0 (length ordered - keep)
      dropped = take overflowCount ordered
      kept = drop overflowCount ordered
  in (renderRecord parts{ recent = kept }, dropped)

-- | Append pushed-out records to the archive file, newest last.
appendToArchive :: Memory.Memory :> es => IncomingMessage -> [DayRecord] -> Eff es ()
appendToArchive message dropped =
  case humanlikeArchiveScope message of
    Nothing -> pure ()
    Just scope
      | null dropped -> pure ()
      | otherwise -> do
          existing <- fromMaybe "" <$> Memory.loadMemory scope
          let added = Text.unlines [ "- " <> day <> ": " <> body | (day, body) <- dropped ]
              updated = Text.stripEnd existing <> "\n" <> Text.strip added <> "\n"
          Memory.replaceMemory scope (Text.strip updated <> "\n")

-- | The summary prompt. Deliberately narrow: roles, nicknames, promises, loose
-- ends. Never the conversation itself, because the record is injected forever.
summarySystemPrompt :: Text
summarySystemPrompt =
  Text.unlines
    [ "你在读一段聊天记录。把它压缩成**一行**不超过 100 字的中文备注，只写这些："
    , "- 双方现在怎么称呼彼此"
    , "- 定下的角色、关系、玩笑梗"
    , "- 明确答应过的事，或者没说完的话题"
    , ""
    , "不要写具体问答内容、不要评价、不要编造记录里没有的东西。"
    , "如果这段对话里没有上述任何一类内容，只输出：无"
    , "只输出这一行，不要加前缀、序号或标点说明。"
    ]

-- | Summarise the span and fold it into the record. Never throws: a failure
-- leaves the previous record untouched.
recordHumanlikeSession
  :: ( ChatLog.ChatLog :> es
     , LLM.LLM :> es
     , Memory.Memory :> es
     , KatipE :> es
     , IOE :> es
     )
  => Int
  -- ^ How many day records stay injected.
  -> IncomingMessage
  -> UTCTime
  -- ^ Start of the span.
  -> UTCTime
  -- ^ End of the span.
  -> Eff es ()
recordHumanlikeSession retentionDays message startedAt endedAt =
  case (humanlikeRecordScope message, humanlikeRecordKey message) of
    (Just scope, Just _) -> handle scope `catchSync` \err ->
      logWarning [i|humanlike record failed: #{show err :: String}|]
    _ -> pure ()
  where
    handle scope = do
      entries <- ChatLog.queryChat message Nothing 200 True (ChatLog.ChatLogTimeRange (Just startedAt) (Just endedAt))
      let usable = filter (not . Text.null . Text.strip . (.text)) entries
      when (length usable >= 2) do
        let transcript = Text.intercalate "\n" (map renderEntry usable)
        answer <- LLM.ask (summarySystemPrompt <> "\n\n" <> Text.take 6000 transcript)
        let summary = Text.take 120 (Text.strip (Text.unlines (take 1 (Text.lines answer))) )
        when (not (Text.null summary) && summary /= "无") do
          existing <- fromMaybe "" <$> Memory.loadMemory scope
          let day = Text.pack (formatTime defaultTimeLocale "%F" (utcToLocalTime (hoursToTimeZone 8) endedAt))
              (updated, dropped) = mergeAutoRegion retentionDays existing (day, summary)
          appendToArchive message dropped
          Memory.replaceMemory scope updated
          logInfo [i|humanlike record updated: day=#{day} dropped=#{length dropped}|]
    renderEntry entry =
      let speaker = if entry.isBot then "FM" else fromMaybe (fromMaybe "对方" entry.senderId) entry.senderUsername
      in "[" <> speaker <> "]: " <> Text.take 200 (Text.unwords (Text.words entry.text))
