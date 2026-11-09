{-|
Module      : Bot.Handler.Humanlike.Proactive
Description : Proactive speaking for conversations in humanlike mode
Stability   : experimental

Design follows what shipped bots converged on, not a plain timer:

* Mai_love's scheduler only *reminds* the planner that it may speak, and the
  planner decides whether to say anything. A timer that always fires is what
  users mute.
* The same project gives the character a day of its own (lying in, cooking,
  napping). That is what makes a proactive line and a "what are you doing?"
  answer agree with each other.
* Quiet hours, a per-day budget and a minimum gap bound everything, so no
  configuration can flood a chat.
-}

module Bot.Handler.Humanlike.Proactive
  ( withProactiveWorker
  , recordScope
  , currentActivity
  , lifeActivities
  , parseScopeTarget
  , inQuietHours
  , Activity (..)
  ) where

import Bot.Core.Message
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Handler.Humanlike as Humanlike
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import Data.Time
  ( LocalTime (..), TimeZone, UTCTime, addUTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime
  , hoursToTimeZone, localDay, localTimeOfDay, todHour, todMin, utcToLocalTime
  )
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.FilePath ((</>))
import Text.Read (readMaybe)

-- | One entry of the character's day.
data Activity = Activity
  { activityMinute :: !Int
  , activityLabel :: !Text
  }
  deriving (Eq, Show)

humanlikeDir :: FilePath
humanlikeDir = "humanlike"

scopesPath :: FilePath
scopesPath = humanlikeDir </> "scopes.txt"

statePath :: FilePath
statePath = humanlikeDir </> "proactive.txt"

proactiveDailyLimit :: Int
proactiveDailyLimit = 8

proactiveMinGapSeconds :: Integer
proactiveMinGapSeconds = 35 * 60

chinaTimeZone :: TimeZone
chinaTimeZone = hoursToTimeZone 8

-- | Stable per (scope, day) seed.
daySeed :: Text -> LocalTime -> Int
daySeed scope localTime =
  let dayNumber = fromMaybe 0 (readMaybe (formatTime defaultTimeLocale "%j" localTime)) :: Int
      text = scope <> "|" <> Text.pack (show dayNumber)
  in Text.foldl' (\acc char -> (acc * 31 + fromEnum char) `mod` 100003) 17 text

activityPool :: [Text]
activityPool =
  [ "赖床", "吃早饭", "晒太阳发呆", "瘫着刷手机", "看视频", "摸鱼", "做饭"
  , "打盹", "散步", "洗澡", "翻聊天记录", "趴在窗边看外面"
  ]

lifeStartMinutes :: [Int]
lifeStartMinutes =
  [ 7 * 60 + 20, 9 * 60 + 10, 10 * 60 + 50, 12 * 60 + 30, 14 * 60 + 10
  , 15 * 60 + 50, 17 * 60 + 30, 19 * 60 + 10, 21 * 60, 23 * 60
  ]

-- | 今天的日程：固定时段 + 按会话与日期确定性抽取的内容。
lifeActivities :: Text -> LocalTime -> [Activity]
lifeActivities scope localTime =
  zipWith Activity lifeStartMinutes (map pick [0 ..])
  where
    seed = daySeed scope localTime
    pick index = fromMaybe "发呆" (viaNonEmpty head (drop ((seed + index * 7) `mod` length activityPool) activityPool))

-- | 现在正在做什么，以及已经做了多少分钟。
currentActivity :: Text -> UTCTime -> (Text, Int)
currentActivity scope now =
  case started of
    (activity : rest) ->
      let latest = foldl' (\best candidate -> if candidate.activityMinute >= best.activityMinute then candidate else best) activity rest
      in (latest.activityLabel, max 0 (nowMinute - latest.activityMinute))
    [] -> ("睡觉", 0)
  where
    local = utcToLocalTime chinaTimeZone now
    nowMinute = todHour (localTimeOfDay local) * 60 + todMin (localTimeOfDay local)
    started = filter (\candidate -> candidate.activityMinute <= nowMinute) (lifeActivities scope local)

-- | 00:00–08:00 不打扰。
inQuietHours :: UTCTime -> Bool
inQuietHours now =
  todHour (localTimeOfDay (utcToLocalTime chinaTimeZone now)) < 8

-- | "qq:private:2822751355" / "qq:group:819641961"
parseScopeTarget :: Text -> Maybe (ChatKind, Integer)
parseScopeTarget scope = do
  rest <- Text.stripPrefix "qq:" (Text.strip scope)
  let (kindText, withColon) = Text.breakOn ":" rest
  chatIdText <- Text.stripPrefix ":" withColon
  chatId <- readMaybe (Text.unpack chatIdText)
  kind <- case kindText of
    "private" -> Just ChatPrivate
    "group" -> Just ChatGroup
    _ -> Nothing
  pure (kind, chatId)

scopeMessage :: Text -> Maybe IncomingMessage
scopeMessage scope = do
  (kind, chatId) <- parseScopeTarget scope
  let sender = case kind of
        ChatPrivate -> Just (Text.pack (show chatId))
        ChatGroup -> Nothing
  pure IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = PlatformQQ
    , kind
    , chatId = Just chatId
    , chatAliases = []
    , digest = emptyMessageDigest
    , senderId = sender
    , senderUsername = Nothing
    , messageId = Nothing
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , files = []
    , text = ""
    , raw = Aeson.Null
    }

-- | scope → 当天已说条数、上次说的 epoch 秒。
data ProactiveState = ProactiveState
  { stateScope :: !Text
  , stateDay :: !Text
  , stateCount :: !Int
  , stateLastAt :: !Integer
  }

loadStates :: (IOE :> es, FileSystem.FileSystem :> es) => Eff es [ProactiveState]
loadStates = do
  exists <- FileSystem.doesFileExist statePath
  if not exists
    then pure []
    else do
      raw <- FileSystemByteString.readFile statePath
      pure (mapMaybe parseLine (Text.lines (TextEncoding.decodeUtf8 raw)))
  where
    parseLine line = case Text.splitOn "\t" line of
      [scope, day, countText, lastText] -> do
        count <- readMaybe (Text.unpack countText)
        lastAt <- readMaybe (Text.unpack lastText)
        pure ProactiveState{stateScope = scope, stateDay = day, stateCount = count, stateLastAt = lastAt}
      _ -> Nothing

saveStates :: (IOE :> es, FileSystem.FileSystem :> es) => [ProactiveState] -> Eff es ()
saveStates states = do
  FileSystem.createDirectoryIfMissing True humanlikeDir
  let body = Text.unlines
        [ Text.intercalate "\t" [state.stateScope, state.stateDay, Text.pack (show state.stateCount), Text.pack (show state.stateLastAt)]
        | state <- states
        ]
  FileSystemByteString.writeFile statePath (TextEncoding.encodeUtf8 body)

-- | 已启用拟人模式的会话清单，由开关命令维护。
loadScopes :: (IOE :> es, FileSystem.FileSystem :> es) => Eff es [Text]
loadScopes = do
  exists <- FileSystem.doesFileExist scopesPath
  if not exists
    then pure []
    else do
      raw <- FileSystemByteString.readFile scopesPath
      pure (filter (not . Text.null) (map Text.strip (Text.lines (TextEncoding.decodeUtf8 raw))))

recordScope :: (IOE :> es, FileSystem.FileSystem :> es) => Text -> Bool -> Eff es ()
recordScope scope enabled = do
  scopes <- loadScopes
  let updated
        | enabled = if scope `elem` scopes then scopes else scopes <> [scope]
        | otherwise = filter (/= scope) scopes
  FileSystem.createDirectoryIfMissing True humanlikeDir
  FileSystemByteString.writeFile scopesPath (TextEncoding.encodeUtf8 (Text.unlines updated))

-- | 决策回合：模型自己判断要不要开口。
decideToSpeak
  :: ( LLM.LLM :> es
     , ChatLog.ChatLog :> es
     , Memory.Memory :> es
     , IOE :> es
     , KatipE :> es
     )
  => UTCTime
  -> Text
  -> IncomingMessage
  -> Eff es (Maybe Text)
decideToSpeak now scope message = do
  persona <- Humanlike.loadPersonaForScope scope
  if Text.null (Text.strip persona)
    then pure Nothing
    else do
      recent <- ChatLog.queryChat message Nothing 1 True (ChatLog.ChatLogTimeRange (Just (addUTCTime (negate (6 * 3600)) now)) Nothing)
      let (activity, minutes) = currentActivity scope now
          lastLine = case recent of
            (entry : _) ->
              let ago = maybe (0 :: Int) (\at -> round (diffUTCTime now at / 60)) entry.recordedAt
              in [i|#{ago} 分钟前对方说：「#{Text.take 40 (Text.unwords (Text.words entry.text))}」|]
            [] -> "最近没聊过天"
          local = utcToLocalTime chinaTimeZone now
          clock = formatTime defaultTimeLocale "%H:%M" (LocalTime (localDay local) (localTimeOfDay local))
      answer <- LLM.ask (Text.unlines
        [ persona
        , ""
        , "【主动开口判断】"
        , "- 现在 " <> Text.pack clock <> "，你正在" <> activity <> "，已经 " <> Text.pack (show minutes) <> " 分钟。"
        , "- " <> lastLine
        , "- 你要不要主动找对方说一句话？"
        , "  想说：只输出这一句话，不超过 15 个字，用上文的说话方式。"
        , "  不想说：只输出 SILENCE，不要解释。"
        ])
      let trimmed = Text.strip answer
      if Text.null trimmed || Text.toCaseFold trimmed == "silence"
        then pure Nothing
        else pure (Just trimmed)

-- | 一轮检查。
proactiveTick
  :: ( Chat.Chat :> es
     , ChatLog.ChatLog :> es
     , LLM.LLM :> es
     , Memory.Memory :> es
     , IOE :> es
     , FileSystem.FileSystem :> es
     , KatipE :> es
     )
  => UTCTime
  -> Eff es Int
proactiveTick now
  | inQuietHours now = do
      logInfo "proactive: quiet hours"
      pure 0
  | otherwise = do
      scopes <- loadScopes
      states <- loadStates
      today <- pure (Text.pack (formatTime defaultTimeLocale "%F" (utcToLocalTime chinaTimeZone now)))
      let nowEpoch = round (utcTimeToPOSIXSeconds now)
      (spoken, updated) <- goScopes today nowEpoch (0, states) scopes
      saveStates updated
      pure spoken
  where
    goScopes _ _ acc [] = pure acc
    goScopes today nowEpoch acc (scope : rest) = do
      next <- step today nowEpoch acc scope
      goScopes today nowEpoch next rest
    step today nowEpoch (spoken, states) scope =
      case scopeMessage scope of
        Nothing -> pure (spoken, states)
        Just message -> do
          let current = fromMaybe (ProactiveState scope today 0 0) (find (\candidate -> candidate.stateScope == scope) states)
              count = if current.stateDay == today then current.stateCount else 0
              lastAt = if current.stateDay == today then current.stateLastAt else 0
          if count >= proactiveDailyLimit || nowEpoch - lastAt < proactiveMinGapSeconds
            then pure (spoken, states)
            else do
              logInfo [i|proactive: considering #{scope} (#{count}/#{proactiveDailyLimit})|]
              decideToSpeak now scope message >>= \case
                Nothing -> pure (spoken, states)
                Just line -> do
                  sent <- Chat.replyTo message line
                  if null (rights sent)
                    then pure (spoken, states)
                    else do
                      logInfo [i|proactive: spoke in #{scope}: #{Text.take 40 line}|]
                      let entry = ProactiveState scope today (count + 1) nowEpoch
                          rest = filter (\candidate -> candidate.stateScope /= scope) states
                      pure (spoken + 1, entry : rest)

proactiveTickMicroseconds :: Int
proactiveTickMicroseconds = 5 * 60 * 1_000_000

withProactiveWorker
  :: ( Chat.Chat :> es
     , ChatLog.ChatLog :> es
     , LLM.LLM :> es
     , Memory.Memory :> es
     , Concurrency.Concurrency :> es
     , IOE :> es
     , FileSystem.FileSystem :> es
     , KatipE :> es
     )
  => Eff es a
  -> Eff es a
withProactiveWorker inner =
  Concurrency.withWorker "humanlike.proactive" loop inner
  where
    loop = forever do
      Concurrency.sleepMicroseconds proactiveTickMicroseconds
      now <- liftIO getCurrentTime
      spoken <- proactiveTick now `catchSync` \err -> do
        logWarning [i|proactive tick failed: #{show err :: String}|]
        pure 0
      when (spoken > 0) (logInfo [i|proactive: sent #{spoken} line(s)|])
