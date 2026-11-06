module Bot.Handler.FM
  ( fmHandlers
  , DirectLibraryCommand (..)
  , DirectRecallCommand (..)
  , DirectModelCommand (..)
  , isDirectLibraryMessage
  , isPotentialTypingScore
  , parseDirectLibraryCommand
  , parseDirectRecallCommand
  , parseDirectModelCommand
  ) where

import Bot.Core.Message
import Bot.Core.Route
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatDriver as ChatDriver
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.Types as AesonTypes
import Data.Char (isDigit, isSpace)
import qualified Data.Text as Text
import qualified System.Directory as Directory
import qualified Data.Text.IO as TextIO
import Network.HTTP.Req
import Network.HTTP.Types.URI (urlEncode)
import qualified Data.Text.Encoding as TextEncoding
import Text.Read (readMaybe)

fmHandlers
  :: (HTTP.HTTP :> es, Chat.Chat :> es, LLM.LLM :> es, IOE :> es)
  => [RouteHandler es]
fmHandlers =
  [ modelRecoveryHandler
  , groupSwitchHandler
  , recallDirectHandler
  , libraryStopHandler
  , aiContestQuickHandler
  , allCompetitionRankQuickHandler
  , liveCompetitionQuickHandler
   , contestChartQuickHandler
  , messageArchiveHandler
  , recallArchiveHandler
  , pausedGroupHandler
  , libraryScoreContinueHandler
  , botGuardHandler
  , libraryDirectHandler
  , repeatCommandHandler
  , repeatFollowHandler
  , agentCapabilityHandler
  ]

modelRecoveryHandler
  :: (Chat.Chat :> es, LLM.LLM :> es)
  => RouteHandler es
modelRecoveryHandler =
  stopOn (matching isModelRecovery) \message _ ->
    if not (isSuperuser message)
      then sendStandalone message "模型恢复只有主人能用。"
      else
        LLM.resetChatModel >>= \case
          Nothing -> sendStandalone message "没有配置默认模型，无法恢复。"
          Just selected -> sendStandalone message
            ("已通过底层恢复入口切回默认模型：" <> selected.provider <> "（" <> selected.model <> "）。")

isModelRecovery :: IncomingMessage -> Bool
isModelRecovery message =
  let text = Text.toCaseFold (Text.strip message.text)
  in text `elem`
      [ "恢复默认模型"
      , "切回默认模型"
      , "模型恢复默认"
      , "紧急恢复模型"
      , "fm恢复默认模型"
      ]

data RepeatCommand
  = SetRepeat Bool
  | GetRepeat

data DirectLibraryCommand
  = DirectLibraryStats
  | DirectLibraryContinue
  | DirectLibraryArticle !Text !Int
  | DirectLibrarySingle !Text !Int !Text !Double !Double
  | DirectLibraryInvalid !Text
  deriving (Eq, Show)

data DirectRecallCommand
  = DirectRecallOne
  | DirectRecallAll
  deriving (Eq, Show)

data DirectModelCommand
  = DirectModelStatus
  | DirectModelSwitch Text
  deriving (Eq, Show)

groupSwitchHandler
  :: (HTTP.HTTP :> es, Chat.Chat :> es)
  => RouteHandler es
groupSwitchHandler =
  stopOn (matching isGroupSwitch) \message _ ->
    if not (isSuperuser message)
      then void (Chat.replyTo message "这条开关只有主人能用。")
      else case (message.chatId, switchOnline message.text) of
        (Just groupId, Just online) -> do
          setGroupOnline (show groupId) online
          let reply = if online then "FM 已在本群上线。" else "FM 已在本群下线。"
          void (Chat.replyTo message reply)
        _ -> pure ()

recallDirectHandler
  :: (HTTP.HTTP :> es, Chat.Chat :> es)
  => RouteHandler es
recallDirectHandler =
  stopOn (matching isDirectRecall) \message _ ->
    if not (isSuperuser message)
      then sendStandalone message "撤回指令只有主人能用。"
      else case parseDirectRecallCommand message.text of
        Nothing -> pure ()
        Just command -> do
          let count = case command of
                DirectRecallOne -> 1
                DirectRecallAll -> 50
          (recalled, failed) <- ChatDriver.recallRecentSelfMessages message count
          sendStandalone message $ case (recalled, failed) of
            (0, 0) -> "最近两分钟没有可撤回的 FM 消息。"
            (0, failures) ->
              "撤回失败，共 " <> show failures <> " 条；可能已经超过 QQ 的两分钟撤回时限。"
            (successes, 0) ->
              "已成功撤回最近 " <> show successes <> " 条 FM 消息。"
            (successes, failures) ->
              "已撤回 " <> show successes <> " 条，另有 " <> show failures
                <> " 条撤回失败，可能已经超过 QQ 的两分钟撤回时限。"

libraryStopHandler
  :: (HTTP.HTTP :> es, Chat.Chat :> es, IOE :> es)
  => RouteHandler es
libraryStopHandler =
  stopOn (matching isLibraryStop) \message _ -> do
    response <- postLibraryStop (Aeson.object
      [ "platform" Aeson..= chatPlatformKey message.platform
      , "chat_id" Aeson..= maybe ("" :: Text) show message.chatId
      , "requester_id" Aeson..= fromMaybe "" message.senderId
      , "requester_name" Aeson..= fromMaybe "" message.senderUsername
      , "owner" Aeson..= message.digest.senderIsSuperuser
      ])
    let status = fromMaybe ("error" :: Text) $ AesonTypes.parseMaybe
          (Aeson.withObject "FM library stop" (Aeson..: "status")) response
        reply = fromMaybe ("发文停止失败。" :: Text) $ AesonTypes.parseMaybe
          (Aeson.withObject "FM library stop" \o -> o Aeson..:? "message" Aeson..!= "") response
        maybeMessageId = AesonTypes.parseMaybe
          (Aeson.withObject "FM library stop" \o -> o Aeson..:? "last_message_id") response
          >>= id
    recalled <- if status /= "stopped"
      then pure Nothing
      else case Text.strip <$> maybeMessageId of
        Just messageId | not (Text.null messageId) ->
          Just <$> Chat.deleteMessage message (textMessageId messageId)
        _ -> pure Nothing
    when (recalled == Just True) $
      for_ (Text.strip <$> maybeMessageId) \messageId ->
        void $ postLibraryAction "session" "recalled" (directLibraryPayload message
          [ "message_ids" Aeson..= [messageId]
          ])
    if status /= "stopped"
      then do
        -- 库文没有进行中的发文时再问番茄专区：所有者 2026-11-06 要求
        -- 「fm 停」对番茄也要停掉并撤回最近一段，跟库文一个行为。
        novelResponse <- postNovelStop (Aeson.object
          [ "platform" Aeson..= chatPlatformKey message.platform
          , "chat_id" Aeson..= maybe ("" :: Text) show message.chatId
          , "requester_id" Aeson..= fromMaybe "" message.senderId
          , "requester_name" Aeson..= fromMaybe "" message.senderUsername
          ])
        let novelStatusText = fromMaybe ("idle" :: Text) $ AesonTypes.parseMaybe
              (Aeson.withObject "novel stop" \o -> o Aeson..:? "status" Aeson..!= "")
              novelResponse
            novelReply = fromMaybe ("当前会话没有正在进行的番茄小说。" :: Text) $ AesonTypes.parseMaybe
              (Aeson.withObject "novel stop" \o -> o Aeson..:? "message" Aeson..!= "")
              novelResponse
        if novelStatusText /= "stopped"
          then sendStandalone message reply
          else do
            let lastPath = "/data/novel-last-" <> maybe "nochat" show message.chatId <> ".txt"
            exists <- liftIO (Directory.doesFileExist lastPath)
            recalledNovel <- if exists
              then do
                rawId <- liftIO (TextIO.readFile lastPath)
                if Text.null (Text.strip rawId)
                  then pure Nothing
                  else Just <$> Chat.deleteMessage message (textMessageId (Text.strip rawId))
              else pure Nothing
            sendStandalone message $ case recalledNovel of
              Just True -> novelReply <> " 最近一段也已撤回。"
              Just False -> novelReply <> " 但最近一段撤回失败（可能超过 QQ 两分钟时限）。"
              Nothing -> novelReply
      else sendStandalone message $ case recalled of
        Just True -> "发文已停止，最近一段也已撤回。"
        Just False -> "发文已停止，但最近一段撤回失败。"
        Nothing -> reply

-- | 番茄停止：POST /novel/stop
postNovelStop :: HTTP.HTTP :> es => Aeson.Value -> Eff es Aeson.Value
postNovelStop payload = do
  response <- HTTP.runReq $
    req POST (http "172.20.0.4" /: "novel" /: "stop")
      (ReqBodyJson payload) jsonResponse (port 8077)
  pure (responseBody response :: Aeson.Value)

aiContestQuickHandler
  :: (HTTP.HTTP :> es, Chat.Chat :> es)
  => RouteHandler es
aiContestQuickHandler =
  stopOn (matching isAiContestQuick) \message _ -> do
    response <- HTTP.runReq $
      req GET (http "172.20.0.4" /: "ai-contest" /: "text") NoReqBody jsonResponse (port 8077)
    case parseAiContest (responseBody response :: Aeson.Value) of
      Nothing -> sendStandalone message "今天的 AI 赛文还没生成，直接告诉 FM 想要什么风格，我现在写。"
      Just (competitionDate, title, body, difficulty) ->
        sendStandalone message $
          "[FM/AI赛文·" <> difficulty <> "] 《" <> title <> "》 [字数" <> show (Text.length body) <> "]\n"
          <> body <> "\n-----第555段 " <> competitionDate <> "-FM赛文"

liveCompetitionQuickHandler
  :: (HTTP.HTTP :> es, Chat.Chat :> es)
  => RouteHandler es
liveCompetitionQuickHandler =
  stopOn (matching isLiveCompetitionRankQuick) \message _ -> do
    enabled <- case (message.kind, message.chatId) of
      (ChatGroup, Just groupId) -> groupCapabilityEnabled (show groupId) "contest"
      _ -> pure True
    if not enabled
      then sendStandalone message "当前群没有开启赛文排行榜功能。"
      else do
        response <- HTTP.runReq $
          req GET (http "172.20.0.4" /: "competition" /: "live") NoReqBody jsonResponse
            ( "source" =: ("锦标赛" :: Text)
              <> "group_id" =: maybe ("" :: Text) show message.chatId
              <> "soft" =: True
              <> port 8077
            )
        case parseLiveCompetition (responseBody response :: Aeson.Value) of
          Just ("ok", kind, actualDate, _) -> do
            let sourceKey = if kind == "group" then maybe "" show message.chatId else kind
                cacheKey = fromMaybe "refresh" (messageIdText <$> message.messageId)
                imageUrl = "http://172.20.0.4:8077/reports/live-competition.png?source="
                  <> sourceKey <> "&date=" <> actualDate <> "&combined=1&v=" <> cacheKey
            void $ Chat.replyTo (standaloneMessage message)
              "😻 FM：正在调用 fm_live_competition_rank 工具…"
            sent <- Chat.replyTo (standaloneMessage message) (ReplyBody.imageDirective imageUrl)
            when (null (rights sent)) $
              sendStandalone message "锦标赛排行榜图片发送失败。"
          Just (_, _, _, errorMessage) ->
            sendStandalone message ("锦标赛排行榜获取失败：" <> errorMessage)
          Nothing ->
            sendStandalone message "锦标赛排行榜获取失败：领域服务返回格式不正确。"

isLiveCompetitionRankQuick :: IncomingMessage -> Bool
isLiveCompetitionRankQuick message =
  message.eventKind == IncomingMessageCreated
    && message.platform == PlatformQQ
    && let text = Text.toCaseFold (Text.strip message.text)
       in (Text.isPrefixOf "fm" text || "@fm" `Text.isInfixOf` text)
          && "锦标赛" `Text.isInfixOf` text
          && ("排行榜" `Text.isInfixOf` text || "排行榜" `Text.isInfixOf` Text.replace "榜" "排行榜" text)

parseLiveCompetition :: Aeson.Value -> Maybe (Text, Text, Text, Text)
parseLiveCompetition value = AesonTypes.parseMaybe parser value
  where
    parser = Aeson.withObject "FM live competition" \o ->
      (,,,)
        <$> o Aeson..:? "status" Aeson..!= "error"
        <*> o Aeson..:? "kind" Aeson..!= "champ"
        <*> o Aeson..:? "date" Aeson..!= ""
        <*> o Aeson..:? "message" Aeson..!= "公开赛事网站当前没有返回可用数据。"

-- | 赛文成绩图直接路由（所有者 2026-11-06 批准：不信模型选工具）。
--
-- 模型反复失手：选错工具、不传 mode、传错赛文、传「我」当名字。
-- 这条路由命中「曲线/走势/趋势」+ 赛文关键词就直接出图，完全不经过模型。
contestChartQuickHandler
  :: (Chat.Chat :> es) => RouteHandler es
contestChartQuickHandler =
  stopOn (matching isContestChartQuick) \message _ -> do
    case pickContest (Text.toCaseFold (Text.strip message.text)) of
      Nothing -> sendStandalone message "没认出是哪个赛文。可以说：群赛文 / 虎杯 / 锦标赛 / 极速杯 / 极速联赛 / 666 / 777 / groupmatch。"
      Just (contestKey, contestLabel) -> do
        let text = Text.toCaseFold (Text.strip message.text)
            days = pickDays text
            userText = pickUser text message
            groupValue =
              case pickGroupName text of
                Just groupName -> groupName
                Nothing ->
                  case message.kind of
                    ChatGroup -> fromMaybe "" (show <$> message.chatId)
                    _ -> ""
            url =
              "http://172.20.0.4:8077/reports/score-chart.png?contest=" <> urlEncodeText contestKey
                <> "&group_id=" <> urlEncodeText groupValue
                <> "&days=" <> Text.pack (show days)
                <> "&mode=trend&user=" <> urlEncodeText userText
                <> "&_=" <> maybe "0" (Text.pack . show) message.messageId
        sent <- Chat.replyTo (standaloneMessage message) (ReplyBody.imageDirective url)
        when (null (rights sent)) $
          sendStandalone message ("「" <> contestLabel <> "」成绩曲线图发送失败，稍后再试。")

isContestChartQuick :: IncomingMessage -> Bool
isContestChartQuick message =
  message.eventKind == IncomingMessageCreated
    && message.platform == PlatformQQ
    && let text = Text.toCaseFold (Text.strip message.text)
       in (Text.isPrefixOf "fm" text || "@fm" `Text.isInfixOf` text)
            && any (`Text.isInfixOf` text) ["曲线", "走势", "趋势"]
            && not (Text.null (fst (fromMaybe ("", "") (pickContest text))))

pickContest :: Text -> Maybe (Text, Text)
pickContest text
  | "极速联赛" `Text.isInfixOf` text = Just ("100004", "极速联赛")
  | "极速杯" `Text.isInfixOf` text = Just ("99999", "极速杯")
  | "锦标赛" `Text.isInfixOf` text = Just ("100000", "锦标赛")
  | "虎杯" `Text.isInfixOf` text = Just ("HB", "虎杯")
  | "groupmatch" `Text.isInfixOf` text || "1847992065" `Text.isInfixOf` text =
      Just ("groupmatch", "1847992065群赛文")
  | "晴分身" `Text.isInfixOf` text || "777" `Text.isInfixOf` text = Just ("777", "晴分身群赛文")
  | "1916996679" `Text.isInfixOf` text || "666" `Text.isInfixOf` text = Just ("666", "1916996679每日赛文")
  | "555" `Text.isInfixOf` text = Just ("555", "555赛文")
  | any (`Text.isInfixOf` text) ["群赛文", "本群赛文", "赛文"] = Just ("999", "本群赛文")
  | otherwise = Nothing

-- | 从消息里认出群名（所有者 2026-11-06：群号换成中文）
pickGroupName :: Text -> Maybe Text
pickGroupName text
  | "梦幻打字阁" `Text.isInfixOf` text = Just "梦幻打字阁"
  | "五笔修炼基地" `Text.isInfixOf` text = Just "五笔修炼基地"
  | "小鹤进修班" `Text.isInfixOf` text = Just "小鹤进修班"
  | "仓颉之友" `Text.isInfixOf` text || "倉頡之友" `Text.isInfixOf` text = Just "仓颉之友"
  | "剑气冲霄堂" `Text.isInfixOf` text = Just "剑气冲霄堂"
  | "五林风" `Text.isInfixOf` text = Just "五林风"
  | "虎码训练营" `Text.isInfixOf` text || "虎营" `Text.isInfixOf` text = Just "虎码训练营"
  | "帝隆" `Text.isInfixOf` text = Just "帝隆"
  | "092" `Text.isInfixOf` text = Just "092五笔"
  | otherwise = Nothing

pickDays :: Text -> Int
pickDays text
  | any (`Text.isInfixOf` text) ["全部", "所有", "历来", "历史", "累计", "一直", "从头"] = 3650
  | "两年" `Text.isInfixOf` text || "2年" `Text.isInfixOf` text = 730
  | any (`Text.isInfixOf` text) ["一年", "一个年", "1年", "12个月", "近一年", "一年内"] = 365
  | "半年" `Text.isInfixOf` text || "六个月" `Text.isInfixOf` text = 180
  | "今年" `Text.isInfixOf` text || "本年" `Text.isInfixOf` text = 365
  | "两个月" `Text.isInfixOf` text || "60天" `Text.isInfixOf` text = 60
  | "三个月" `Text.isInfixOf` text || "90天" `Text.isInfixOf` text || "90 天" `Text.isInfixOf` text = 90
  | "一周" `Text.isInfixOf` text || "7天" `Text.isInfixOf` text || "七天" `Text.isInfixOf` text = 7
  | "今天" `Text.isInfixOf` text || "当天" `Text.isInfixOf` text = 1
  | "一个月" `Text.isInfixOf` text || "30天" `Text.isInfixOf` text || "30 天" `Text.isInfixOf` text = 30
  | Just months <- digitsBefore "个月" text = months * 30
  | otherwise = fromMaybe 30 (digitsBefore "天" text)

digitsBefore :: Text -> Text -> Maybe Int
digitsBefore marker text =
  case Text.breakOn marker text of
    (before, rest)
      | not (Text.null rest) ->
          let digits = Text.takeWhileEnd isDigit before
          in if Text.null digits then Nothing else readMaybe (Text.unpack digits)
    _ -> Nothing

pickUser :: Text -> IncomingMessage -> Text
pickUser text message =
  let speaker = Text.strip (fromMaybe "" message.senderUsername)
      body = Text.strip (dropFm text)
      boundary = earliestIndex
        [ "近", "最", "这", "过", "赛文", "成绩", "曲线", "趋势", "走势", "锦标赛", "虎杯"
        , "极速", "晴分身", "555", "666", "777", "999", "groupmatch", "一个月", "天", "的" ]
        body
      candidate = Text.strip (Text.take boundary body)
      cleaned = stripPrefixes
        [ "看下", "看一下", "看看", "查一下", "查询", "查", "画一下", "画下", "画", "给我", "帮我"
        , "帮忙", "来", "要", "想", "一下", "下" ] candidate
  in if Text.null cleaned || any (`Text.isInfixOf` cleaned) ["我", "自己", "本人"]
       then speaker
       else cleaned

dropFm :: Text -> Text
dropFm text =
  let stripped = Text.strip text
  in fromMaybe stripped (Text.stripPrefix "fm" stripped)

stripPrefixes :: [Text] -> Text -> Text
stripPrefixes prefixes value =
  case filter (\p -> Text.isPrefixOf p value) prefixes of
    (p : _) -> Text.strip (Text.drop (Text.length p) value)
    [] -> value

earliestIndex :: [Text] -> Text -> Int
earliestIndex needles hay =
  foldr min (Text.length hay)
    (map (\n -> Text.length (fst (Text.breakOn n hay))) (filter (not . Text.null) needles))

urlEncodeText :: Text -> Text
urlEncodeText = TextEncoding.decodeUtf8 . urlEncode True . TextEncoding.encodeUtf8

messageArchiveHandler
  :: (HTTP.HTTP :> es, Chat.Chat :> es)
  => RouteHandler es
messageArchiveHandler =
  continueOn (matching isArchivableMessage) \message _ -> do
    response <- postEvent "message" (messagePayload message)
    when (message.platform == PlatformQQ && jsonFlag "ai_contest_archived" response) do
      let competitionDate = fromMaybe "" . join $ AesonTypes.parseMaybe
            (Aeson.withObject "FM event response" (Aeson..:? "competition_date")) response
          cacheKey = fromMaybe "refresh" (messageIdText <$> message.messageId)
          leaderboardUrl =
            "http://172.20.0.4:8077/reports/ai-leaderboard.png?date="
              <> competitionDate <> "&v=" <> cacheKey
      sent <- Chat.replyTo (standaloneMessage message)
        (ReplyBody.imageDirective leaderboardUrl)
      when (null (rights sent)) $
        void $ Chat.replyTo (standaloneMessage message) "555 成绩已收录，但排行榜图片发送失败。"

libraryScoreContinueHandler
  :: (HTTP.HTTP :> es, Chat.Chat :> es)
  => RouteHandler es
libraryScoreContinueHandler =
  stopOn (matching isPotentialTypingScore) \message _ -> do
    -- Score handling stops later routes, so archive the score explicitly here.
    void $ postEvent "message" (messagePayload message)
    enabled <- case (message.kind, message.chatId) of
      (ChatGroup, Just groupId) -> groupCapabilityEnabled (show groupId) "library"
      _ -> pure True
    when enabled do
      response <- postLibraryAction "session" "score" (directLibraryPayload message
        [ "text" Aeson..= message.text
        ])
      let status = fromMaybe ("error" :: Text) $ AesonTypes.parseMaybe
            (Aeson.withObject "FM library score result" (Aeson..: "status")) response
      when (status == "segment") $
        sendDirectLibraryResult message response
      -- 库文没有在打的文章时，看看番茄专区有没有进度（2026-11-06：
      -- 所有者问「为什么打完之后没有自动续段」，因为这条路只接了库文）。
      when (status /= "segment") do
        novelResponse <- postNovelScore (Aeson.object
          [ "platform" Aeson..= chatPlatformKey message.platform
          , "chat_id" Aeson..= maybe ("" :: Text) show message.chatId
          , "requester_id" Aeson..= fromMaybe "" message.senderId
          , "requester_name" Aeson..= fromMaybe "" message.senderUsername
          , "text" Aeson..= message.text
          ])
        let novelStatusText = fromMaybe ("idle" :: Text) $ AesonTypes.parseMaybe
              (Aeson.withObject "novel score" \o -> o Aeson..:? "status" Aeson..!= "")
              novelResponse
        when (novelStatusText == "segment") do
          let novelMessage = fromMaybe "" $ AesonTypes.parseMaybe
                (Aeson.withObject "novel score" \o -> o Aeson..:? "message" Aeson..!= "")
                novelResponse
          void $ Chat.replyTo (standaloneMessage message) novelMessage

-- | 番茄成绩：POST /novel/score
postNovelScore :: HTTP.HTTP :> es => Aeson.Value -> Eff es Aeson.Value
postNovelScore payload = do
  response <- HTTP.runReq $
    req POST (http "172.20.0.4" /: "novel" /: "score")
      (ReqBodyJson payload) jsonResponse (port 8077)
  pure (responseBody response :: Aeson.Value)

recallArchiveHandler :: HTTP.HTTP :> es => RouteHandler es
recallArchiveHandler =
  continueOn (matching isRecall) \message _ ->
    void $ postEvent "recall" (messagePayload message)

pausedGroupHandler :: HTTP.HTTP :> es => RouteHandler es
pausedGroupHandler =
  Route
    { help = Nothing
    , helpVisible = const False
    , decide = \message ->
        if not (isPausableGroupMessage message)
          then pure Skip
          else case message.chatId of
            Nothing -> pure Skip
            Just groupId -> do
              online <- groupOnline (show groupId)
              pure if online then Skip else StopWith (pure ())
    }

botGuardHandler
  :: (HTTP.HTTP :> es, Chat.Chat :> es)
  => RouteHandler es
botGuardHandler =
  Route
    { help = Nothing
    , helpVisible = const False
    , decide = \message ->
        if not (isQQGroupMessage message)
          then pure Skip
          else do
            result <- checkBotGuard message
            pure case result of
              Nothing -> Skip
              Just reply -> StopWith do
                unless (Text.null reply) $
                  void (Chat.replyTo message reply)
    }

libraryDirectHandler
  :: (HTTP.HTTP :> es, Chat.Chat :> es)
  => RouteHandler es
libraryDirectHandler =
  Route
    { help = Nothing
    , helpVisible = const False
    , decide = \message ->
        if not (isDirectLibraryMessage message)
          then pure Skip
          else case parseDirectLibraryCommand message.text of
            Nothing -> pure Skip
            Just command -> do
              enabled <- case (message.kind, message.chatId) of
                (ChatGroup, Just groupId) -> groupCapabilityEnabled (show groupId) "library"
                _ -> pure True
              pure . StopWith $
                when enabled (runDirectLibraryCommand message command)
    }

isDirectLibraryMessage :: IncomingMessage -> Bool
isDirectLibraryMessage message =
  message.eventKind == IncomingMessageCreated
    && message.platform `elem` [PlatformQQ, PlatformMatrix]

runDirectLibraryCommand
  :: (HTTP.HTTP :> es, Chat.Chat :> es)
  => IncomingMessage
  -> DirectLibraryCommand
  -> Eff es ()
runDirectLibraryCommand message = \case
  DirectLibraryInvalid reason ->
    sendStandalone message reason
  DirectLibraryStats -> do
    response <- HTTP.runReq $
      req GET (http "172.20.0.4" /: "library" /: "stats") NoReqBody jsonResponse (port 8077)
    sendStandalone message (renderLibraryStats (responseBody response :: Aeson.Value))
  DirectLibraryContinue -> do
    response <- postLibraryAction "session" "continue-same" (directLibraryPayload message [])
    sendDirectLibraryResult message response
  DirectLibraryArticle difficulty requestedLength -> do
    response <- postLibraryAction "session" "start" (directLibraryPayload message
      [ "query" Aeson..= ("" :: Text)
      , "difficulty" Aeson..= difficulty
      , "length" Aeson..= requestedLength
      ])
    sendDirectLibraryResult message response
  DirectLibrarySingle name requestedLength orderName keyReq accReq -> do
    response <- postLibraryAction "single" "start" (directLibraryPayload message
      [ "name" Aeson..= name
      , "length" Aeson..= requestedLength
      , "order" Aeson..= orderName
      , "key_req" Aeson..= keyReq
      , "acc_req" Aeson..= accReq
      ])
    sendDirectLibraryResult message response

sendDirectLibraryResult
  :: (HTTP.HTTP :> es, Chat.Chat :> es)
  => IncomingMessage
  -> Aeson.Value
  -> Eff es ()
sendDirectLibraryResult message response = do
  let status = fromMaybe ("error" :: Text) $ AesonTypes.parseMaybe
        (Aeson.withObject "FM direct library result" (Aeson..: "status")) response
      body = fromMaybe ("发文失败。" :: Text) $ AesonTypes.parseMaybe
        (Aeson.withObject "FM direct library result" \o -> o Aeson..:? "message" Aeson..!= "") response
      sessionId = fromMaybe ("" :: Text) $ AesonTypes.parseMaybe
        (Aeson.withObject "FM direct library result" \o -> o Aeson..:? "session_id" Aeson..!= "") response
  sent <- Chat.replyTo (standaloneMessage message) body
  when (status == "segment" && not (Text.null sessionId)) $
    for_ (viaNonEmpty head (rights sent)) \sentMessageId ->
      void $ postLibraryAction "session" "sent" (Aeson.object
        [ "session_id" Aeson..= sessionId
        , "message_id" Aeson..= messageIdText sentMessageId
        ])

sendStandalone :: Chat.Chat :> es => IncomingMessage -> Text -> Eff es ()
sendStandalone message body =
  void (Chat.replyTo (standaloneMessage message) body)

standaloneMessage :: IncomingMessage -> IncomingMessage
standaloneMessage message =
  message
    { messageId = Nothing
    , replyToMessageId = Nothing
    , raw = Aeson.Null
    }

directLibraryPayload :: IncomingMessage -> [AesonTypes.Pair] -> Aeson.Value
directLibraryPayload message fields =
  Aeson.object $
    [ "platform" Aeson..= chatPlatformKey message.platform
    , "chat_id" Aeson..= maybe ("" :: Text) show message.chatId
    , "requester_id" Aeson..= fromMaybe "" message.senderId
    , "requester_name" Aeson..= fromMaybe "" message.senderUsername
    , "owner" Aeson..= message.digest.senderIsSuperuser
    ] <> fields

postLibraryAction :: HTTP.HTTP :> es => Text -> Text -> Aeson.Value -> Eff es Aeson.Value
postLibraryAction section action payload = do
  response <- HTTP.runReq $
    req POST (http "172.20.0.4" /: "library" /: section /: action)
      (ReqBodyJson payload) jsonResponse (port 8077)
  pure (responseBody response :: Aeson.Value)

renderLibraryStats :: Aeson.Value -> Text
renderLibraryStats value =
  fromMaybe "文库统计读取失败。" $ AesonTypes.parseMaybe parse value
  where
    parse = Aeson.withObject "FM library stats" \o -> do
      total <- o Aeson..:? "texts" Aeson..!= (0 :: Int)
      categories <- o Aeson..: "categories"
      (normal, single, baobiao) <- Aeson.withObject "FM library categories" (\c ->
        (,,)
          <$> c Aeson..:? "fm_texts" Aeson..!= (0 :: Int)
          <*> c Aeson..:? "fm_single_chars" Aeson..!= (0 :: Int)
          <*> c Aeson..:? "fm_baobiao_texts" Aeson..!= (0 :: Int)) categories
      pure $
        "FM 文库当前共有 " <> show total <> " 份文本，其中普通文本 " <> show normal
          <> " 份、单字库 " <> show single <> " 份、爆表文 " <> show baobiao <> " 份。"

repeatCommandHandler
  :: (HTTP.HTTP :> es, Chat.Chat :> es)
  => RouteHandler es
repeatCommandHandler =
  stopOn (matching (isJust . repeatCommand . (.text))) \message _ ->
    if not (isSuperuser message)
      then void (Chat.replyTo message "这个开关只有主人能改。")
      else case (message.chatId, repeatCommand message.text) of
        (Just groupId, Just (SetRepeat enabled)) -> do
          actual <- setRepeatEnabled (show groupId) enabled
          let reply = if actual then "跟风复读开了。" else "跟风复读关了。"
          void (Chat.replyTo message reply)
        (Just groupId, Just GetRepeat) -> do
          enabled <- repeatEnabled (show groupId)
          let reply = if enabled then "跟风复读现在开着。" else "跟风复读现在关着。"
          void (Chat.replyTo message reply)
        _ -> pure ()

allCompetitionRankQuickHandler
  :: (HTTP.HTTP :> es, Chat.Chat :> es)
  => RouteHandler es
allCompetitionRankQuickHandler =
  stopOn (matching isAllCompetitionRankQuick) \message _ -> do
    enabled <- case (message.kind, message.chatId) of
      (ChatGroup, Just groupId) -> groupCapabilityEnabled (show groupId) "contest"
      _ -> pure True
    if not enabled
      then sendStandalone message "当前群没有开启赛文排行榜功能。"
      else do
        void $ Chat.replyTo (standaloneMessage message)
          "😻 FM：正在调用 fm_live_competition_rank 工具…"
        results <- traverse (fetchRank message)
          [ ("champ" :: Text, "锦标赛" :: Text)
          , ("tiger", "虎杯")
          , ("comp", "极速杯")
          , ("540678308", "极速联赛")
          , ("1021522088", "五笔修炼基地")
          , ("151040026", "帝隆")
          , ("776227233", "梦幻打字阁")
          , ("391047371", "092五笔正规闲聊群")
          , ("201323122", "倉頡之友")
          , ("488748631", "小鹤进修班")
          ]
        aiResult <- fetchAiRank
        let nodes = rights (results <> [aiResult])
        if null nodes
          then sendStandalone message "全部排行榜获取失败，请稍后再试。"
          else do
            sent <- Chat.sendMergedForward (standaloneMessage message) nodes
            case sent of
              Right _ -> sendStandalone message "😻 FM：全部排行榜已合并转发。"
              Left errorText -> sendStandalone message ("全部排行榜合并转发失败：" <> errorText)
  where
    fetchRank targetMessage (sourceKey, label) = do
      response <- HTTP.runReq $
        req GET (http "172.20.0.4" /: "competition" /: "live") NoReqBody jsonResponse
          ( "source" =: sourceKey
            <> "group_id" =: maybe ("" :: Text) show targetMessage.chatId
            <> "soft" =: True
            <> port 8077
          )
      case AesonTypes.parseMaybe parseRank (responseBody response :: Aeson.Value) of
        Just ("ok", actualDate) ->
          pure $ Right (forwardNode label
            ("http://172.20.0.4:8077/reports/live-competition.png?source="
              <> sourceKey <> "&date=" <> actualDate <> "&combined=1"))
        Just (_, _) -> pure (Left label)
        Nothing -> pure (Left label)
    fetchAiRank = do
      response <- HTTP.runReq $
        req GET (http "172.20.0.4" /: "ai-contest" /: "leaderboard") NoReqBody jsonResponse
          (port 8077)
      case AesonTypes.parseMaybe parseAiRank (responseBody response :: Aeson.Value) of
        Just (actualDate, rows) | not (null rows) ->
          pure $ Right (forwardNode ("555 AI赛文榜" :: Text)
            ("http://172.20.0.4:8077/reports/ai-leaderboard.png?date=" <> actualDate))
        _ -> pure (Left "555 AI赛文榜")
    parseAiRank = Aeson.withObject "FM AI leaderboard" $ \o ->
      (,) <$> o Aeson..:? "date" Aeson..!= ("" :: Text)
          <*> o Aeson..:? "rows" Aeson..!= ([] :: [Aeson.Value])
    parseRank = Aeson.withObject "FM live rank" $ \o ->
      (,)
        <$> o Aeson..:? "status" Aeson..!= ("error" :: Text)
        <*> o Aeson..:? "date" Aeson..!= ""
    forwardNode label imageUrl =
      Aeson.object
        [ "type" Aeson..= Aeson.String "node"
        , "data" Aeson..= Aeson.object
            [ "name" Aeson..= Aeson.String "FM"
            , "uin" Aeson..= Aeson.String "3471095459"
            , "content" Aeson..= [
                Aeson.object
                  [ "type" Aeson..= Aeson.String "text"
                  , "data" Aeson..= Aeson.object ["text" Aeson..= label]
                  ]
              , Aeson.object
                  [ "type" Aeson..= Aeson.String "image"
                  , "data" Aeson..= Aeson.object ["file" Aeson..= imageUrl]
                  ]
              ]
            ]
        ]

isAllCompetitionRankQuick :: IncomingMessage -> Bool
isAllCompetitionRankQuick message =
  message.eventKind == IncomingMessageCreated
    && message.platform == PlatformQQ
    && let text = Text.toCaseFold (Text.strip message.text)
       in (Text.isPrefixOf "fm" text || "@fm" `Text.isInfixOf` text)
          && ("全部" `Text.isInfixOf` text || "所有" `Text.isInfixOf` text)
          && ("排行榜" `Text.isInfixOf` text || "排行榜" `Text.isInfixOf` Text.replace "榜" "排行榜" text)

repeatFollowHandler
  :: (HTTP.HTTP :> es, Chat.Chat :> es)
  => RouteHandler es
repeatFollowHandler =
  Route
    { help = Nothing
    , helpVisible = const False
    , decide = \message ->
        if not (isQQGroupMessage message)
          then pure Skip
          else do
            repeated <- checkRepeatFollow message
            pure case repeated of
              Nothing -> Skip
              Just text -> StopWith (void (Chat.replyTo (standaloneMessage message) text))
    }

agentCapabilityHandler :: HTTP.HTTP :> es => RouteHandler es
agentCapabilityHandler =
  Route
    { help = Nothing
    , helpVisible = const False
    , decide = \message ->
        if not (isQQGroupMessage message)
          then pure Skip
          else case message.chatId of
            Nothing -> pure Skip
            Just groupId -> do
              enabled <- groupCapabilityEnabled (show groupId) "agent"
              pure if enabled then Skip else StopWith (pure ())
    }

isGroupSwitch :: IncomingMessage -> Bool
isGroupSwitch message =
  message.platform == PlatformQQ
    && message.kind == ChatGroup
    && isJust (switchOnline message.text)

isDirectRecall :: IncomingMessage -> Bool
isDirectRecall message =
  message.eventKind == IncomingMessageCreated
    && message.platform == PlatformQQ
    && isJust (parseDirectRecallCommand message.text)

parseDirectRecallCommand :: Text -> Maybe DirectRecallCommand
parseDirectRecallCommand raw =
  case Text.toCaseFold (compactCommand (Text.replace "@" "" raw)) of
    "撤回" -> Just DirectRecallOne
    "撤回刚才" -> Just DirectRecallOne
    "撤回最近" -> Just DirectRecallOne
    "撤回最近消息" -> Just DirectRecallOne
    "撤回全部" -> Just DirectRecallAll
    "撤回所有" -> Just DirectRecallAll
    "撤回最近所有消息" -> Just DirectRecallAll
    "撤回最近全部消息" -> Just DirectRecallAll
    "撤回全部消息" -> Just DirectRecallAll
    "撤回所有消息" -> Just DirectRecallAll
    _ -> Nothing

parseDirectModelCommand :: Text -> Maybe DirectModelCommand
parseDirectModelCommand raw =
  let normalized = Text.toCaseFold
        (Text.filter (not . (`elem` [' ', '\t', '\x3000', '，', ',']))
          (Text.replace "@" "" (Text.strip raw)))
      visionModel = "deepseek-v4-flash-vision"
  in if normalized == "fm模型列表" || normalized == "模型列表"
       then Just DirectModelStatus
       else if "fm切换模型" `Text.isPrefixOf` normalized
         then Just (DirectModelSwitch (Text.drop (Text.length "fm切换模型") normalized))
         else if "fm使用" `Text.isPrefixOf` normalized
           then Just (DirectModelSwitch (Text.drop (Text.length "fm使用") normalized))
           else if "让fm切换带视觉的模型" `Text.isInfixOf` normalized
             then Just (DirectModelSwitch visionModel)
             else if "fm切换视觉模型" `Text.isInfixOf` normalized
               then Just (DirectModelSwitch visionModel)
               else Nothing

switchOnline :: Text -> Maybe Bool
switchOnline text =
  case Text.toLower (Text.strip text) of
    "fm 上线" -> Just True
    "fm上线" -> Just True
    "fm 下线" -> Just False
    "fm下线" -> Just False
    _ -> Nothing

isLibraryStop :: IncomingMessage -> Bool
isLibraryStop message =
  message.eventKind == IncomingMessageCreated
    && message.platform == PlatformQQ
    && Text.toLower (Text.filter (not . (`elem` [' ', '\t', '\x3000'])) message.text) == "fm停"

isAiContestQuick :: IncomingMessage -> Bool
isAiContestQuick message =
  message.eventKind == IncomingMessageCreated
    && message.platform == PlatformQQ
    && Text.strip message.text == "555"

parseAiContest :: Aeson.Value -> Maybe (Text, Text, Text, Text)
parseAiContest =
  AesonTypes.parseMaybe $ Aeson.withObject "FM AI contest text" \o ->
    (,,,)
      <$> o Aeson..: "competition_date"
      <*> o Aeson..: "title"
      <*> o Aeson..: "body"
      <*> o Aeson..:? "difficulty" Aeson..!= "普"

isArchivableMessage :: IncomingMessage -> Bool
isArchivableMessage message =
  message.eventKind == IncomingMessageCreated
    && message.platform `elem` [PlatformQQ, PlatformMatrix]
    && isJust message.messageId

isRecall :: IncomingMessage -> Bool
isRecall message =
  message.eventKind == IncomingMessageDeleted
    && message.platform `elem` [PlatformQQ, PlatformMatrix]
    && isJust message.messageId

isPausableGroupMessage :: IncomingMessage -> Bool
isPausableGroupMessage message =
  message.eventKind == IncomingMessageCreated
    && message.platform == PlatformQQ
    && message.kind == ChatGroup

isQQGroupMessage :: IncomingMessage -> Bool
isQQGroupMessage message =
  message.eventKind == IncomingMessageCreated
    && message.platform == PlatformQQ
    && message.kind == ChatGroup

repeatCommand :: Text -> Maybe RepeatCommand
repeatCommand text =
  case compactCommand text of
    "复读开" -> Just (SetRepeat True)
    "跟风复读开" -> Just (SetRepeat True)
    "复读开启" -> Just (SetRepeat True)
    "开启复读" -> Just (SetRepeat True)
    "打开复读" -> Just (SetRepeat True)
    "复读关" -> Just (SetRepeat False)
    "跟风复读关" -> Just (SetRepeat False)
    "复读关闭" -> Just (SetRepeat False)
    "关闭复读" -> Just (SetRepeat False)
    "关掉复读" -> Just (SetRepeat False)
    "复读状态" -> Just GetRepeat
    "跟风复读状态" -> Just GetRepeat
    _ -> Nothing

compactCommand :: Text -> Text
compactCommand =
  Text.filter (not . (`elem` [' ', '\t', '\x3000']))
    . Text.toLower
    . Text.strip
    . fromMaybeText
    . Text.stripPrefix "fm"
    . Text.toLower
    . Text.strip
  where
    fromMaybeText = fromMaybe ""

parseDirectLibraryCommand :: Text -> Maybe DirectLibraryCommand
parseDirectLibraryCommand raw =
  case compactCommand raw of
    "" -> Nothing
    "文库" -> Just DirectLibraryStats
    "续" -> Just DirectLibraryContinue
    "续发" -> Just DirectLibraryContinue
    "续段" -> Just DirectLibraryContinue
    body -> parseDirectSingle body <|> parseDirectArticle body

isPotentialTypingScore :: IncomingMessage -> Bool
isPotentialTypingScore message =
  message.eventKind == IncomingMessageCreated
    && message.platform `elem` [PlatformQQ, PlatformMatrix]
    && "第" `Text.isInfixOf` message.text
    && "段" `Text.isInfixOf` message.text
    && any (`Text.isInfixOf` message.text) ["速度", "速"]

parseDirectArticle :: Text -> Maybe DirectLibraryCommand
parseDirectArticle body =
  firstJust
    [ parse "文来" ""
    , parse "爆表" "虐"
    , parse "爆虐" "虐"
    , parse "淼" "淼"
    , parse "水" "水"
    , parse "易" "易"
    , parse "普" "普"
    , parse "难" "难"
    , parse "虐" "虐"
    ]
  where
    parse prefix difficulty = do
      suffix <- Text.stripPrefix prefix body
      guard (Text.null suffix || Text.all isDigit suffix)
      let requestedLength
            | Text.null suffix = 0
            | otherwise = max 1 (fromMaybe 0 (readMaybe (toString suffix)))
      pure $
        if requestedLength > 1400
          then DirectLibraryInvalid "单次发文最多 1400 字。"
          else DirectLibraryArticle difficulty requestedLength

parseDirectSingle :: Text -> Maybe DirectLibraryCommand
parseDirectSingle body =
  firstJust (parseName <$> singleSetNames)
  where
    parseName name = do
      suffix <- Text.stripPrefix name body
      let (lengthText, afterLength) = Text.span isDigit suffix
          requestedLength
            | Text.null lengthText = 100
            | otherwise = max 1 (fromMaybe 0 (readMaybe (toString lengthText)))
          (orderName, afterOrder)
            | Just rest <- Text.stripPrefix "顺" afterLength = ("顺", rest)
            | Just rest <- Text.stripPrefix "乱" afterLength = ("乱", rest)
            | otherwise = ("乱", afterLength)
      (keyReq, afterKey) <- parseOptionalMetric "击" afterOrder
      (accReq, remaining) <- parseOptionalMetric "准" afterKey
      guard (Text.null remaining)
      pure $
        if requestedLength > 1400
          then DirectLibraryInvalid "单次发文最多 1400 字。"
          else DirectLibrarySingle name requestedLength orderName keyReq accReq

singleSetNames :: [Text]
singleSetNames =
  [ "前1500", "前500", "中500", "后500", "黄500", "玄500"
  , "地500", "天500", "王500", "皇500", "帝500"
  ]

parseOptionalMetric :: Text -> Text -> Maybe (Double, Text)
parseOptionalMetric marker value =
  case Text.stripPrefix marker value of
    Nothing -> Just (0, value)
    Just suffix -> do
      let (number, remaining) = Text.span (\char -> isDigit char || char == '.') suffix
      parsed <- readMaybe (toString number)
      pure (parsed, remaining)

firstJust :: [Maybe a] -> Maybe a
firstJust =
  viaNonEmpty head . catMaybes

checkBotGuard :: HTTP.HTTP :> es => IncomingMessage -> Eff es (Maybe Text)
checkBotGuard message = do
  let explicit = message.digest.mentionsBot || "fm" `Text.isInfixOf` Text.toLower message.text
  response <- postJson "bot-guard" "check" (Aeson.object
    [ "group_id" Aeson..= maybe ("" :: Text) show message.chatId
    , "sender_id" Aeson..= fromMaybe "" message.senderId
    , "explicit" Aeson..= explicit
    ])
  pure $ AesonTypes.parseMaybe parse response >>= \(blocked, reply) ->
    reply <$ guard blocked
  where
    parse = Aeson.withObject "FM bot guard result" \o ->
      (,) <$> o Aeson..: "blocked" <*> o Aeson..:? "reply" Aeson..!= ""

checkRepeatFollow :: HTTP.HTTP :> es => IncomingMessage -> Eff es (Maybe Text)
checkRepeatFollow message = do
  response <- postJson "repeat-follow" "check" (Aeson.object
    [ "group_id" Aeson..= maybe ("" :: Text) show message.chatId
    , "sender_id" Aeson..= fromMaybe "" message.senderId
    , "text" Aeson..= message.text
    , "has_media" Aeson..= (not (null message.imageUrls) || not (null message.files))
    , "mentions" Aeson..= (not (null message.mentions) || message.digest.mentionsBot)
    ])
  pure $ AesonTypes.parseMaybe parse response >>= \(repeated, text) ->
    text <$ guard (repeated && not (Text.null text))
  where
    parse = Aeson.withObject "FM repeat result" \o ->
      (,) <$> o Aeson..: "repeat" <*> o Aeson..:? "text" Aeson..!= ""

setRepeatEnabled :: HTTP.HTTP :> es => Text -> Bool -> Eff es Bool
setRepeatEnabled groupId enabled = do
  response <- postJson "repeat-follow" "state" (Aeson.object
    [ "group_id" Aeson..= groupId
    , "enabled" Aeson..= enabled
    ])
  pure (fromMaybe enabled (AesonTypes.parseMaybe (Aeson.withObject "FM repeat state" (Aeson..: "enabled")) response))

repeatEnabled :: HTTP.HTTP :> es => Text -> Eff es Bool
repeatEnabled groupId = do
  response <- HTTP.runReq $
    req GET (http "172.20.0.4" /: "repeat-follow" /: "state") NoReqBody jsonResponse
      ("group_id" =: groupId <> port 8077)
  pure . fromMaybe True . AesonTypes.parseMaybe (Aeson.withObject "FM repeat state" (Aeson..: "enabled")) $
    (responseBody response :: Aeson.Value)

groupCapabilityEnabled :: HTTP.HTTP :> es => Text -> Text -> Eff es Bool
groupCapabilityEnabled groupId capability = do
  response <- HTTP.runReq $
    req GET (http "172.20.0.4" /: "group-capability") NoReqBody jsonResponse
      ("group_id" =: groupId <> "capability" =: capability <> port 8077)
  pure . fromMaybe True . AesonTypes.parseMaybe (Aeson.withObject "FM group capability" (Aeson..: "enabled")) $
    (responseBody response :: Aeson.Value)

postJson :: HTTP.HTTP :> es => Text -> Text -> Aeson.Value -> Eff es Aeson.Value
postJson section action payload = do
  response <- HTTP.runReq $
    req POST (http "172.20.0.4" /: section /: action)
      (ReqBodyJson payload) jsonResponse (port 8077)
  pure (responseBody response :: Aeson.Value)

postLibraryStop :: HTTP.HTTP :> es => Aeson.Value -> Eff es Aeson.Value
postLibraryStop payload = do
  response <- HTTP.runReq $
    req POST (http "172.20.0.4" /: "library" /: "session" /: "stop")
      (ReqBodyJson payload) jsonResponse (port 8077)
  pure (responseBody response :: Aeson.Value)

messagePayload :: IncomingMessage -> Aeson.Value
messagePayload message =
  Aeson.object
    [ "platform" Aeson..= chatPlatformKey message.platform
    , "message_id" Aeson..= maybe "" messageIdText message.messageId
    , "group_id" Aeson..= maybe ("" :: Text) show message.chatId
    , "sender_id" Aeson..= fromMaybe "" message.senderId
    , "sender_name" Aeson..= fromMaybe "" message.senderUsername
    , "text" Aeson..= message.text
    , "image_urls" Aeson..= message.imageUrls
    , "raw" Aeson..= message.raw
    ]

postEvent :: HTTP.HTTP :> es => Text -> Aeson.Value -> Eff es Aeson.Value
postEvent eventName payload = do
  response <- HTTP.runReq $
    req POST (http "172.20.0.4" /: "events" /: eventName)
      (ReqBodyJson payload) jsonResponse (port 8077)
  pure (responseBody response :: Aeson.Value)

jsonFlag :: Text -> Aeson.Value -> Bool
jsonFlag field =
  fromMaybe False . AesonTypes.parseMaybe
    (Aeson.withObject "FM event response" (\object -> do
      value <- object Aeson..:? AesonKey.fromText field
      pure (fromMaybe False value)))

setGroupOnline :: HTTP.HTTP :> es => Text -> Bool -> Eff es ()
setGroupOnline groupId online = do
  response <- HTTP.runReq $
    req POST (http "172.20.0.4" /: "group-state")
      (ReqBodyJson (Aeson.object ["group_id" Aeson..= groupId, "online" Aeson..= online]))
      jsonResponse
      (port 8077)
  void . pure $ (responseBody response :: Aeson.Value)

groupOnline :: HTTP.HTTP :> es => Text -> Eff es Bool
groupOnline groupId = do
  response <- HTTP.runReq $
    req GET (http "172.20.0.4" /: "group-state") NoReqBody jsonResponse
      ("group_id" =: groupId <> port 8077)
  pure . fromMaybe True . AesonTypes.parseMaybe parseOnline $ (responseBody response :: Aeson.Value)
  where
    parseOnline = Aeson.withObject "FM group state" (Aeson..: "online")
