module Bot.Agent.Tools.FMDomain
  ( fmGroupStatusTool
  , fmLibrarySearchTool
  , fmLibraryPickTool
  , fmLibraryStartTool
  , fmLibraryContinueTool
  , fmLibraryContinueSameTool
  , fmLibraryContinuePreviousTool
  , fmLibraryStopTool
  , fmLibraryRecallRecentTool
  , fmLibraryStatsTool
  , fmContestChartTool
  , typingStatsTool
  , fanqieReadingTool
  , fmRecallQueryTool
  , fmScoreQueryTool
  , fmGroupSetOnlineTool
  , fmGroupSetCapabilityTool
  , fmContestSearchTool
  , fmContestSendTool
  , fmLiveCompetitionRankTool
  , fmLiveCompetitionTextTool
  , fmAiContestTextTool
  , fmAiContestPublishTool
  , fmAiContestLeaderboardTool
  , fmAiContestLeaderboardImageTool
  , fmCompetitionScoreQueryTool
  , fmCompetitionScoreSummaryTool
  , fmScoreAnalysisTool
  , fmCompetitionScoreImageTool
  , fmChartTool
  , fmBotGuardAccountsTool
  , fmDomainStatsTool
  , fmAdminStatusTool
  , fmSelfReviewTool
  , fmSelfNotesTool
  , fmSelfNoteTool
  ) where

import Bot.Agent.Tool
import Bot.Agent.Tools.Common
  ( jsonText
  , optionalBoolean
  , fieldBoolean
  , optionalInt
  , optionalText
  , requiredText
  , superuserOnly
  )
import Bot.Agent.Types (Context (..), ToolResult, toolFailure, toolText)
import qualified Bot.Agent.Failure as Failure
import Bot.Core.Message
  ( ChatPlatform (PlatformQQ)
  , IncomingMessage (..)
  , MessageDigest (..)
  , MessageInput (..)
  , chatPlatformKey
  , messageIdText
  , textMessageId
  )
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.HTTP as HTTP
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.List as List
import Data.Char (isAlphaNum)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import qualified System.Directory as Directory
import Network.HTTP.Req
import Network.HTTP.Types.URI (urlEncode)

fmGroupStatusTool :: HTTP.HTTP :> es => Tool (Eff es)
fmGroupStatusTool =
  withDescription "Read FM group status and enabled capabilities from the migrated FM data service."
    $ tool "fm_group_status" noArguments do
      response <- HTTP.runReq $
        req GET (http "172.20.0.4" /: "groups") NoReqBody jsonResponse (port 8077)
      pure . toolText . jsonText $ (responseBody response :: [Aeson.Value])

fmLibrarySearchTool :: HTTP.HTTP :> es => Tool (Eff es)
fmLibrarySearchTool =
  allowWhen hasExplicitLibraryIntent
  . withDescription "Search FM's migrated typing-practice library by title, topic, or content keyword. Available only when the current user message explicitly asks for typing-practice text, an article, 文来, 发文, or the FM library; never reinterpret an unrelated topic as a library search."
    $ tool "fm_library_search"
      ( requiredText "query" "Title, topic, or keyword."
      , optionalInt "limit" "Maximum results, from 1 to 30."
      )
      \query requestedLimit -> do
        context <- askToolContext
        withCapability context "library" do
          let limit = max 1 (min 30 (fromMaybe 10 requestedLimit))
          response <- HTTP.runReq $
            req GET (http "172.20.0.4" /: "library" /: "search") NoReqBody jsonResponse
              ("q" =: query <> "limit" =: limit <> port 8077)
          pure . toolText . jsonText $ (responseBody response :: [Aeson.Value])

fmLibraryPickTool :: HTTP.HTTP :> es => Tool (Eff es)
fmLibraryPickTool =
  allowWhen hasExplicitLibraryIntent
  . withDescription "Select one complete typing-practice text from FM's library matching a natural-language source, date, or topic. Available only for an explicit typing-practice article or FM-library request."
    $ tool "fm_library_pick" (requiredText "query" "Desired title, topic, style, or keyword.")
      \query -> do
        context <- askToolContext
        withCapability context "library" do
          response <- HTTP.runReq $
            req GET (http "172.20.0.4" /: "library" /: "pick") NoReqBody jsonResponse
              ("q" =: query <> port 8077)
          pure . toolText . jsonText $ (responseBody response :: Aeson.Value)

fmLibraryStartTool :: (Chat.Chat :> es, HTTP.HTTP :> es) => Tool (Eff es)
fmLibraryStartTool =
  allowWhen hasExplicitLibraryIntent
  . withDescription "Open a NEW typing-practice article session in the current chat and send its first segment. THIS is the tool to use whenever the current message asks FM to send an article in any phrasing -- 来一篇、来篇、来点文、发一篇、开一篇、文来、发文、文章、练习文, or a difficulty word such as 水文/水档/来篇水文 -- and also whenever no session is in progress yet. Passing difficulty (淼/水/易/普/难/虐) picks strictly inside that interval; leave query empty then. The selected mode persists after typing scores. A topic mentioned in ordinary conversation is not a request to send an article. If a session is already running and the user asks for another article, use fm_library_continue instead. The tool sends the article itself; do not repeat the article body in the final response."
  $ tool "fm_library_start"
      ( optionalText "query" "Optional title, topic, style, or content keyword. Leave empty for 文来 and difficulty commands."
      , optionalText "difficulty" "Optional strict difficulty mode: 淼、水、易、普、难、虐. Empty means random 文来."
      , optionalInt "length" "Optional segment length from 1 to 1400 characters; omitted means 200 to 400."
      )
      \query difficulty requestedLength -> do
        context <- askToolContext
        withCapability context "library" do
          response <- postLibrarySession "start" (libraryStartPayload context
            (fromMaybe "" query) (fromMaybe "" difficulty) (fromMaybe 0 requestedLength))
          sendLibrarySegment context response

fmLibraryContinueTool :: (Chat.Chat :> es, HTTP.HTTP :> es) => Tool (Eff es)
fmLibraryContinueTool =
  withDescription "Continue an ALREADY RUNNING typing-practice session by selecting and sending the next article. Requires a session currently in progress in this chat for this person. If it answers that there is no article in progress, that is NOT a limitation of FM: immediately call fm_library_start to open one, in the same turn, and never report the absence as something the system cannot do. Never use this tool to open the first article. Typically used right after a typing score is submitted. A requested difficulty selects another article strictly inside that same interval."
  $ tool "fm_library_continue" noArguments do
      context <- askToolContext
      withCapability context "library" do
        response <- postLibrarySession "continue" (librarySessionPayload context)
        sendLibrarySegment context response

fmLibraryContinueSameTool :: (Chat.Chat :> es, HTTP.HTTP :> es) => Tool (Eff es)
fmLibraryContinueSameTool =
  withDescription "Continue with the next segment of the currently displayed FM library article. Use only for 当前这篇、这一篇、这篇文. Never use this for 上一篇、刚才成绩前那篇, because those require fm_library_continue_previous. If the article has ended, report the service's question about returning to the previous random or difficulty mode."
  $ tool "fm_library_continue_same" noArguments do
      context <- askToolContext
      withCapability context "library" do
        response <- postLibrarySession "continue-same" (librarySessionPayload context)
        sendLibrarySegment context response

fmLibraryContinuePreviousTool :: (Chat.Chat :> es, HTTP.HTTP :> es) => Tool (Eff es)
fmLibraryContinuePreviousTool =
  withDescription "Restore and continue the article from before the current newly generated article. Use whenever the user says 上一篇、继续打上一篇、刚才成绩前那篇, especially after a typing score caused FM to auto-select a new article. This recalls the currently displayed new article first, then sends the previous article from its saved offset."
  $ tool "fm_library_continue_previous" noArguments do
      context <- askToolContext
      withCapability context "library" do
        response <- postLibrarySession "continue-previous" (librarySessionPayload context)
        sendPreviousLibrarySegment context response

fmLibraryStopTool :: (Chat.Chat :> es, HTTP.HTTP :> es) => Tool (Eff es)
fmLibraryStopTool =
  withDescription "Stop the active FM typing-practice article session in the current chat. It also attempts to recall the most recently sent article segment. Use when the user naturally asks FM to stop sending the article."
  $ tool "fm_library_stop" noArguments do
      context <- askToolContext
      withCapability context "library" do
        response <- postLibrarySession "stop" (librarySessionPayload context)
        let status = fromMaybe ("error" :: Text) $ AesonTypes.parseMaybe
              (Aeson.withObject "FM library stop" (Aeson..: "status")) response
            message = fromMaybe ("发文停止失败。" :: Text) $ AesonTypes.parseMaybe
              (Aeson.withObject "FM library stop" \o -> o Aeson..:? "message" Aeson..!= "") response
            maybeMessageId = AesonTypes.parseMaybe
              (Aeson.withObject "FM library stop" \o -> o Aeson..:? "last_message_id") response
              >>= id
        if status /= "stopped"
          then pure (toolText message)
          else do
            recalled <- case Text.strip <$> maybeMessageId of
              Just messageId | not (Text.null messageId) ->
                Chat.deleteMessage context.message (textMessageId messageId)
              _ -> pure False
            when recalled $
              for_ (Text.strip <$> maybeMessageId) \messageId ->
                void $ postLibrarySession "recalled" (libraryRecallPayload context 1 [messageId])
            pure . toolText $
              if recalled
                then "发文已停止，最近一段也已撤回。"
                else "发文已停止，但最近一段撤回失败。"

fmLibraryRecallRecentTool :: (Chat.Chat :> es, HTTP.HTTP :> es) => Tool (Eff es)
fmLibraryRecallRecentTool =
  withDescription "Recall FM's own recently sent typing-practice article messages in the current QQ chat. Use this tool whenever the user asks to 撤回刚刚的发文、撤回这几条文章、删除刚才 FM 发的练习文. Do not use chat_log or fm_recall_query for this request. QQ normally allows recalling FM's own messages only within two minutes."
  $ tool "fm_library_recall_recent"
      (optionalInt "count" "Number of recent article messages to recall, from 1 to 20; default 5.")
      \requestedCount -> do
        context <- askToolContext
        withCapability context "library" do
          let count = max 1 (min 20 (fromMaybe 5 requestedCount))
          response <- postLibrarySession "recall-recent" (libraryRecallPayload context count [])
          let messageIds = fromMaybe ([] :: [Text]) $ AesonTypes.parseMaybe
                (Aeson.withObject "FM recent library messages" \o -> o Aeson..:? "message_ids" Aeson..!= []) response
          results <- forM messageIds \messageId -> do
            succeeded <- Chat.deleteMessage context.message (textMessageId messageId)
            pure (messageId, succeeded)
          let recalledIds = map fst (filter snd results)
              failedCount = length results - length recalledIds
          unless (null recalledIds) $
            void $ postLibrarySession "recalled" (libraryRecallPayload context count recalledIds)
          pure . toolText $
            case (length recalledIds, failedCount) of
              (0, 0) -> "最近两分钟没有可撤回的发文。"
              (0, failed) -> "最近的发文撤回失败，共 " <> show failed <> " 条；可能已经超过 QQ 的两分钟撤回时限。"
              (recalled, 0) -> "已成功撤回最近 " <> show recalled <> " 条发文。"
              (recalled, failed) ->
                "已撤回 " <> show recalled <> " 条发文，另有 " <> show failed <> " 条撤回失败，可能已经超过 QQ 的两分钟时限。"

fmLibraryStatsTool :: HTTP.HTTP :> es => Tool (Eff es)
fmLibraryStatsTool =
  withDescription "Read FM typing-practice library statistics, including total texts, characters, difficulty indexing, and active article sessions."
  $ tool "fm_library_stats" noArguments do
      response <- HTTP.runReq $
        req GET (http "172.20.0.4" /: "library" /: "stats") NoReqBody jsonResponse (port 8077)
      pure . toolText . jsonText $ (responseBody response :: Aeson.Value)

librarySessionPayload :: Context -> Aeson.Value
librarySessionPayload context =
  Aeson.object
    [ "platform" Aeson..= chatPlatformKey context.message.platform
    , "chat_id" Aeson..= maybe ("" :: Text) show context.message.chatId
    , "requester_id" Aeson..= fromMaybe "" context.message.senderId
    , "requester_name" Aeson..= fromMaybe "" context.message.senderUsername
    , "owner" Aeson..= context.message.digest.senderIsSuperuser
    ]

libraryRecallPayload :: Context -> Int -> [Text] -> Aeson.Value
libraryRecallPayload context count messageIds =
  Aeson.object
    [ "platform" Aeson..= chatPlatformKey context.message.platform
    , "chat_id" Aeson..= maybe ("" :: Text) show context.message.chatId
    , "requester_id" Aeson..= fromMaybe "" context.message.senderId
    , "requester_name" Aeson..= fromMaybe "" context.message.senderUsername
    , "owner" Aeson..= context.message.digest.senderIsSuperuser
    , "count" Aeson..= count
    , "message_ids" Aeson..= messageIds
    ]

libraryStartPayload :: Context -> Text -> Text -> Int -> Aeson.Value
libraryStartPayload context query difficulty length =
  Aeson.object
    [ "platform" Aeson..= chatPlatformKey context.message.platform
    , "chat_id" Aeson..= maybe ("" :: Text) show context.message.chatId
    , "requester_id" Aeson..= fromMaybe "" context.message.senderId
    , "requester_name" Aeson..= fromMaybe "" context.message.senderUsername
    , "owner" Aeson..= context.message.digest.senderIsSuperuser
    , "query" Aeson..= query
    , "difficulty" Aeson..= difficulty
    , "length" Aeson..= length
    ]

postLibrarySession :: HTTP.HTTP :> es => Text -> Aeson.Value -> Eff es Aeson.Value
postLibrarySession action payload = do
  response <- HTTP.runReq $
    req POST (http "172.20.0.4" /: "library" /: "session" /: action)
      (ReqBodyJson payload) jsonResponse (port 8077)
  pure (responseBody response :: Aeson.Value)

sendLibrarySegment
  :: (Chat.Chat :> es, HTTP.HTTP :> es)
  => Context
  -> Aeson.Value
  -> Eff es ToolResult
sendLibrarySegment context response = do
  let status = fromMaybe ("error" :: Text) $ AesonTypes.parseMaybe
        (Aeson.withObject "FM library segment" (Aeson..: "status")) response
      message = fromMaybe ("文章生成失败。" :: Text) $ AesonTypes.parseMaybe
        (Aeson.withObject "FM library segment" \o -> o Aeson..:? "message" Aeson..!= "") response
      sessionId = fromMaybe ("" :: Text) $ AesonTypes.parseMaybe
        (Aeson.withObject "FM library segment" \o -> o Aeson..:? "session_id" Aeson..!= "") response
  if status /= "segment"
    then pure (toolText message)
    else do
      sent <- Chat.replyTo (standaloneLibraryMessage context.message) message
      case rights sent of
        sentMessageIdRaw : _ -> do
          let sentMessageId = messageIdText sentMessageIdRaw
          unless (Text.null sessionId) $
            void $ postLibrarySession "sent" (Aeson.object
              [ "session_id" Aeson..= sessionId
              , "message_id" Aeson..= sentMessageId
              ])
          pure (toolText "文章已发送。不要在最终回复中重复文章正文。")
        [] ->
          pure . toolText $
            "文章已经生成，但发送失败：" <> Text.intercalate "；" (lefts sent)

sendPreviousLibrarySegment
  :: (Chat.Chat :> es, HTTP.HTTP :> es)
  => Context
  -> Aeson.Value
  -> Eff es ToolResult
sendPreviousLibrarySegment context response = do
  let status = fromMaybe ("error" :: Text) $ AesonTypes.parseMaybe
        (Aeson.withObject "FM previous library segment" (Aeson..: "status")) response
      maybeMessageId = AesonTypes.parseMaybe
        (Aeson.withObject "FM previous library segment" \o -> o Aeson..:? "recall_message_id") response
        >>= id
  recalled <- if status /= "segment"
    then pure Nothing
    else case Text.strip <$> maybeMessageId of
      Just messageId | not (Text.null messageId) ->
        Just <$> Chat.deleteMessage context.message (textMessageId messageId)
      _ -> pure Nothing
  when (recalled == Just True) $
    for_ (Text.strip <$> maybeMessageId) \messageId ->
      void $ postLibrarySession "recalled" (libraryRecallPayload context 1 [messageId])
  sentResult <- sendLibrarySegment context response
  pure case recalled of
    Just False -> toolText "上一篇已恢复并发送，但刚才的新文章撤回失败；请明确告知用户。"
    _ -> sentResult

standaloneLibraryMessage :: IncomingMessage -> IncomingMessage
standaloneLibraryMessage message =
  message
    { messageId = Nothing
    , replyToMessageId = Nothing
    , raw = Aeson.Null
    }

-- | 番茄小说专区（2026-11-05）。
--
-- 所有者要的用法：
--   fm 打番茄小说斩神      → action=start book=斩神
--   fm 继续                → action=continue
--   fm 跳到第 100 章       → action=jump chapter=100
--   fm 我打到哪了          → action=progress
--   fm 这本书均速多少      → action=stats
--
-- 书不在库里时由 domain 返回 not_found，工具把「让 fix 哥加上」原样交给模型，
-- 不要自己编一个「系统不支持」的说法。
novelReadingPayload :: Context -> Text -> Maybe Int -> Maybe Int -> Bool -> Aeson.Value
novelReadingPayload context book maybeChapter maybeLength restart =
  Aeson.object $
    [ "platform" Aeson..= chatPlatformKey context.message.platform
    , "chat_id" Aeson..= maybe ("" :: Text) show context.message.chatId
    , "requester_id" Aeson..= fromMaybe "" context.message.senderId
    , "requester_name" Aeson..= fromMaybe "" context.message.senderUsername
    , "book" Aeson..= book
    ]
      <> maybe [] (\chapter -> ["chapter" Aeson..= chapter]) maybeChapter
      <> maybe [] (\len -> ["length" Aeson..= len]) maybeLength
      <> (if restart then ["restart" Aeson..= True] else [])

-- | 记住「这个会话上一次发的番茄段落的 message id」，重开时要撤它。
readNovelLastId :: FilePath -> IO (Maybe Text)
readNovelLastId path = do
  exists <- Directory.doesFileExist path
  if not exists then pure Nothing else Just <$> TextIO.readFile path

writeNovelLastId :: FilePath -> Text -> IO ()
writeNovelLastId path value = do
  Directory.createDirectoryIfMissing True "/data"
  TextIO.writeFile path value

postNovel :: HTTP.HTTP :> es => Text -> Aeson.Value -> Eff es Aeson.Value
postNovel action payload = do
  response <- HTTP.runReq $
    req POST (http "172.20.0.4" /: "novel" /: action)
      (ReqBodyJson payload) jsonResponse (port 8077)
  pure (responseBody response :: Aeson.Value)

novelStatus :: Aeson.Value -> Text
novelStatus response =
  fromMaybe "error" $
    AesonTypes.parseMaybe
      (Aeson.withObject "novel response" \o -> o Aeson..:? "status" Aeson..!= "")
      response

novelMessage :: Aeson.Value -> Text
novelMessage response =
  fromMaybe "番茄专区请求失败。" $
    AesonTypes.parseMaybe
      (Aeson.withObject "novel response" \o -> o Aeson..:? "message" Aeson..!= "")
      response

novelFieldBool :: Text -> Aeson.Value -> Bool
novelFieldBool key response =
  fromMaybe False $
    AesonTypes.parseMaybe
      (Aeson.withObject "novel response" \o -> o Aeson..:? "restarted" Aeson..!= False)
      response

novelFieldList :: Aeson.Value -> [Text]
novelFieldList response =
  fromMaybe [] $
    AesonTypes.parseMaybe
      (Aeson.withObject "novel response" \o -> o Aeson..:? "book_lines" Aeson..!= [])
      response

novelChapterText :: Aeson.Value -> Text
novelChapterText response =
  fromMaybe "" $
    AesonTypes.parseMaybe
      (Aeson.withObject "novel response" \o -> o Aeson..:? "chapter_index" Aeson..!= "")
      response

-- | 查打字成绩（2026-11-06）。
--
-- 所有用户的成绩都已入库（全字段 + 原始文本），这里把常见问法封装成四个 scope。
-- 所有者原话：「成绩不是专门用来只记均速的，是保留全部，可以用来分析」——
-- 所以除了均速，还报击键、码长、打词、键准和趋势，够回答大多数分析问题。
typingStatsPayload :: Context -> Text -> Int -> Text -> Int -> Aeson.Value
typingStatsPayload context scope days book limit =
  Aeson.object
    [ "platform" Aeson..= chatPlatformKey context.message.platform
    , "chat_id" Aeson..= maybe ("" :: Text) show context.message.chatId
    , "requester_id" Aeson..= fromMaybe "" context.message.senderId
    , "requester_name" Aeson..= fromMaybe "" context.message.senderUsername
    , "scope" Aeson..= scope
    , "days" Aeson..= days
    , "book" Aeson..= book
    , "limit" Aeson..= limit
    ]

postTypingStats :: HTTP.HTTP :> es => Aeson.Value -> Eff es Aeson.Value
postTypingStats payload = do
  response <- HTTP.runReq $
    req POST (http "172.20.0.4" /: "typing" /: "stats")
      (ReqBodyJson payload) jsonResponse (port 8077)
  pure (responseBody response :: Aeson.Value)

typingStatsTool :: HTTP.HTTP :> es => Tool (Eff es)
typingStatsTool =
  withDescription
    "查打字成绩与趋势。随便问「我这周打得怎样」「我均速多少」「最近怎么样」\
    \「群里谁最快」「这本书我打得怎么样」都用它。\n\
    \scope:\n\
    \· me      —— 我的成绩汇总（均速/最快/键准/击键/码长/打词/累计字数）。可给 days。\n\
    \· trend   —— 按天的趋势，默认 14 天。\n\
    \· ranking —— 群里均速排行（默认最近 7 天，至少 50 段才上榜）。\n\
    \· book    —— 某本书我的成绩，需要 book（书名关键词）。\n\
    \返回的是可以直接念的中文，不要自己再算一遍。"
    $ tool "typing_stats"
        ( requiredText "scope" "One of: me, trend, ranking, book."
        , optionalInt "days" "时间范围（天）。me 默认全部，trend 默认 14，ranking 默认 7。"
        , optionalText "book" "书名关键词，scope=book 时必填。"
        )
        \rawScope maybeDays book -> do
          context <- askToolContext
          let scope = Text.toLower (Text.strip rawScope)
              days = fromMaybe 0 maybeDays
          if scope `notElem` ["me", "trend", "ranking", "book"]
            then pure (toolText "scope 只能是 me / trend / ranking / book。")
            else do
              response <- postTypingStats (typingStatsPayload context scope days (fromMaybe "" book) 5)
              let status = novelStatus response
                  message = novelMessage response
              pure (toolText message)

-- | 赛文成绩图（2026-11-06）。
--
-- 所有者要求：网站取不到成绩时，直接从成绩库画。
-- 这个工具就是那条路 —— 数据全部来自 contest_scores，不依赖任何外部站。
--
-- 段号就是赛文身份：
--   全群共用  HB(虎杯) / 100000(锦标赛) / 99999(极速杯) / 100004(极速联赛)
--             mb(晴分身每日) / 666(1916996679每日) / 555(555赛文)
--   每群不同  777(晴分身群赛文) / 999(本群赛文) / groupmatch(1847992065)
-- 按群分的默认用当前群（所有者 2026-11-06 确认）。
fmContestChartTool :: (Chat.Chat :> es) => Tool (Eff es)
fmContestChartTool =
  allowWhen superuserOnly
    . withDescription
      "Draw a 赛文 chart -- ranking bars, or a per-person score **curve / 趋势 / 走势** when \
      \mode=trend. Use this for ANY request that names a 赛文 (群赛文/本群赛文/虎杯/锦标赛/极速杯/\
      \极速联赛/晴分身/666/777/groupmatch) and wants 成绩/排行/曲线/趋势. Data comes from the \
      \official site first and falls back to FM's contest table, so it works even when the site \
      \is down. Do NOT use fm_chart for these -- fm_chart only knows 555赛文.\n\
      \contest: the 段号 or 赛文 name. Known: \
      \HB(虎杯) 100000(锦标赛) 99999(极速杯) 100004(极速联赛) mb 666 777 999 groupmatch.\n\
      \重要：\n\
      \· 「群赛文」「本群赛文」「这个群自己的赛文」「<某个群名>的群赛文」= **999**，\n\
      \  例如「梦幻打字阁的群赛文」就是 contest=999（不要传 555）。\n\
      \· 「555」「AI赛文」才用 contest=555。\n\
      \· 「晴分身」= 777；「1916996679」= 666；「1847992065」= groupmatch。\n\
      \· 用户说「曲线」「走势」「趋势」时 mode=trend；否则用默认排行榜。\n\
      \· 曲线默认画**当前发言人自己**的；用户说「XXX的曲线」就传 user=XXX。\n\
      \每群不同的 777 / 999 / groupmatch 默认用当前群；需要别的群时传 group_id。\n\
      \days: 天数，0 或不传表示全部时间。\n\
      \画完直接把图发出去，最多补一句「这是全部/最近N天的成绩」，不要自己复述名单。"
    $ tool "fm_contest_chart"
        ( requiredText "contest" "赛文段号。999=群赛文/本群赛文（最常用）；HB=虎杯；100000=锦标赛；99999=极速杯；100004=极速联赛；666=1916996679；777=晴分身；groupmatch=1847992065；mb=晴分身每日。注意：555 是 FM 自己的 AI 赛文，不属于这个工具。"
        , optionalInt "days" "统计天数，0 或不传 = 全部时间。"
        , optionalText "group_id" "只有 777/999/groupmatch 需要；不传就用当前群。"
        , optionalText "mode" "rank=排行榜柱状图（默认），trend=按天成绩曲线（用户说「曲线」「走势」时用）。"
        , optionalText "user" "看谁的成绩曲线。用户说「我的」或不指定就留空（= 当前发言人）；说别人名字就传名字关键字。"
        )
        \contest days maybeGroupId maybeMode maybeUser -> do
          context <- askToolContext
          -- 2026-11-06：模型反复把「群赛文」传成 555（555 是 FM 自己的 AI 赛文，
          -- 和群赛文是两回事）。这里直接读用户原话纠正，不再依赖模型判断。
          let inputLower = Text.toLower (context.input.text)
              groupContestMentioned =
                any (`Text.isInfixOf` inputLower)
                  ["群赛文", "本群赛文", "这个群的赛文", "群里的赛文", "本群的赛文"]
              contestText =
                if groupContestMentioned then "999" else Text.strip contest
              chatIdText = maybe "" (Text.pack . show) context.message.chatId
              groupText = fromMaybe chatIdText maybeGroupId
              dayText = Text.pack (show (fromMaybe 0 days))
              -- 默认看「当前发言人」自己的曲线（所有者 2026-11-06：
              -- 他问的个人成绩曲线，结果我画成全群平均了 ✗）
              userText =
                case Text.strip (fromMaybe "" maybeUser) of
                  "" -> Text.strip (fromMaybe "" context.message.senderUsername)
                  other -> other
              url =
                "http://172.20.0.4:8077/contest/chart.png?contest="
                  <> Text.strip contest
                  <> "&group_id=" <> groupText
                  <> "&days=" <> dayText
                  <> "&mode=" <> (if Text.strip (fromMaybe "rank" maybeMode) == "trend" then "trend" else "rank")
                  <> "&user=" <> urlEncodeText userText
          void $ Chat.replyTo (standaloneLibraryMessage context.message) (ReplyBody.imageDirective url)
          pure . toolText $
            "已按成绩库画好并发出：" <> contestText
              <> (if Text.null (Text.strip groupText) then "（全群）" else "（群 " <> groupText <> "）")
              <> "。不要复述名单。"

fanqieReadingTool :: (Chat.Chat :> es, HTTP.HTTP :> es, IOE :> es) => Tool (Eff es)
fanqieReadingTool =
  withDescription
    "番茄小说专区：发网文给用户做**打字跟打练习**（用户说「打番茄小说」「打XX书」就是它）。\n\
    \注意：这是「发文章给用户打」，**不是下载**；书不在库里就回复「让 fix 哥加上」，\n\
    \绝不要调用任何下载工具，也不要说什么「后台下载中」。\n\
    \Actions:\n\
    \· start   —— 开始/继续打某本小说。需要 book（书名或其中一段，如「斩神」）。\
    \  书不存在的直接告诉用户「让 fix 哥加上」，不要说成系统不支持。\n\
    \· continue —— 接着上次的进度发下一段。\n\
    \· next    —— 下一章。\n\
    \· jump    —— 跳到第 N 章，需要 chapter（书的章号）。\n\
    \· progress —— 我打到哪了。\n\
    \· stats   —— 这本书我的均速/准确率。\n\
    \· books   —— 列出书库里所有番茄小说（用户问「有哪些书」时用）。\n\
    \用户说「每段200字」就传 length=200，之后会记住这个偏好。\n\
    \工具自己会把正文发到当前会话；最终回复最多一句确认，绝不重复正文。"
    $ tool "fanqie_reading"
        ( requiredText "action" "One of: start, continue, next, jump, progress, stats, books."
        , optionalText "book" "书名关键词，start 和 stats 用。"
        , optionalInt "chapter" "书的章号，jump 用。"
        , optionalInt "length" "每段多少字（1~1400）。用户说「200字一段」时传它；不传就沿用上次的。"
        , optionalBoolean "restart" "用户说「从开头发 / 重新开始这一章」时传 true。"
        )
        \rawAction book maybeChapter maybeLength maybeRestart -> do
          context <- askToolContext
          let action = Text.toLower (Text.strip rawAction)
          if action `notElem` ["start", "continue", "next", "jump", "progress", "stats", "books"]
            then pure (toolText "action 只能是 start / continue / next / jump / progress / stats / books。")
            else do
              let chatKeyText = maybe "nopchat" (Text.pack . show) context.message.chatId
                  lastPath = "/data/novel-last-" <> Text.unpack chatKeyText <> ".txt"
              response <- postNovel action (novelReadingPayload context (fromMaybe "" book) maybeChapter maybeLength (fromMaybe False maybeRestart))
              let status = novelStatus response
                  message = novelMessage response
                  restarted = novelFieldBool "restarted" response
              case status of
                "ok" | action == "books" -> do
                  -- 书单走合并转发（折叠消息），否则 26 本书会把群刷屏
                  let bookLines = novelFieldList response
                  if null bookLines
                    then pure (toolText (novelMessage response))
                    else do
                      let nodes =
                            [ Aeson.object
                                [ "type" Aeson..= ("node" :: Text)
                                , "data" Aeson..= Aeson.object
                                    [ "name" Aeson..= ("FM" :: Text)
                                    , "uin" Aeson..= ("3471095459" :: Text)
                                    , "content" Aeson..= line
                                    ]
                                ]
                            | line <- bookLines
                            ]
                      sent <- Chat.sendMergedForward (standaloneLibraryMessage context.message) nodes
                      pure . toolText $ case sent of
                        Right _ -> "书单已作为一条合并转发（折叠消息）发到会话里，共 " <> Text.pack (show (length bookLines)) <> " 本。不要再把书名一条条列出来。"
                        Left err -> "书单合并转发失败：" <> err <> "\n" <> novelMessage response
                "segment" -> do
                  -- 重开时先把上一条已发的段落撤掉（用户改段长/要求从开头来时）
                  when restarted $ do
                    previous <- liftIO (readNovelLastId lastPath)
                    case previous of
                      Just oldId -> void (Chat.deleteMessage context.message (textMessageId oldId))
                      Nothing -> pure ()
                  sent <- Chat.replyTo (standaloneLibraryMessage context.message) message
                  case rights sent of
                    sentIdRaw : _ -> do
                      liftIO (writeNovelLastId lastPath (messageIdText sentIdRaw))
                      pure . toolText $
                        (if restarted then "已撤回上一段并从本章开头重发。 " else "")
                          <> "已发到当前会话（第" <> novelChapterText response
                          <> "章）。不要复述正文，最多回一句短的。"
                    [] ->
                      pure . toolText $
                        "正文已生成但发送失败：" <> Text.intercalate "；" (lefts sent)
                _ -> pure (toolText message)

fmRecallQueryTool :: (HTTP.HTTP :> es, Chat.Chat :> es) => Tool (Eff es)
fmRecallQueryTool =
  withDescription "Query original QQ messages captured before recall. Use natural-language criteria; all users may query recall records. Returns the recalled messages' text, sender and time. If a recalled message carried images, the record says how many; the images themselves are not included and FM must not try to resend them."
    $ tool "fm_recall_query"
      ( optionalText "group_id" "Optional QQ group ID."
      , optionalText "query" "Optional text keyword."
      )
      \groupId query -> do
        context <- askToolContext
        withCapability context "recall" do
          response <- HTTP.runReq $
            req GET (http "172.20.0.4" /: "recalls") NoReqBody jsonResponse
              ( "group_id" =: fromMaybe "" groupId
                <> "q" =: fromMaybe "" query
                <> port 8077
              )
          let records = responseBody response :: [Aeson.Value]
          -- 这里原本有「把撤回消息里的图顺手再发一遍」的逻辑：
          --
          --     let imageUrls = concatMap recallImageUrls records
          --     when (not (null imageUrls)) $ void $ Chat.replyTo context.message
          --       (Text.intercalate "\n" (map ReplyBody.imageDirective imageUrls))
          --
          -- 已删除，两个原因：
          --   1. 用户问「谁撤回了什么」时不会预期 FM 把撤回的图再贴一遍；
          --      而且那是别人主动撤回的内容。工具说明里也从没提过会转发。
          --   2. 更糟的是它把带 image_urls 的记录交给模型，模型会把里面的
          --      media:mf_xxx 引用写进自己的回复体，再发一次。
          --      实测 09-18 19:26 那两条空文本带图的群消息就是这么来的：
          --      撤回记录里有那两张图，工具转发一次，模型又生成一次。
          --
          -- 现在改成：只给文字记录，图的数量用一句话交代，internal id 不外传。
          let stripImages = map addImageNote
          pure . toolText . jsonText $ Aeson.toJSON (stripImages records)
  where
    addImageNote record = case record of
      Aeson.Object o ->
        let n = case KeyMap.lookup "image_urls" o of
                  Just (Aeson.Array arr) -> length arr
                  _ -> 0
            cleaned = KeyMap.delete "image_urls" o
        in if n > 0
             then Aeson.Object (KeyMap.insert "images" (Aeson.String
                    (Text.pack (show n) <> " 张图片（未包含，不要尝试转发）")) cleaned)
             else Aeson.Object cleaned
      other -> other

fmScoreQueryTool :: HTTP.HTTP :> es => Tool (Eff es)
fmScoreQueryTool =
  withDescription "Query archived typing score records by QQ group and/or sender."
    $ tool "fm_score_query"
      ( optionalText "group_id" "Optional QQ group ID."
      , optionalText "sender_id" "Optional sender QQ ID."
      , optionalInt "limit" "Maximum records, from 1 to 100."
      )
      \groupId senderId requestedLimit -> do
        context <- askToolContext
        withCapability context "scores" do
          let limit = max 1 (min 100 (fromMaybe 20 requestedLimit))
          response <- HTTP.runReq $
            req GET (http "172.20.0.4" /: "scores") NoReqBody jsonResponse
              ( "group_id" =: fromMaybe "" groupId
                <> "sender_id" =: fromMaybe "" senderId
                <> "limit" =: limit
                <> port 8077
              )
          pure . toolText . jsonText $ (responseBody response :: [Aeson.Value])

fmGroupSetOnlineTool :: HTTP.HTTP :> es => Tool (Eff es)
fmGroupSetOnlineTool =
  allowWhen superuserOnly
  . withDescription "Set FM online or offline for one QQ group. This changes persistent group runtime state and is restricted to the owner."
  $ tool "fm_group_set_online"
      ( requiredText "group_id" "QQ group ID."
      , requiredArgument (fieldBoolean "online" "True to bring FM online; false to take FM offline.") :: ToolArgument Bool
      )
      \groupId online -> do
        response <- HTTP.runReq $
          req POST (http "172.20.0.4" /: "group-state")
            (ReqBodyJson (Aeson.object ["group_id" Aeson..= groupId, "online" Aeson..= online]))
            jsonResponse
            (port 8077)
        pure . toolText . jsonText $ (responseBody response :: Aeson.Value)

fmGroupSetCapabilityTool :: HTTP.HTTP :> es => Tool (Eff es)
fmGroupSetCapabilityTool =
  allowWhen superuserOnly
  . withDescription "Enable or disable one persistent FM capability for a QQ group. Valid capabilities: agent, library, contest, ai_contest, scores, recall, repeat."
  $ tool "fm_group_set_capability"
      ( requiredText "group_id" "QQ group ID."
      , requiredText "capability" "One of: agent, library, contest, ai_contest, scores, recall, repeat."
      , requiredArgument (fieldBoolean "enabled" "True to enable the capability; false to disable it.") :: ToolArgument Bool
      )
      \groupId capability enabled -> do
        response <- HTTP.runReq $
          req POST (http "172.20.0.4" /: "group-capability")
            (ReqBodyJson (Aeson.object
              [ "group_id" Aeson..= groupId
              , "capability" Aeson..= capability
              , "enabled" Aeson..= enabled
              ]))
            jsonResponse
            (port 8077)
        pure . toolText . jsonText $ (responseBody response :: Aeson.Value)

fmContestSearchTool :: HTTP.HTTP :> es => Tool (Eff es)
fmContestSearchTool =
  allowWhen hasExplicitContestIntent
  . withDescription "Search FM's preserved typing-contest text library by source group, date, title, topic, or content. Available only when the current message literally mentions a typing contest, 赛文, 比赛, 赛事, 555, a named cup, or a leaderboard. Role-play words such as 案子, 案件, 卷宗, 侦探, or 调查 never mean contest text."
    $ tool "fm_contest_search"
      ( optionalText "query" "Optional title, topic, or content keyword."
      , optionalText "source" "Optional source group or contest name."
      , optionalText "date" "Optional date in YYYY-MM-DD format."
      , optionalInt "limit" "Maximum results, from 1 to 30."
      )
      \query source competitionDate requestedLimit -> do
        context <- askToolContext
        withCapability context "contest" do
          let limit = max 1 (min 30 (fromMaybe 10 requestedLimit))
          response <- HTTP.runReq $
            req GET (http "172.20.0.4" /: "contest" /: "search") NoReqBody jsonResponse
              ( "q" =: fromMaybe "" query
                <> "source" =: fromMaybe "" source
                <> "date" =: fromMaybe "" competitionDate
                <> "limit" =: limit
                <> port 8077
              )
          pure . toolText . jsonText $ (responseBody response :: [Aeson.Value])

fmContestSendTool :: (Chat.Chat :> es, HTTP.HTTP :> es) => Tool (Eff es)
fmContestSendTool =
  allowWhen hasExplicitContestIntent
  . withDescription "Send one complete preserved historical typing-contest text matching a source, date, title, or topic. This is FM's dedicated historical contest sending tool for requests to发/来一篇/练一篇历史赛文, or when the user does not ask for today's/current/online text. For today/current/online contest text use fm_live_competition_text. The tool sends the complete text itself, so do not repeat it."
    $ tool "fm_contest_send"
      ( optionalText "query" "Optional title, topic, or content keyword."
      , optionalText "source" "Optional source group or contest name."
      , optionalText "date" "Optional date in YYYY-MM-DD format."
      )
      \query source competitionDate -> do
        context <- askToolContext
        withCapability context "contest" do
          response <- HTTP.runReq $
            req POST (http "172.20.0.4" /: "contest" /: "session" /: "start")
              (ReqBodyJson (Aeson.object
                [ "platform" Aeson..= chatPlatformKey context.message.platform
                , "chat_id" Aeson..= maybe ("" :: Text) show context.message.chatId
                , "requester_id" Aeson..= fromMaybe "" context.message.senderId
                , "requester_name" Aeson..= fromMaybe "" context.message.senderUsername
                , "query" Aeson..= fromMaybe "" query
                , "source" Aeson..= fromMaybe "" source
                , "date" Aeson..= fromMaybe "" competitionDate
                ])) jsonResponse (port 8077)
          sendLibrarySegment context (responseBody response :: Aeson.Value)

fmLiveCompetitionRankTool :: (Chat.Chat :> es, HTTP.HTTP :> es) => Tool (Eff es)
fmLiveCompetitionRankTool =
  allowWhen hasExplicitContestIntent
  . noisy
  . withDescription "Query and send a complete live public typing-contest leaderboard as one PNG image. Use this autonomously when natural language asks for the current or dated leaderboard of 虎杯, a supported group, 极速杯, or 锦标赛. The source may be 虎杯, a group name, group number, 极速杯, or 锦标赛; when omitted in a supported QQ group, that current group is used. The tool sends the complete image itself, so do not repeat the rows in the final response."
  $ tool "fm_live_competition_rank"
      ( optionalText "source" "Optional public contest source: 虎杯, supported QQ group name/number, 极速杯, or 锦标赛."
      , optionalText "date" "Optional date in YYYY-MM-DD format; defaults to today in China."
      )
      \source competitionDate -> do
        context <- askToolContext
        withCapability context "contest" do
          response <- HTTP.runReq $
            req GET (http "172.20.0.4" /: "competition" /: "live") NoReqBody jsonResponse
              ( "source" =: fromMaybe "" source
                <> "group_id" =: contextChatId context
                <> "date" =: fromMaybe "" competitionDate
                <> "soft" =: True
                <> port 8077
              )
          case AesonTypes.parseMaybe parseLiveRank (responseBody response :: Aeson.Value) of
            Nothing -> pure (toolText "实时排行榜查询失败：领域服务返回格式不正确。")
            Just (status, kind, groupId, actualDate, _, message)
              | status /= "ok" -> pure (toolText ("实时排行榜查询失败：" <> fallback message "公开网站当前没有返回可用数据。"))
              | otherwise -> do
                  let sourceKey = if kind == "group" then groupId else kind
                      imageUrl = "http://172.20.0.4:8077/reports/live-competition.png?source=" <> sourceKey
                        <> "&date=" <> actualDate <> "&combined=1"
                  sent <- Chat.replyTo context.message (ReplyBody.imageDirective imageUrl)
                  case rights sent of
                    _ : _ -> pure (toolText "实时排行榜图片已发送。不要在最终回复中重复排行榜内容。")
                    [] -> do
                      let err = Text.intercalate "；" (lefts sent)
                      pure (toolFailure Failure.Failure
                        { category = Failure.ExternalServiceUnavailable
                        , userMessage = "实时排行榜已生成，但图片发送失败：" <> err
                        , detail = err
                        })
  where
    parseLiveRank :: Aeson.Value -> AesonTypes.Parser (Text, Text, Text, Text, Int, Text)
    parseLiveRank = Aeson.withObject "FM live competition rank" \o ->
      (,,,,,)
        <$> o Aeson..: "status"
        <*> o Aeson..:? "kind" Aeson..!= ""
        <*> o Aeson..:? "group_id" Aeson..!= ""
        <*> o Aeson..:? "date" Aeson..!= ""
        <*> o Aeson..:? "page_count" Aeson..!= 0
        <*> o Aeson..:? "message" Aeson..!= ""
    fallback value defaultValue = if Text.null (Text.strip value) then defaultValue else value

fmLiveCompetitionTextTool :: (Chat.Chat :> es, HTTP.HTTP :> es) => Tool (Eff es)
fmLiveCompetitionTextTool =
  allowWhen hasExplicitLiveContestIntent
  . withDescription "Fetch and send the exact live public typing-contest text for a supported group or 极速杯. Use only when the current message explicitly asks for 赛文, 比赛文章, 赛事文本, or a named typing contest. Never treat 案子, 案件, 卷宗, 侦探, or other persona metaphors as a contest request. 锦标赛 does not expose its text. The tool sends the exact text itself, so do not rewrite or repeat it."
  $ tool "fm_live_competition_text"
      ( optionalText "source" "Optional public contest source: supported QQ group name/number, 极速杯, or 锦标赛."
      , optionalText "date" "Optional date in YYYY-MM-DD format; defaults to today in China."
      )
      \source competitionDate -> do
        context <- askToolContext
        withCapability context "contest" do
          response <- HTTP.runReq $
            req GET (http "172.20.0.4" /: "competition" /: "live" /: "text") NoReqBody jsonResponse
              ( "source" =: fromMaybe "" source
                <> "group_id" =: contextChatId context
                <> "date" =: fromMaybe "" competitionDate
                <> "soft" =: True
                <> port 8077
              )
          case AesonTypes.parseMaybe parseLiveText (responseBody response :: Aeson.Value) of
            Nothing -> pure (toolText "实时赛文查询失败：领域服务返回格式不正确。")
            Just (status, sourceName, title, wordNumber, body, message)
              | status /= "ok" -> pure (toolText ("实时赛文查询失败：" <> fallback message "公开网站当前没有返回赛文正文。"))
              | Text.null (Text.strip body) -> pure (toolText "实时赛文查询失败：公开网站返回了空正文。")
              | otherwise -> do
                  let displaySource = fromMaybe sourceName source
                      formatted =
                        "[FM/赛文·" <> contestDisplayName displaySource <> "] 《" <> title <> "》 [字数" <> show wordNumber <> "]\n\n"
                          <> body <> "\n\n-----第" <> contestShortName displaySource <> "段-FM发文"
                  sent <- Chat.replyTo context.message formatted
                  case rights sent of
                    _ : _ -> pure (toolText "实时赛文已发送。不要在最终回复中重复正文。")
                    [] -> do
                      let err = Text.intercalate "；" (lefts sent)
                      pure (toolFailure Failure.Failure
                        { category = Failure.ExternalServiceUnavailable
                        , userMessage = "实时赛文已取得，但发送失败：" <> err
                        , detail = err
                        })
  where
    parseLiveText :: Aeson.Value -> AesonTypes.Parser (Text, Text, Text, Int, Text, Text)
    parseLiveText = Aeson.withObject "FM live competition text" \o ->
      (,,,,,)
        <$> o Aeson..: "status"
        <*> o Aeson..:? "source" Aeson..!= ""
        <*> o Aeson..:? "title" Aeson..!= "实时赛文"
        <*> o Aeson..:? "word_number" Aeson..!= 0
        <*> (cleanLiveCompetitionBody
          <$> o Aeson..:? "content" Aeson..!= ""
          <*> o Aeson..:? "title" Aeson..!= "")
        <*> o Aeson..:? "message" Aeson..!= ""
    cleanLiveCompetitionBody rawBody title =
      Text.intercalate "\n" . dropTrailingEmptyLines . dropTrailingMarker . dropTitle . Text.lines $ rawBody
      where
        dropTitle (firstLine : rest)
          | Text.strip firstLine == Text.strip title = rest
        dropTitle lines = lines
        dropTrailingMarker = reverse . dropWhile (\line -> "-----" `Text.isPrefixOf` Text.strip line) . reverse
        dropTrailingEmptyLines = reverse . dropWhile Text.null . reverse
    contestDisplayName sourceName
      | "极速杯" `Text.isInfixOf` sourceName = "极速杯"
      | "虎杯" `Text.isInfixOf` sourceName = "虎杯"
      | "锦标赛" `Text.isInfixOf` sourceName = "锦标赛"
      | otherwise = "赛文"
    contestShortName sourceName
      | "极速杯" `Text.isInfixOf` sourceName = "jsb"
      | "虎杯" `Text.isInfixOf` sourceName = "hb"
      | "锦标赛" `Text.isInfixOf` sourceName = "jb"
      | otherwise = "sw"
    fallback value defaultValue = if Text.null (Text.strip value) then defaultValue else value

contextChatId :: Context -> Text
contextChatId context = maybe "" show context.message.chatId

fmAiContestTextTool :: HTTP.HTTP :> es => Tool (Eff es)
fmAiContestTextTool =
  allowWhen hasExplicitContestIntent
  . withDescription "Read FM's persisted daily AI typing-contest text. Available only for an explicit AI赛文 or 555 request; ordinary cases and detective role-play are unrelated. When date is omitted, return the latest saved text."
    $ tool "fm_ai_contest_text" (optionalText "date" "Optional date in YYYY-MM-DD format.")
      \competitionDate -> do
        context <- askToolContext
        withCapability context "ai_contest" do
          response <- HTTP.runReq $
            req GET (http "172.20.0.4" /: "ai-contest" /: "text") NoReqBody jsonResponse
              ("date" =: fromMaybe "" competitionDate <> port 8077)
          pure . toolText . jsonText $ (responseBody response :: Aeson.Value)

fmAiContestPublishTool :: (Chat.Chat :> es, HTTP.HTTP :> es) => Tool (Eff es)
fmAiContestPublishTool =
  allowWhen hasExplicitContestIntent
  . withDescription "Publish and send FM's daily AI typing contest text (segment 555). Use when the user naturally asks for today's AI contest text or uses the retained 555 workflow. If today's text already exists it is reused. When absent, provide a newly written 200-to-350-character Chinese typing text as body, with a short title and the requested difficulty. The tool sends the text itself; do not repeat it in the final response."
  $ tool "fm_ai_contest_publish"
      ( requiredText "body" "A newly written 200-to-350-character Chinese typing text, used only if the requested date has no saved text."
      , optionalText "title" "Optional short title."
      , optionalText "difficulty" "Optional difficulty label."
      , optionalText "date" "Optional date in YYYY-MM-DD format; defaults to today in China."
      )
      \body title difficulty competitionDate -> do
        context <- askToolContext
        withCapability context "ai_contest" do
          response <- HTTP.runReq $
            req POST (http "172.20.0.4" /: "ai-contest" /: "text")
              (ReqBodyJson (Aeson.object
                [ "body" Aeson..= body
                , "title" Aeson..= fromMaybe "" title
                , "difficulty" Aeson..= fromMaybe "普" difficulty
                , "date" Aeson..= fromMaybe "" competitionDate
                ]))
              jsonResponse (port 8077)
          sendAiContestText context (responseBody response :: Aeson.Value)

fmAiContestLeaderboardTool :: HTTP.HTTP :> es => Tool (Eff es)
fmAiContestLeaderboardTool =
  allowWhen hasExplicitContestIntent
  . withDescription "Read FM's persisted 555 AI contest leaderboard for one date, keeping each participant's best score."
    $ tool "fm_ai_contest_leaderboard" (optionalText "date" "Optional date in YYYY-MM-DD format.")
      \competitionDate -> do
        context <- askToolContext
        withCapability context "ai_contest" do
          response <- HTTP.runReq $
            req GET (http "172.20.0.4" /: "ai-contest" /: "leaderboard") NoReqBody jsonResponse
              ("date" =: fromMaybe "" competitionDate <> port 8077)
          pure . toolText . jsonText $ (responseBody response :: Aeson.Value)

fmAiContestLeaderboardImageTool :: (Chat.Chat :> es, HTTP.HTTP :> es) => Tool (Eff es)
fmAiContestLeaderboardImageTool =
  allowWhen hasExplicitContestIntent
  . withDescription "Generate and send FM's persisted 555 AI contest leaderboard as a PNG image to the current chat."
    $ tool "fm_ai_contest_leaderboard_image" (optionalText "date" "Optional date in YYYY-MM-DD format.")
      \competitionDate -> do
        context <- askToolContext
        withCapability context "ai_contest" do
          resolvedDate <- case Text.strip <$> competitionDate of
            Just date | not (Text.null date) -> pure date
            _ -> do
              response <- HTTP.runReq $
                req GET (http "172.20.0.4" /: "ai-contest" /: "leaderboard") NoReqBody jsonResponse
                  (port 8077)
              pure $ fromMaybe "" $ AesonTypes.parseMaybe
                (Aeson.withObject "FM AI leaderboard" (Aeson..: "date"))
                (responseBody response :: Aeson.Value)
          let cacheKey = fromMaybe "refresh" (messageIdText <$> context.message.messageId)
              url = "http://172.20.0.4:8077/reports/ai-leaderboard.png?date="
                <> resolvedDate <> "&v=" <> cacheKey
          sent <- Chat.replyTo context.message (ReplyBody.imageDirective url)
          case rights sent of
            _:_ -> pure (toolText "Leaderboard image sent successfully.")
            [] -> do
              let err = Text.intercalate "; " (lefts sent)
              pure (toolFailure Failure.Failure
                { category = Failure.ExternalServiceUnavailable
                , userMessage = "Leaderboard image send failed: " <> err
                , detail = err
                })

fmCompetitionScoreQueryTool :: HTTP.HTTP :> es => Tool (Eff es)
fmCompetitionScoreQueryTool =
  withDescription "Query FM's preserved personal contest-score archive by QQ ID, display name, or contest source."
    $ tool "fm_competition_score_query"
      ( optionalText "user_id" "Optional QQ or platform user ID."
      , optionalText "name" "Optional display-name fragment."
      , optionalText "source" "Optional contest source group."
      , optionalInt "limit" "Maximum records, from 1 to 100."
      )
      \userId name source requestedLimit -> do
        context <- askToolContext
        withCapability context "scores" do
          let limit = max 1 (min 100 (fromMaybe 20 requestedLimit))
          response <- HTTP.runReq $
            req GET (http "172.20.0.4" /: "competition" /: "scores") NoReqBody jsonResponse
              ( "user_id" =: fromMaybe "" userId
                <> "name" =: fromMaybe "" name
                <> "source" =: fromMaybe "" source
                <> "limit" =: limit
                <> port 8077
              )
          pure . toolText . jsonText $ (responseBody response :: [Aeson.Value])

fmCompetitionScoreSummaryTool :: HTTP.HTTP :> es => Tool (Eff es)
fmCompetitionScoreSummaryTool =
  withDescription "Summarize FM's preserved personal contest-score history by QQ ID, display name, or contest source, including best and average values."
  $ tool "fm_competition_score_summary"
      ( optionalText "user_id" "Optional QQ or platform user ID."
      , optionalText "name" "Optional display-name fragment."
      , optionalText "source" "Optional contest source group."
      )
      \userId name source -> do
        context <- askToolContext
        withCapability context "scores" do
          response <- HTTP.runReq $
            req GET (http "172.20.0.4" /: "competition" /: "summary") NoReqBody jsonResponse
              ( "user_id" =: fromMaybe "" userId
                <> "name" =: fromMaybe "" name
                <> "source" =: fromMaybe "" source
                <> port 8077
              )
          pure . toolText . jsonText $ (responseBody response :: Aeson.Value)

fmScoreAnalysisTool :: HTTP.HTTP :> es => Tool (Eff es)
fmScoreAnalysisTool =
  withDescription "Analyze the current user's typing performance from live public contest leaderboards and FM's archived typing scores. Use for natural-language questions such as today's contest results, today's typing results, all scores together, comparisons, averages, best results, accuracy below 95%, or a recent date range. The tool resolves the current sender by QQ ID, keeps contest and ordinary typing results separate, and never treats a missing leaderboard row as a zero score. Do not use this for merely sending a leaderboard image."
  $ tool "fm_score_analysis"
      ( optionalText "scope" "Optional scope: all, contest, or typing. Defaults to all."
      , optionalText "start_date" "Optional start date in YYYY-MM-DD format."
      , optionalText "end_date" "Optional end date in YYYY-MM-DD format; defaults to today in China."
      , optionalInt "days" "Optional recent calendar-day span from 1 to 3650; ignored when start_date is provided."
      , optionalText "user_id" "Optional QQ or platform user ID; defaults to the current sender."
      , optionalText "name" "Optional exact display name; used as a fallback when a public source has no QQ ID."
      , optionalText "source" "Optional source such as 虎杯, 锦标赛, 极速杯, or 555赛文. Empty means all sources."
      )
      \scope startDate endDate requestedDays userId name source -> do
        context <- askToolContext
        withCapability context "scores" do
          let choose values = case filter (not . Text.null . Text.strip) values of
                value : _ -> value
                [] -> ""
              resolvedUserId = choose [fromMaybe "" userId, fromMaybe "" context.message.senderId]
              resolvedName = choose [fromMaybe "" name, fromMaybe "" context.message.senderUsername]
              days = fromMaybe 1 requestedDays
          response <- HTTP.runReq $
            req GET (http "172.20.0.4" /: "analysis" /: "scores") NoReqBody jsonResponse
              ( "user_id" =: resolvedUserId
                <> "name" =: resolvedName
                <> "scope" =: fromMaybe "all" scope
                <> "start_date" =: fromMaybe "" startDate
                <> "end_date" =: fromMaybe "" endDate
                <> "days" =: days
                <> "source" =: fromMaybe "" source
                <> port 8077
              )
          pure . toolText . jsonText $ (responseBody response :: Aeson.Value)

fmCompetitionScoreImageTool :: (Chat.Chat :> es, HTTP.HTTP :> es) => Tool (Eff es)
fmCompetitionScoreImageTool =
  noisy
  . withDescription "Generate and send a PNG report for one person's preserved contest-score history. Resolve the target from natural language and provide a QQ ID or display name."
  $ tool "fm_competition_score_image"
      ( optionalText "user_id" "Optional QQ or platform user ID."
      , optionalText "name" "Optional display-name fragment."
      , optionalText "source" "Optional contest source group."
      )
      \userId name source -> do
        context <- askToolContext
        withCapability context "scores" do
          let url = "http://172.20.0.4:8077/reports/competition-score.png?user_id="
                <> fromMaybe "" userId <> "&name=" <> fromMaybe "" name <> "&source=" <> fromMaybe "" source
                -- cache-buster：URL 每次必须不同。2026-10-12 实测，URL 一样时
                -- 修好之后所有者仍收到旧的空图（别人的 URL 没请求过所以正常）。
                <> "&_=" <> maybe "0" (Text.pack . show) context.message.messageId
          sent <- Chat.replyTo context.message (ReplyBody.imageDirective url)
          case rights sent of
            _ : _ -> pure (toolText "成绩图片已发送。")
            [] -> do
              let err = Text.intercalate "；" (lefts sent)
              pure (toolFailure Failure.Failure
                { category = Failure.ExternalServiceUnavailable
                , userMessage = "成绩图片发送失败：" <> err
                , detail = err
                })

-- | URL 编码。2026-11-06：用户名是中文+全角括号（「云南」Fixmood（三当家）」），
-- 直接拼进 URL 会让 http-client 抛 UnicodeEncodeError，图片永远缓存不到，
-- 表现出来就是「图片 URL 抓不到 / 502」。
urlEncodeText :: Text -> Text
urlEncodeText = TextEncoding.decodeUtf8 . urlEncode True . TextEncoding.encodeUtf8

fmChartTool :: (Chat.Chat :> es, HTTP.HTTP :> es) => Tool (Eff es)
fmChartTool =
  withDescription
    "成绩图表统一入口：排行榜柱状图、个人成绩曲线、趋势、历史，全部用它。\n\
    \\n\
    \两种模式，看有没有点名赛文：\n\
    \1) 赛文成绩（排行榜/曲线）：传 contest。\n\
    \   999=群赛文/本群赛文（最常用）；777=晴分身群赛文；groupmatch=1847992065；\n\
    \   HB=虎杯；100000=锦标赛；99999=极速杯；100004=极速联赛；666=1916996679；555=555赛文。\n\
    \   说「曲线」「走势」「趋势」时 mode=trend，否则默认排行榜。\n\
    \   user 留空=当前发言人自己；说「XXX的」就传 XXX。\n\
    \   数据优先取官网，拿不到自动用成绩库，所以永远能出图。\n\
    \2) 不分赛文的个人成绩历史：不传 contest，只传 user_id / name。\n\
    \\n\
   \每次用户要图都必须真的调用本工具，不要拿对话历史里的失败当结论（工具已修好，0.7 秒出图）。\n\
    \画完把图发出去，最多补一句说明，不要复述名单。"
    $ tool "fm_chart"
        ( optionalText "contest" "赛文段号或名称。群赛文=999；虎杯=HB；锦标赛=100000；极速杯=99999；极速联赛=100004；666；777；groupmatch；555。留空=不分赛文。"
        , optionalText "user" "看谁：留空=当前发言人；说「XXX的」就传 XXX。"
        , optionalText "mode" "rank=排行榜（默认），trend=按天曲线（用户说曲线/走势/趋势时用）。"
        , optionalInt "days" "统计天数，0 或不传 = 全部时间。"
        , optionalText "group_id" "按群的赛文（999/777/groupmatch）看哪个群。**可以直接传群名**（梦幻打字阁/五笔修炼基地/虎码训练营/剑气冲霄堂…），也可以传群号；留空=当前群。"
        )
        \contest user mode requestedDays maybeGroupId -> do
          context <- askToolContext
          withCapability context "scores" do
            let days = fromMaybe 0 requestedDays
                chatIdText =
                  case Text.strip (fromMaybe "" maybeGroupId) of
                    "" -> maybe "" (Text.pack . show) context.message.chatId
                    given -> given
                cacheBust = "&_=" <> maybe "0" (Text.pack . show) context.message.messageId
                inputLower = Text.toLower (context.input.text)
                groupMentioned =
                  any (`Text.isInfixOf` inputLower)
                    ["群赛文", "本群赛文", "这个群的赛文", "群里的赛文", "本群的赛文"]
                contestText =
                  if groupMentioned then "999" else Text.strip (fromMaybe "" contest)
                -- 模式也读用户原话判断：2026-11-06 实测模型只传 contest 不传 mode，
                -- 于是「成绩曲线」画成了排行榜。
                trendMentioned =
                  any (`Text.isInfixOf` inputLower)
                    ["曲线", "走势", "趋势", "折线", "trend"]
                modeText =
                  if trendMentioned then "trend"
                  else if Text.strip (fromMaybe "rank" mode) == "trend" then "trend"
                  else "rank"
                userText =
                  case Text.strip (fromMaybe "" user) of
                    "" -> Text.strip (fromMaybe "" context.message.senderUsername)
                    other -> other
                url
                  | not (Text.null contestText) =
                      "http://172.20.0.4:8077/reports/score-chart.png?contest=" <> urlEncodeText contestText
                        <> "&group_id=" <> urlEncodeText chatIdText
                        <> "&days=" <> Text.pack (show days)
                        <> "&mode=" <> modeText
                        <> "&user=" <> urlEncodeText userText
                        <> cacheBust
                  | otherwise =
                      -- 没点名赛文：老的个人成绩历史图（555/AI赛文那套）
                      "http://172.20.0.4:8077/reports/score-chart.png?name="
                        <> userText
                        <> "&days=" <> Text.pack (show (max 1 (min 3650 days)))
                        <> cacheBust
            sent <- Chat.replyTo context.message (ReplyBody.imageDirective url)
            case rights sent of
              _ : _ -> pure . toolText $
                if Text.null contestText
                  then "个人成绩图表已发送。"
                  else "「" <> contestText <> "」的"
                    <> (if modeText == "trend" then "成绩曲线" else "排行榜")
                    <> "已发送（看的是 " <> userText <> "）。"
                    <> "回答时请按这个赛文说，不要提 555 或别的赛文。"
              [] -> do
                let err = Text.intercalate "；" (lefts sent)
                pure (toolFailure Failure.Failure
                  { category = Failure.ExternalServiceUnavailable
                  , userMessage = "成绩图表发送失败：" <> err
                  , detail = err
                  })

fmBotGuardAccountsTool :: HTTP.HTTP :> es => Tool (Eff es)
fmBotGuardAccountsTool =
  allowWhen superuserOnly
  . withDescription "List, add, or remove QQ bot accounts used by FM's loop protection. This changes persistent safety state and is restricted to the owner."
  $ tool "fm_bot_guard_accounts"
      ( requiredText "action" "One of: list, add, remove."
      , optionalText "account_id" "QQ account ID; required for add and remove."
      , optionalText "label" "Optional readable label when adding an account."
      )
      \action accountId label -> do
        response <- HTTP.runReq $
          req POST (http "172.20.0.4" /: "bot-guard" /: "accounts")
            (ReqBodyJson (Aeson.object
              [ "action" Aeson..= action
              , "account_id" Aeson..= fromMaybe "" accountId
              , "label" Aeson..= fromMaybe "" label
              ]))
            jsonResponse (port 8077)
        pure . toolText . jsonText $ (responseBody response :: Aeson.Value)

sendAiContestText
  :: Chat.Chat :> es
  => Context
  -> Aeson.Value
  -> Eff es ToolResult
sendAiContestText context response =
  case AesonTypes.parseMaybe parse response of
    Nothing -> pure (toolText "AI 赛文保存失败，领域服务没有返回有效正文。")
    Just (competitionDate, title, body, difficulty) -> do
      let message =
            "[FM/AI赛文·" <> difficulty <> "] 《" <> title <> "》 [字数" <> show (Text.length body) <> "]\n"
            <> body <> "\n-----第555段 " <> competitionDate <> "-FM赛文"
      sent <- Chat.replyTo context.message message
      case rights sent of
        _ : _ -> pure (toolText "AI 赛文已发送。不要在最终回复中重复正文。")
        [] -> do
          let err = Text.intercalate "；" (lefts sent)
          pure (toolFailure Failure.Failure
            { category = Failure.ExternalServiceUnavailable
            , userMessage = "AI 赛文已保存，但发送失败：" <> err
            , detail = err
            })
  where
    parse = Aeson.withObject "FM AI contest publish" \outer -> do
      textValue <- outer Aeson..: "text"
      Aeson.withObject "FM AI contest text" (\inner ->
        (,,,)
          <$> inner Aeson..: "date"
          <*> inner Aeson..: "title"
          <*> inner Aeson..: "body"
          <*> inner Aeson..:? "difficulty" Aeson..!= "普") textValue

fmDomainStatsTool :: HTTP.HTTP :> es => Tool (Eff es)
fmDomainStatsTool =
  withDescription "Read record counts for FM's migrated library, contest, score, recall, and group data."
    $ tool "fm_domain_stats" noArguments do
      response <- HTTP.runReq $
        req GET (http "172.20.0.4" /: "stats") NoReqBody jsonResponse (port 8077)
      pure . toolText . jsonText $ (responseBody response :: Aeson.Value)

fmAdminStatusTool :: HTTP.HTTP :> es => Tool (Eff es)
fmAdminStatusTool =
  allowWhen superuserOnly
  . withDescription "Return the authoritative current FM Control Center address. Use this whenever the owner asks for FM's backend, admin panel, control center, or dashboard address. Never infer or reuse historical score-analyzer addresses or ports. The public address is fixed at http://117.72.34.77:8090. This tool is owner-only and does not expose RPC or admin tokens."
    $ tool "fm_admin_status" noArguments do
      pure . toolText $
        "当前 FM Control Center 后台地址：http://117.72.34.77:8090\n"
          <> "状态：已部署。\n"
          <> "说明：这是 FM 当前使用的管理后台；不要使用历史 score-analyzer 地址或 8787、8788、8790 端口。"

-- | FM 查看「自己」。
--
-- 为什么需要这个工具：FM 手里有 30 个 fm_* 工具，但全都是关于外部世界的
-- （查撤回、查成绩、查文库、查群状态）。**没有一个是关于它自己的。**
--
-- 后果是它无法回答「我上一条说了什么」「我哪句话被纠正了」「我最近哪些运行
-- 慢或失败」这类问题。而这些都是改进的前提——没有观察就没有迭代，
-- 它此前只能盲改（实测：8 分钟内把自己的人格改了 28 次，越改越抽象）。
--
-- 数据来自 fm-domain 的 /bot/messages 与 /bot/runs，那是为中控后台加的只读
-- 接口，直接读机器人的 cosmobot.sqlite3。机器人本来就能访问到（同一个
-- fm-runtime 网络、无鉴权），只是此前不知道有这两个入口。
--
-- Owner only：里面有它的运行细节（工具用量、token、失败原因），
-- 不该让普通群成员拿它当探测窗口。
fmSelfReviewTool :: HTTP.HTTP :> es => Tool (Eff es)
fmSelfReviewTool =
  allowWhen superuserOnly
  . withDescription "Look at FM's own recent behaviour. action=messages returns what FM recently said, together with the message it was replying to and who sent it; action=runs returns FM's recent agent runs with elapsed time, turns, tool-call count, tokens and status. Use this when the owner asks how FM has been doing, why it replied strangely, what it said, or which of its own runs were slow or failed -- and use it to check FM's own behaviour before changing how it works. This is FM's own record, not the group's chat log; to read other people's messages use chat_log instead."
  $ tool "fm_self_review"
      ( requiredText "action" "messages (what FM said) or runs (FM's own runs with timing and status)."
      , optionalInt "limit" "Maximum rows, from 1 to 200. Defaults to 40 for messages and 20 for runs."
      , optionalText "chat_id" "For action=messages, restrict to one chat (group id or QQ number)."
      )
      \action limitText chatFilter -> do
        let wanted = Text.toCaseFold (Text.strip action)
        if wanted /= "messages" && wanted /= "runs"
          then pure . toolText $ "action 只能是 messages 或 runs，收到的是「" <> action <> "」。"
          else do
            let defaultLimit = if wanted == "messages" then 40 else 20
                lim = max 1 (min 200 (fromMaybe defaultLimit limitText))
                chatArg = Text.strip (fromMaybe "" chatFilter)
                -- target 用 Url 直接构造：/: 产生的是 Url 而不是 Text，
                -- 所以不能先算出路径名再拼（会类型不匹配）。
                request
                  | wanted == "messages" =
                      req GET (http "172.20.0.4" /: "bot" /: "messages") NoReqBody jsonResponse
                        ( if Text.null chatArg
                            then "limit" =: lim <> port 8077
                            else "limit" =: lim <> "chat_id" =: chatArg <> port 8077
                        )
                  | otherwise =
                      req GET (http "172.20.0.4" /: "bot" /: "runs") NoReqBody jsonResponse
                        ("limit" =: lim <> port 8077)
            response <- HTTP.runReq request
            pure . toolText . jsonText $ (responseBody response :: Aeson.Value)

-- | 读自己的自省笔记。
--
-- 存在的理由：每日自省任务会把 FM 前一天的行为写成一篇笔记
-- （/data/memory/self/notes/YYYY-MM-DD.md），但**笔记躺在磁盘上没人读**——
-- 观察和调整之间是断的。这个工具补上那一段：让它能翻自己的旧账。
--
-- 用「按需读」而不是「每轮都塞进上下文」，是因为笔记有近千字，
-- 常驻会占掉每一轮的 token；而它真正需要它们的时刻是
-- 「准备改自己 / 复盘自己 / 被问到自己表现」这几种。
fmSelfNotesTool :: IOE :> es => Tool (Eff es)
fmSelfNotesTool =
  allowWhen superuserOnly
  . withDescription "Read FM's own daily self-reflection notes. These are written each night by a review pass that reads FM's own messages and runs, and they record what FM did well and what went wrong. Use this before changing any of FM's own behaviour (memory, persona, procedures), and when the owner asks how FM has been doing or whether something was fixed. Returns the most recent notes, newest first."
  $ tool "fm_self_notes"
      ( optionalInt "days" "How many recent daily notes to return, from 1 to 14. Defaults to 3."
      )
      \daysArg -> do
        let want = max 1 (min 14 (fromMaybe 3 daysArg))
        let dir = "/data/memory/self/notes"
        exists <- liftIO (Directory.doesDirectoryExist dir)
        if not exists
          then pure (toolText "还没有自省笔记。")
          else do
            files <- liftIO (Directory.listDirectory dir)
            let notes = reverse (List.sort [f | f <- files, ".md" `List.isSuffixOf` f])
                chosen = take want notes
            if null chosen
              then pure (toolText "还没有自省笔记。")
              else do
                bodies <- liftIO $ mapM (\f -> (f,) <$> readFileUtf8 (dir <> "/" <> f)) chosen
                pure . toolText $
                  Text.intercalate "\n\n" [ "===== " <> Text.pack f <> " =====\n" <> body
                                          | (f, body) <- bodies ]

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = Text.pack <$> readFile path

-- | FM 自己的工作记忆。
--
-- 为什么需要：它每次对话都是「刚醒来」。除了被回复的消息串（threads 表），
-- 它不知道自己最近在琢磨什么、哪件事没做完。
--
-- 「工作记忆」和现有几种记忆不是一回事：
--   · chat_memory / sender_memory  —— 关于**别人**的事实
--   · 人格文件                      —— 它**是什么样**
--   · growth.md / recurring.md     —— 它**过去经历 / 犯过什么**
--   · 这个                          —— 它**现在手上有什么**
--
-- 刻意不放进 scratchpad_todos：那张表是用户面向的待办命令（!todo / !done），
-- 按发送者分，是给群友记事用的，不是它自己的工作区。
--
-- 存放位置与 recurring.md 同级（memory/self/working.md），由人设引用，
-- 所以每轮都注入上下文——不靠它自己想起来读。
-- 写操作限定 superuser：这是它的自我认知，不该被别人改。
fmSelfNoteTool :: IOE :> es => Tool (Eff es)
fmSelfNoteTool =
  withDescription "FM's own working note: what it is currently looking into, what is still unfinished, what to pick up next time. action=read shows it; action=set replaces it (owner only). Read it when a request seems to continue earlier work, or when unsure what you were doing. Set it after finishing or starting something that should carry across conversations. Keep it short -- a few lines. Do not put facts about other people here (use chat_memory or sender_memory), and do not put personality or style here (those live in the persona files)."
    $ tool "fm_self_note"
      ( requiredText "action" "read, or set (owner only)."
      , optionalText "content" "For action=set: the new note, a few lines at most. Empty clears it."
      )
      \action contentArg -> do
        context <- askToolContext
        let wanted = Text.toCaseFold (Text.strip action)
            path = "/data/memory/self/working.md"
        case wanted of
          "read" -> do
            exists <- liftIO (Directory.doesFileExist path)
            if not exists
              then pure (toolText "还没有工作笔记。")
              else do
                body <- liftIO (readFileUtf8 path)
                pure (toolText (if Text.null (Text.strip body) then "工作笔记是空的。" else body))
          "set" ->
            if not context.superuser
              then pure (toolText "只有所有者能改工作笔记。")
              else do
                let capped = Text.take 1200 (Text.strip (fromMaybe "" contentArg))
                liftIO (Directory.createDirectoryIfMissing True "/data/memory/self")
                liftIO (writeFile path (Text.unpack capped))
                pure . toolText $
                  if Text.null capped
                    then "工作笔记已清空。"
                    else "工作笔记已更新（" <> Text.pack (show (Text.length capped)) <> " 字符）。"
          _ -> pure (toolText "action 只能是 read 或 set。")

hasExplicitLibraryIntent :: Context -> Bool
hasExplicitLibraryIntent context =
  not (hasExplicitContestIntent context)
    && (any (`Text.isInfixOf` normalized) libraryTerms || isDifficultyCommand normalized)
  where
    normalized = Text.toLower . Text.filter (not . (`elem` [' ', '\t', '\r', '\n'])) $ context.input.text
    libraryTerms =
      [ "发文", "文来", "来一篇", "来篇", "来点文", "发一篇", "发篇"
      , "文章", "练习文", "文库", "开一篇", "开篇"
      , "续文", "续段", "上一篇", "这篇文", "停止发文", "单字练习"
      -- Difficulty words on their own count as an article request; the model
      -- passes the character in `difficulty`. Without this, "来篇水文" fell
      -- through the gate entirely and the tools were hidden from the model.
      , "淼文", "水文", "易文", "普文", "难文", "虐文"
      , "淼档", "水档", "易档", "普档", "难档", "虐档"
      -- 2026-10-30：所有者问「能不能指定发哪篇文章」时，这句话里没有上面任何一个词，
      -- 于是库文工具被闸门挡住，FM 只能回答「不能按篇名指定」—— 而能力其实一直是有的
      -- （fm_library_start 的 query 参数就是标题/主题关键词）。把「指定」相关的说法收进来。
      , "按篇名", "按标题", "指定篇", "指定文章", "指定那篇", "篇名", "标题是"
      , "那篇文", "那篇文章", "这篇文", "发那篇", "发这篇"
      ]
    isDifficultyCommand input =
      any (`Text.isInfixOf` input)
        [ "fm淼", "fm水", "fm易", "fm普", "fm难", "fm虐"
        , "fm，淼", "fm，水", "fm，易", "fm，普", "fm，难", "fm，虐"
        ]

hasExplicitContestIntent :: Context -> Bool
hasExplicitContestIntent context =
  any (`Text.isInfixOf` normalized) contestTerms
  where
    normalized = Text.toLower context.input.text
    contestTerms =
      [ "赛文", "比赛", "赛事", "555", "虎杯", "极速杯", "锦标赛"
      , "群赛", "赛榜", "排行榜", "ai赛文"
      ]

hasExplicitLiveContestIntent :: Context -> Bool
hasExplicitLiveContestIntent context =
  hasExplicitContestIntent context && any (`Text.isInfixOf` normalized) liveTerms
  where
    normalized = Text.toLower context.input.text
    liveTerms = ["今天", "当天", "当前", "实时", "在线", "官网", "公开", "最新"]

withCapability :: HTTP.HTTP :> es => Context -> Text -> Eff es ToolResult -> Eff es ToolResult
withCapability context capability action =
  case context.message of
    IncomingMessage{platform = PlatformQQ, chatId = Just groupId} -> do
      response <- HTTP.runReq $
        req GET (http "172.20.0.4" /: "group-capability") NoReqBody jsonResponse
          ("group_id" =: (show groupId :: Text) <> "capability" =: capability <> port 8077)
      let enabled = fromMaybe True $ AesonTypes.parseMaybe
            (Aeson.withObject "FM group capability" (Aeson..: "enabled"))
            (responseBody response :: Aeson.Value)
      if enabled
        then action
        else pure (toolText ("This capability is disabled in the current group: " <> capability))
    _ -> action
