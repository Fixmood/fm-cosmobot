{-|
Module      : Bot.Agent.Tools
Description : Built-in agent tools
Stability   : experimental
-}

module Bot.Agent.Tools
  ( defaultTools
  , defaultToolsWith
  , acpTools
  , selectToolsForMessage
  , toolSelectionSummary
  )
where

import Bot.Agent.Tools.Chat
import Bot.Agent.Tools.Audio
import Bot.Agent.Tools.Emacs
import Bot.Agent.Tools.Bridge
import Bot.Agent.Tools.Files
import Bot.Agent.Tools.FMDomain
import Bot.Agent.Tools.Image
import Bot.Agent.Tools.Media
import Bot.Agent.Tools.Memory
import Bot.Agent.Tools.Matrix
import Bot.Agent.Tools.Schedule
import Bot.Agent.Tools.Sandbox
import Bot.Agent.Tools.Shell
import Bot.Agent.Tools.Skills
import Bot.Agent.Tools.SubAgent
import Bot.Agent.Tools.Continuation
import Bot.Agent.Tools.Meta
import Bot.Agent.Tools.Model
import Bot.Agent.Tools.Python
import Bot.Agent.Tools.Relay
import Bot.Agent.Tools.Repository
import Bot.Agent.Tools.Terminal
import Bot.Agent.Tools.Time
import Bot.Agent.Tools.Trigger
import Bot.Agent.Tools.Typst
import Bot.Agent.Tools.Web
import Bot.Agent.Tools.Workspace
import Bot.Agent.Tool
import Bot.Agent.Types (Context (..))
import Bot.Core.Message (MessageInput (..))
import qualified Bot.Effect.ACP as ACP
import qualified Bot.Effect.Agent as Agent
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.Lifecycle as Lifecycle
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Effect.Matrix as Matrix
import qualified Bot.Effect.Resource as Resource
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Effect.Skills as Skills
import qualified Bot.Effect.Typst as Typst
import Bot.Prelude
import qualified Data.Text as Text
import Effectful.Timeout
import Effectful.Process
import Effectful.FileSystem

-- | Built-in tools exposed to the model after per-message permission checks.
defaultTools
  :: Agent.Agent :> es
  => AgentAudit.AgentAudit :> es
  => Chat.Chat :> es
  => ChatLog.ChatLog :> es
  => HTTP.HTTP :> es
  => Lifecycle.Lifecycle :> es
  => LLM.LLM :> es
  => Media.Media :> es
  => Memory.Memory :> es
  => Matrix.Matrix :> es
  => Resource.Resource :> es
  => Scheduler.Scheduler :> es
  => Skills.Skills :> es
  => Typst.Typst :> es
  => Fail :> es
  => Concurrency.Concurrency :> es
  => Prim :> es
  => Concurrent :> es
  => Timeout :> es
  => KatipE :> es
  => Process :> es
  => FileSystem :> es
  => IOE :> es
  => [Tool (Eff es)]
defaultTools = tools
  where
    tools = defaultToolsWith []

defaultToolsWith
  :: Agent.Agent :> es
  => AgentAudit.AgentAudit :> es
  => Chat.Chat :> es
  => ChatLog.ChatLog :> es
  => HTTP.HTTP :> es
  => Lifecycle.Lifecycle :> es
  => LLM.LLM :> es
  => Media.Media :> es
  => Memory.Memory :> es
  => Matrix.Matrix :> es
  => Resource.Resource :> es
  => Scheduler.Scheduler :> es
  => Skills.Skills :> es
  => Typst.Typst :> es
  => Fail :> es
  => Concurrency.Concurrency :> es
  => Prim :> es
  => Concurrent :> es
  => Timeout :> es
  => KatipE :> es
  => Process :> es
  => FileSystem :> es
  => IOE :> es
  => [Tool (Eff es)]
  -> [Tool (Eff es)]
defaultToolsWith extraTools = tools
  where
    tools =
      [ toolEnableTool
      , triggerManageTool
      , fmRepositoryPRTool
      , accountBalanceTool
      , chatModelManageTool
      , imageModelManageTool
      , chatModelAddTool
      , chatModelEditTool
      , chatModelDeleteTool
      , chatModelSwitchTool
      , chatModelResetTool
      , queryChatLogTool
      , queryCurrentSenderChatLogTool
      , recallRecentSelfMessagesTool
      , webSearchTool
      , webFetchTool
      , datetimeTool
      , chatModelStatusTool
      , fmGroupStatusTool
      , fmLibrarySearchTool
      , fmLibraryPickTool
      , fmLibraryStartTool
      , fmLibraryContinueTool
      , fmLibraryContinueSameTool
      , fmLibraryContinuePreviousTool
      , fmLibraryRecallRecentTool
      , fmLibraryStopTool
      , fmLibraryStatsTool
      , fmRecallQueryTool
      , fmScoreQueryTool
      , fmGroupSetOnlineTool
      , fmGroupSetCapabilityTool
      , fmBridgeStatusTool
      , fmBridgeManageTool
      , fmBridgeTestTool
      , fmRelayToOwnerTool
      , fmRelayMessageTool
      , fmTellMemberTool
      , fmTakeoverManageTool
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
      , readMediaTextTool
      , mediaToFileTool
      , viewImageTool
      , generateImageTool
      , editImageTool
      , generateAudioTool
      , typstRenderTool
      , sendReplyTool
      , sendFileTool
      , sendMediaTool
      , mentionUserTool
      , senderMemberInfoTool
      , memberInfoTool
      , userAvatarTool
      , listGroupMembersTool
      , currentMessageInfoTool
      , matrixRequestTool
      , scheduleTool
      , senderMemoryTool
      , chatMemoryTool
      , privatePersonaTool
      , groupPersonaTool
      , memberStyleTool
      , loadSkillTool
      , sandboxTool
      , commandTool
      , runBashTool
      , runPythonTool
      , workspaceTool
      , captureContinuationTool
      , resumeContinuationTool
      , subagentTool tools
      , emacsEvalTool
      ] <> extraTools

-- | Keep the full tool set for ordinary or ambiguous messages.  For an
-- explicit FM domain request, hide unrelated tools from the model while
-- retaining every tool that can participate in that domain.  This changes
-- only model-visible schemas; dispatch and the registered tool definitions
-- remain unchanged.
selectToolsForMessage :: Context -> [Tool m] -> [Tool m]
selectToolsForMessage context tools =
  case selection compact of
    SimpleChat -> []
    FullTools -> tools
    ToolSubset domain -> filter (keepTool domain . toolName) tools
  where
    keepTool domain name =
      name `elem` alwaysVisible domain
        || name `elem` domainTools domain

    -- Delivering a message into another chat is not domain-specific: the owner
    -- may ask for it in any conversation, so it must survive every subset.
    alwaysVisible Image =
      [ "datetime"
      , "current_message_info"
      , "fm_tell_member"
      ]
    alwaysVisible _ =
      [ toolEnableName
      , "datetime"
      , "current_message_info"
      , "fm_tell_member"
      ]

    domainTools = \case
      Library ->
        [ "fm_library_search", "fm_library_pick", "fm_library_start"
        , "fm_library_continue", "fm_library_continue_same"
        , "fm_library_continue_previous", "fm_library_recall_recent"
        , "fm_library_stop", "fm_library_stats"
        ]
      Contest ->
        [ "fm_contest_search", "fm_contest_send"
        , "fm_live_competition_rank", "fm_live_competition_text"
        , "fm_ai_contest_text", "fm_ai_contest_publish"
        , "fm_ai_contest_leaderboard", "fm_ai_contest_leaderboard_image"
        , "fm_competition_score_query", "fm_competition_score_summary"
        , "fm_score_analysis", "fm_competition_score_image", "fm_chart"
        ]
      Scores ->
        [ "fm_score_query", "fm_competition_score_query"
        , "fm_competition_score_summary", "fm_score_analysis"
        , "fm_competition_score_image", "fm_chart"
        ]
      Admin ->
        [ "fm_admin_status", "fm_domain_stats", "fm_group_status"
        , "chat_model_status", "account_balance"
        ]
      Image ->
        [ "user_avatar", "image_generate", "image_edit", "send_reply"
        , "send_media", "read_media_text", "media_to_file", "view_image"
        , "search_web"
        ]

    normalized = Text.toCaseFold context.input.text
    compact = Text.filter (not . (`elem` [' ', '\t', '\n', '\r', '\x3000'])) normalized

    selection value
      | explicitAdmin value = ToolSubset Admin
      | explicitImage value = ToolSubset Image
      | explicitScores value = ToolSubset Scores
      | explicitContest value = ToolSubset Contest
      | explicitLibrary value = ToolSubset Library
      | simpleChat value = SimpleChat
      | otherwise = FullTools

    simpleChat value =
      Text.length value <= 48
        && any (`Text.isInfixOf` value)
          [ "你好", "您好", "嗨", "嘿", "谢谢", "感谢", "辛苦了", "早安", "晚安"
          , "哈哈", "笑死", "在吗", "好的", "好啊", "嗯嗯", "收到", "明白了"
          ]

    explicitAdmin value =
      any (`Text.isInfixOf` value)
        [ "后台地址", "后台在哪", "控制中心", "管理后台", "fm后台"
        , "什么模型", "哪个模型", "当前模型", "模型状态", "模型余额"
        , "多少余额", "剩余余额", "还剩多少", "账户余额", "账号余额"
        ]

    explicitImage value =
      any (`Text.isInfixOf` value)
        [ "参考头像", "用头像", "根据头像", "头像生成", "头像做"
        , "九宫格表情包", "表情包", "生图", "生成图片", "生成一张图"
        , "画一张", "画个图", "做张图", "做一张图", "修改图片"
        , "编辑图片", "图片编辑", "改图", "参考这张图"
        , "搜图", "搜索图片", "查找图片", "找张图", "找一张图"
        , "找个图", "找一个图", "搜张图", "搜一张图"
        ]

    explicitLibrary value =
      any (`Text.isInfixOf` value)
        [ "文来", "发文", "发文章", "来一篇", "来篇", "练一篇", "练文"
        , "开始跟打", "继续打", "上一篇", "下一篇", "这篇文", "继续"
        ]

    explicitContest value =
      any (`Text.isInfixOf` value)
        [ "赛文", "比赛", "比赛文章", "赛事文本", "虎杯", "极速杯", "锦标赛"
        , "555赛文", "ai赛文", "排行榜", "榜单"
        ]

    explicitScores value =
      "成绩" `Text.isInfixOf` value
        && any (`Text.isInfixOf` value) [ "查", "看", "我的", "怎么样", "如何", "分析", "曲线", "排行" ]

data ToolSelection
  = SimpleChat
  | ToolSubset RequestDomain
  | FullTools

toolSelectionSummary :: Context -> [Tool m] -> (Text, Text)
toolSelectionSummary context _tools =
  case selectionFor compact of
    SimpleChat -> ("none", "simple")
    ToolSubset domain -> ("subset", domainName domain)
    FullTools -> ("full", "fallback")
  where
    compact = Text.filter (not . (`elem` [' ', '\t', '\n', '\r', '\x3000']))
      (Text.toCaseFold context.input.text)
    selectionFor value
      | any (`Text.isInfixOf` value)
          [ "后台地址", "后台在哪", "控制中心", "管理后台", "fm后台"
          , "群设置", "暂停群", "群能力", "切换模型", "模型管理"
          , "什么模型", "哪个模型", "当前模型", "模型状态", "模型余额"
          , "多少余额", "剩余余额", "还剩多少", "账户余额", "账号余额"
          ] = ToolSubset Admin
      | any (`Text.isInfixOf` value)
          [ "参考头像", "用头像", "根据头像", "头像生成", "头像做"
          , "九宫格表情包", "表情包", "生图", "生成图片", "生成一张图"
          , "画一张", "画个图", "做张图", "做一张图", "修改图片"
          , "编辑图片", "图片编辑", "改图", "参考这张图"
          , "搜图", "搜索图片", "查找图片", "找张图", "找一张图"
          , "找个图", "找一个图", "搜张图", "搜一张图"
          ] = ToolSubset Image
      | "成绩" `Text.isInfixOf` value && any (`Text.isInfixOf` value) ["查", "看", "我的", "怎么样", "如何", "分析", "曲线", "排行"] = ToolSubset Scores
      | any (`Text.isInfixOf` value) ["赛文", "比赛", "比赛文章", "赛事文本", "虎杯", "极速杯", "锦标赛", "555赛文", "ai赛文", "排行榜", "榜单"] = ToolSubset Contest
      | any (`Text.isInfixOf` value) ["文来", "发文", "发文章", "来一篇", "来篇", "练一篇", "练文", "开始跟打", "继续打", "上一篇", "下一篇", "这篇文", "继续"] = ToolSubset Library
      | Text.length value <= 48 && any (`Text.isInfixOf` value)
          ["你好", "您好", "嗨", "嘿", "谢谢", "感谢", "辛苦了", "早安", "晚安", "哈哈", "笑死", "在吗", "好的", "好啊", "嗯嗯", "收到", "明白了"] = SimpleChat
      | otherwise = FullTools
    domainName = \case
      Library -> "library"
      Contest -> "contest"
      Scores -> "scores"
      Admin -> "admin"
      Image -> "image"

data RequestDomain = Library | Contest | Scores | Admin | Image

acpTools :: ACP.ACP :> es => [Tool (Eff es)]
acpTools =
  [ acpReadClientFileTool
  , acpWriteClientFileTool
  , terminalTool
  ]
