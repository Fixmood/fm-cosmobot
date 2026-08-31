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
import Bot.Agent.Tools.Repository
import Bot.Agent.Tools.Terminal
import Bot.Agent.Tools.Time
import Bot.Agent.Tools.Trigger
import Bot.Agent.Tools.Typst
import Bot.Agent.Tools.Web
import Bot.Agent.Tools.Workspace
import Bot.Agent.Tool
import Bot.Agent.Types (Context)
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
  case requestDomain compact of
    Nothing -> tools
    Just domain -> filter (keepTool domain . toolName) tools
  where
    keepTool domain name =
      name `elem` alwaysVisible
        || name `elem` domainTools domain

    alwaysVisible =
      [ toolEnableName
      , "datetime"
      , "current_message_info"
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
        ]

    normalized = Text.toCaseFold context.input.text
    compact = Text.filter (not . (`elem` [' ', '\t', '\n', '\r', '\x3000'])) normalized

    requestDomain value
      | explicitAdmin value = Just Admin
      | explicitScores value = Just Scores
      | explicitContest value = Just Contest
      | explicitLibrary value = Just Library
      | otherwise = Nothing

    explicitLibrary value =
      any (`Text.isInfixOf` value)
        [ "文来", "发文", "来一篇", "来篇", "练一篇", "练文"
        , "继续打", "上一篇", "这篇文"
        ]

    explicitContest value =
      any (`Text.isInfixOf` value)
        [ "赛文", "比赛文章", "赛事文本", "虎杯", "极速杯", "锦标赛"
        , "555赛文", "ai赛文", "排行榜"
        ]

    explicitScores value =
      any (`Text.isInfixOf` value)
        [ "查成绩", "成绩如何", "成绩怎么样", "分析成绩", "成绩分析"
        , "成绩曲线", "成绩图", "平均成绩", "最好成绩", "成绩排行"
        ]

    explicitAdmin value =
      any (`Text.isInfixOf` value)
        [ "后台地址", "后台在哪", "控制中心", "管理后台", "fm后台" ]

data RequestDomain = Library | Contest | Scores | Admin

acpTools :: ACP.ACP :> es => [Tool (Eff es)]
acpTools =
  [ acpReadClientFileTool
  , acpWriteClientFileTool
  , terminalTool
  ]
