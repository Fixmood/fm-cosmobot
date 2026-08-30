{-|
Module      : Bot.Agent.Tools
Description : Built-in agent tools
Stability   : experimental
-}

module Bot.Agent.Tools
  ( defaultTools
  , defaultToolsWith
  , acpTools
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
      , fmCompetitionScoreImageTool
      , fmChartTool
      , fmBotGuardAccountsTool
      , fmDomainStatsTool
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

acpTools :: ACP.ACP :> es => [Tool (Eff es)]
acpTools =
  [ acpReadClientFileTool
  , acpWriteClientFileTool
  , terminalTool
  ]
