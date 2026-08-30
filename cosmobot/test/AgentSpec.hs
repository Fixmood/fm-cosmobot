{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import qualified Bot.Agent as Agent
import qualified Bot.Agent.Tool as AgentTool
import qualified Bot.Agent.Tools as AgentTools
import qualified Bot.Agent.Core as AgentCore
import qualified Bot.Agent.Middleware.Python as PythonMiddleware
import qualified Bot.Agent.Middleware.Observation as AgentObservation
import qualified Bot.Agent.Middleware.Observation.Types as ObservationTypes
import qualified Bot.Agent.Middleware.ToolResultCompaction as ToolResultCompaction
import qualified Bot.Agent.Middleware.Tools as ToolMiddleware
import qualified Bot.Agent.Program.Python as PythonProgram
import qualified Bot.Agent.Tools.Audio as AudioTools
import qualified Bot.Agent.Tools.Chat as ChatTools
import qualified Bot.Agent.Tools.Continuation as ContinuationTools
import qualified Bot.Agent.Tools.Files as FileTools
import qualified Bot.Agent.Tools.FMDomain as FMDomainTools
import qualified Bot.Agent.Tools.Image as ImageTools
import qualified Bot.Agent.Tools.Media as MediaTools
import qualified Bot.Agent.Tools.Matrix as MatrixTools
import qualified Bot.Agent.Tools.Meta as MetaTools
import qualified Bot.Agent.Tools.Model as ModelTools
import qualified Bot.Agent.Tools.Python as PythonTools
import qualified Bot.Agent.Tools.Sandbox as SandboxTools
import qualified Bot.Agent.Tools.SubAgent as SubAgentTools
import qualified Bot.Agent.Tools.Terminal as TerminalTools
import qualified Bot.Agent.Tools.Workspace as WorkspaceTools
import qualified Bot.Agent.Types as AgentTypes
import qualified Bot.Agent.ToolRegistry as ToolRegistry
import Bot.Agent.Tools.Shell (runBashSafe, runBashTool)
import qualified Bot.AgentAudit.Storage as AgentAuditStorage
import Bot.Agent.Tools.Common (UseLimit (..), chatTag, newUseLimiter, workTag)
import Bot.Chat.Driver.Types (ChatDriverEffects)
import qualified Bot.Chat.Driver.Types as Driver
import qualified Bot.Concurrency.Manager as ConcurrencyManager
import qualified Bot.Chat.Bridge.FM as FMBridge
import Bot.Core.Thread
import Bot.Core.Transcript
import qualified Bot.Core.ReplyBody as ReplyBody
import Bot.Core.Route (runHandlers)
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Agent as AgentEffect
import qualified Bot.Effect.ACP as ACP
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Lifecycle as LifecycleEffect
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Matrix as Matrix
import qualified Bot.Media.Config as MediaConfig
import qualified Bot.Media.Interpreter as MediaInterpreter
import qualified Bot.LLM.OpenAI.Config as LLMConfig
import qualified Bot.LLM.OpenAI.Transport as LLMTransport
import qualified Bot.LLM.Test as LLMTest
import qualified Bot.Lifecycle as Lifecycle
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Effect.Resource as ResourceEffect
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Effect.Skills as Skills
import qualified Bot.Effect.Storage as StorageEffect
import qualified Bot.Effect.Typst as Typst
import qualified Bot.Memory as MemoryStore
import qualified Bot.Resource as ResourceManager
import qualified Bot.Resource.SubAgent as SubAgentResource
import qualified Bot.Skills as SkillsStore
import Bot.Core.Message
import qualified Bot.Core.Message as Message
import Bot.Handler.Ask (askHandlers)
import Bot.Handler.Ask.Config (AskHandlerConfig (..))
import qualified Bot.Handler.Ask.AgentRun as AgentRun
import Bot.Handler.Audit (auditHandlers)
import qualified Bot.HTTP as HTTP
import qualified Bot.Log as Log
import Bot.Storage.Thread
import qualified Bot.Storage.SQLite as StorageSQLite
import qualified Bot.System.Typst.Test as TypstTest
import qualified Bot.System.Typst.Types as TypstTypes
import qualified Bot.Util.HList as HList
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import qualified Data.IORef as IORef
import qualified Data.Sequence as Seq
import qualified Streaming.ByteString as Q
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, getCurrentTime)
import Data.Unique
import qualified Database.Selda as Selda
import qualified Database.Selda.Backend as SeldaBackend
import qualified Effectful.Concurrent.MVar as MVar
import Effectful.FileSystem (FileSystem, runFileSystem)
import qualified Effectful.FileSystem as FS
import qualified Effectful.FileSystem.IO.ByteString as FSByteString
import Effectful.Process (Process, runProcess)
import qualified Effectful.Concurrent.Async as Async
import qualified Effectful.Process.Typed as TypedProcess
import Effectful.Timeout (Timeout, runTimeout, timeout)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.Internal as HTTPInternal
import qualified Network.HTTP.Req as Req
import qualified Network.HTTP.Types.Status as HTTPStatus
import qualified Network.HTTP.Types.Version as HTTPVersion
import qualified Streaming.Prelude as S
import System.Directory
import System.FilePath
import System.IO.Error (catchIOError)
import System.Posix.Signals (nullSignal, signalProcess)
import qualified System.Process as Process
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit

type AgentStack =
  '[ ACP.ACP
   , Matrix.Matrix
   , LifecycleEffect.Lifecycle
   , Chat.Chat
   , AgentAudit.AgentAudit
   , AgentEffect.Agent
   , ChatLog.ChatLog
   , LLM.LLM
   , Media.Media
   , Skills.Skills
   , Memory.Memory
   , Scheduler.Scheduler
   , Typst.Typst
   , HTTP.HTTP
   , ResourceEffect.Resource
   , Concurrency.Concurrency
   , StorageEffect.Storage
   , KatipE
   , Prim
   , Fail
   , Concurrent
   , Timeout
   , Process
   , FileSystem
   , IOE
   ]

data NoopChatDriver =
  NoopChatDriver

instance Driver.ChatDriver NoopChatDriver where
  driverPlatform _ = PlatformTelegram

data AgentMockChatDriver es = AgentMockChatDriver
  { agentReply :: IncomingMessage -> Text -> Eff es (Either Text MessageId)
  , agentReplyAudio :: IncomingMessage -> Text -> Maybe Text -> Eff es (Either Text MessageId)
  , agentUploadFile :: IncomingMessage -> FilePath -> Maybe Text -> Eff es (Either Text MessageId)
  , agentEditMessage :: IncomingMessage -> MessageId -> Text -> Eff es Bool
  , agentMessageOutPolicy :: IncomingMessage -> Eff es Chat.MessageOutPolicy
  , agentFetchMessage :: IncomingMessage -> MessageId -> Eff es (Maybe ReferencedMessage)
  , agentUserAvatar :: IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
  }

instance Driver.ChatDriver (AgentMockChatDriver es0) where
  type ChatDriverEffects (AgentMockChatDriver es0) es = es ~ es0
  driverPlatform _ = PlatformTelegram
  sendReplyMessage driver = driver.agentReply
  replyAudio driver = driver.agentReplyAudio
  uploadFile driver = driver.agentUploadFile
  editMessage driver message messageId body =
    driver.agentEditMessage message messageId body
  messageOutPolicy driver = driver.agentMessageOutPolicy
  getMessageContent driver = driver.agentFetchMessage
  getUserAvatar driver = driver.agentUserAvatar

defaultAgentMockChatDriver :: AgentMockChatDriver es
defaultAgentMockChatDriver =
  AgentMockChatDriver
    { agentReply = \_ _ -> pure (Left "noop reply")
    , agentReplyAudio = \_ _ _ -> pure (Right "audio")
    , agentUploadFile = \_ _ _ -> pure (Right "upload")
    , agentEditMessage = \_ _ _ -> pure False
    , agentMessageOutPolicy = \_ -> pure (Chat.ChunkedMessage 1800)
    , agentFetchMessage = \_ _ -> pure Nothing
    , agentUserAvatar = \_ _ -> pure Nothing
    }

data ChatMock = ChatMock
  { replies :: !(Maybe (IORef.IORef [Text]))
  , replyId :: !(Maybe MessageId)
  , userAvatar :: !(Maybe Aeson.Value)
  }

data StreamingAnswer = StreamingAnswer
  { chunks :: ![Text]
  , answer :: !LLM.ChatAnswer
  }

data LegacyAuditRow = LegacyAuditRow
  { legacy_id :: Selda.RowID
  , legacy_run_id :: Text
  , legacy_occurred_at :: UTCTime
  , legacy_linked_message_id :: Maybe Text
  , legacy_parent_message_id :: Maybe Text
  , legacy_event_json :: Text
  }
  deriving (Generic)

instance Selda.SqlRow LegacyAuditRow

legacyAuditRows :: Selda.Table LegacyAuditRow
legacyAuditRows =
  Selda.tableFieldMod "audit_log"
    [ #legacy_id Selda.:- Selda.untypedAutoPrimary
    , #legacy_run_id Selda.:- Selda.index
    , #legacy_linked_message_id Selda.:- Selda.index
    , #legacy_parent_message_id Selda.:- Selda.index
    ]
    (fromMaybe "" . Text.stripPrefix "legacy_")

data ImageGenerateCall = ImageGenerateCall
  { prompt :: !Text
  , imageRefs :: ![Text]
  , options :: !LLM.ImageRequestOptions
  }
  deriving (Eq, Show)

data ImageEditCall = ImageEditCall
  { prompt :: !Text
  , imageRefs :: ![Text]
  , maskRef :: !(Maybe Text)
  , options :: !LLM.ImageRequestOptions
  }
  deriving (Eq, Show)

data AudioGenerateCall = AudioGenerateCall
  { prompt :: !Text
  , options :: !LLM.AudioRequestOptions
  }
  deriving (Eq, Show)

main :: IO ()
main =
  defaultMain $
    testGroup "agent"
      [ testCase "schedule tool creates a queryable pending schedule" testScheduleToolCreatesQueryableSchedule
      , testCase "tool argument DSL shares schema, decoding, and reader context" testToolArgumentDSL
      , testCase "FM domain tools require literal feature intent" testFMDomainToolIntentIsolation
      , testCase "dynamic tool visibility is frozen for each model turn" testDynamicToolVisibilitySnapshot
      , testCase "Python nested tools reuse frozen registry dispatch without transcript entries" testPythonNestedRegistryDispatch
      , testCase "Python configuration controls visibility, description, and nested budget" testPythonConfiguration
      , testCase "Python action middleware preserves root and managed-child metadata" testPythonActionMiddlewareMetadata
      , testCase "Python resource protocol resumes the existing continuation" testPythonResourceContinuation
      , testCase "Python rejects every program control before starting a sibling" testPythonRejectsProgramControls
      , testCase "Python nested ids, ordering, and budget are host-owned" testPythonNestedIdsAndBudget
      , testCase "Python outer and nested calls retain distinct audit and message scopes" testPythonControlAndNestedScopes
      , testCase "malformed Python call keeps outer lifecycle without entering interpreter" testPythonMalformedControlLifecycle
      , testCase "mixed Python model batch reaches no control or ordinary call scope" testPythonMixedBatchHasNoCallSideEffects
      , testCase "tool tags are enabled from the thread transcript" testToolTagsEnabledFromTranscript
      , testCase "ACP client file tools are ACP-only" testAcpClientFileToolsAreAcpOnly
      , testCase "terminal and sandbox tools respect their scopes" testTerminalAndSandboxToolScopes
      , testCase "matrix request tool is Matrix-superuser-only" testMatrixRequestToolScope
      , testCase "chat model status is public and switching is superuser-only" testChatModelToolScopes
      , testCase "subagent lifecycle is shared within a chat" testSubAgentLifecycle
      , testCase "subagent wait operations avoid polling without cancelling work" testSubAgentWaitOperations
      , testCase "send reply tool uses chat effect and records bot message" testSendReplyToolUsesChatEffect
      , testCase "tool reply middleware normalizes reply images" testToolReplyMiddlewareNormalizesReplyImages
      , testCase "tool reply middleware rejects uncached remote images" testToolReplyMiddlewareRejectsUncachedRemoteImages
      , testCase "send file tool uploads via chat effect" testSendFileToolUploadsViaChatEffect
      , testCase "send file tool reports upload failure" testSendFileToolReportsUploadFailure
      , testCase "send file tool is noisy and superuser-only" testSendFileToolIsNoisyAndSuperuserOnly
      , testCase "chat upload filenames are safe basenames" testUploadFileName
      , testCase "send_media uploads cached media for normal users" testSendMediaToolUploadsCachedMedia
      , testCase "chatlog tool filters by sender" testChatLogToolFiltersBySender
      , testCase "chatlog tool treats a blank sender as no filter" testChatLogToolIgnoresBlankSender
      , testCase "current sender chatlog tool queries matching sender messages globally" testCurrentSenderChatLogToolQueriesChatLog
      , testCase "user avatar tool queries chat effect" testUserAvatarToolQueriesChatEffect
      , testCase "user avatar tool requires user id" testUserAvatarToolRequiresUserId
      , testCase "user avatar tool rejects zero user id" testUserAvatarToolRejectsZeroUserId
      , testCase "typst_render tool renders and sends an image" testTypstToImageToolRendersAndSendsImage
      , testCase "image_edit tool edits current message image and sends result" testEditImageToolEditsCurrentMessageImageAndSendsResult
      , testCase "ask handler passes referenced images to image_edit tool" testAskHandlerPassesReferencedImagesToEditImageTool
      , testCase "ask handler includes referenced image URLs in text context" testAskHandlerIncludesReferencedImageUrlsInTextContext
      , testCase "ask handler includes current and referenced files in text context" testAskHandlerIncludesFilesInTextContext
      , testCase "ask handler requires mention for group non-bot replies but not private replies" testAskHandlerHandlesReplyToNonBotMessageByChatKind
      , testCase "LLM failure replies remain linked to their thread" testLLMFailureReplyLinksThread
      , testCase "image_generate tool passes image request options" testGenerateImageToolPassesImageRequestOptions
      , testCase "image_cache tool caches image for current context" testViewImageToolCachesImageForContext
      , testCase "model image refs inline cached images as base64" testModelImageRefEncodesCachedImage
      , testCase "image_view rejects local file URLs without reading them" testViewImageToolRejectsLocalFileUrls
      , testCase "media_text reads cached media text slices" testReadMediaTextToolReadsCachedSlices
      , testCase "media_to_file returns cache path without media context" testMediaToFileReturnsCachePath
      , testCase "audio_generate tool uses configured audio options and sends audio" testGenerateAudioToolUsesConfiguredAudioOptions
      , testCase "image_edit tool passes image request options" testEditImageToolPassesImageRequestOptions
      , testCase "agent request merges current message context into system prompt" testAgentRequestMergesCurrentMessageContextIntoSystemPrompt
      , testCase "agent compacts old transcript context before model turn" testAgentCompactsOldTranscriptContextBeforeModelTurn
      , testCase "agent announces context compaction" testAgentAnnouncesContextCompaction
      , testCase "agent resumes nested continuations with JSON values" testAgentResumesNestedContinuations
      , testCase "agent rejects continuation calls mixed with sibling tools" testAgentRejectsConcurrentContinuationCalls
      , testCase "agent does not intercept unexposed continuation tools" testAgentDoesNotInterceptUnexposedContinuationTools
      , testCase "agent may resume a continuation at the tool limit" testAgentResumesContinuationAtToolLimit
      , testCase "agent steering continues after a final answer" testAgentSteeringContinuesAfterFinalAnswer
      , testCase "agent steering preserves surrounding model middleware" testAgentSteeringPreservesModelMiddleware
      , testCase "agent steering waits for complete tool results" testAgentSteeringWaitsForToolResults
      , testCase "agent steering clears saved continuations" testAgentSteeringClearsContinuations
      , testCase "ask handler system context includes configured bot and sender ids" testAskHandlerSystemContextIncludesConfiguredBotAndSenderIds
      , testCase "ask handler system context uses message bot id" testAskHandlerSystemContextUsesMessageBotId
      , testCase "ask handler injects startup skill metadata" testAskHandlerInjectsStartupSkillMetadata
      , testCase "ask handler routes replies to active aliases as steering" testAskHandlerRoutesActiveReplyAsSteering
      , testCase "group reply from another sender does not continue a finished user alias" testGroupReplyDoesNotContinueFinishedUserAlias
      , testCase "ask handler continues a finished bot reply" testAskHandlerContinuesFinishedBotReply
      , testCase "load_skill loads only advertised skill instructions" testLoadSkillLoadsAdvertisedSkillInstructions
      , testCase "ask handler announces noisy tool calls without internal ids" testAskHandlerAnnouncesNoisyToolCallsWithoutInternalIds
      , testCase "ask handler announces each noisy tool type only once" testAskHandlerAnnouncesEachNoisyToolTypeOnlyOnce
      , testCase "ask handler hides streamed narration before tool calls" testAskHandlerHidesStreamedNarrationBeforeToolCalls
      , testCase "long final replies are replayed as Matrix edit chunks" testLongFinalRepliesUseMatrixEditChunks
      , testCase "short final replies remain a single chunk" testShortFinalRepliesRemainSingleChunk
      , testCase "ask handler automatically continues after a tool limit" testAskHandlerAutomaticallyContinuesAfterToolLimit
      , testCase "ask handler bounds automatic tool-limit continuations" testAskHandlerBoundsAutomaticToolLimitContinuations
      , testCase "agent streams tool request content before tool notification" testAgentStreamsToolRequestContentBeforeToolNotification
      , testCase "agent audit records tool events" testAgentAuditRecordsToolEvents
      , testCase "agent audit migrates legacy records without changing ids" testAgentAuditMigratesLegacyRecords
      , testCase "SQLite storage pool runs actions concurrently" testSQLiteStoragePoolRunsActionsConcurrently
      , testCase "thread stats accumulate the replied branch" testThreadStatsAccumulateRepliedBranch
      , testCase "thread stats show active running tools from every alias" testThreadStatsShowActiveRunningTools
      , testCase "thread audit is scoped by platform, chat, and run occurrence" testThreadAuditScope
      , testCase "agent audit recent records exclude synthetic restarted runs" testAgentAuditRecentRecordsExcludeSyntheticRestartedRuns
      , testCase "agent audit storage omits large tool results" testAgentAuditStorageOmitsLargeToolResults
      , testCase "agent omits large tool results only after one model turn consumes them" testAgentOmitsLargeToolResultAfterOneModelTurnConsumesIt
      , testCase "agent hard-limits immediate tool results" testAgentHardLimitsImmediateToolResults
      , testCase "agent audit records structured tool failure category" testAgentAuditRecordsStructuredToolFailureCategory
      , testCase "chat answer JSON remains object compatible" testChatAnswerJsonRemainsObjectCompatible
      , testCase "reply body parses structured content" testReplyBodyParsesStructuredContent
      , testCase "LLM tool request content streams immediately when enabled" testLLMToolRequestContentStreamsImmediatelyWhenEnabled
      , testCase "LLM streaming response preserves token usage" testLLMStreamingResponsePreservesTokenUsage
      , testCase "LLM image stream request asks only for final image" testLLMImageStreamRequestAsksOnlyForFinalImage
      , testCase "LLM audio speech request includes provider options" testLLMAudioSpeechRequestIncludesProviderOptions
      , testCase "LLM image stream completed event yields final image bytes" testLLMImageStreamCompletedEventYieldsFinalImage
      , testCase "LLM image edit stream completed event yields final image bytes" testLLMImageEditStreamCompletedEventYieldsFinalImage
      , testCase "LLM image stream ignores partial event without final image" testLLMImageStreamIgnoresPartialEventWithoutFinalImage
      , testCase "LLM log JSON truncates base64 image payloads" testLLMLogJsonTruncatesBase64ImagePayloads
      , testCase "LLM streaming effect preserves yielded chunks" testLLMStreamingEffectPreservesYieldedChunks
      , testCase "empty chat reply sends nothing" testEmptyChatReplySendsNothing
      , testCase "empty streaming chunks are ignored" testEmptyStreamingChunksAreIgnored
      , testCase "empty chunked streaming reply sends nothing" testEmptyChunkedStreamingReplySendsNothing
      , testCase "chat streaming chunks replies and yields updates" testChatStreamingChunksRepliesAndYieldsUpdates
      , testCase "editable segmented replies open a new tail after tool messages" testEditableSegmentedRepliesOpenNewTail
      , testCase "segmented replies flush final open segment" testSegmentedRepliesFlushFinalOpenSegment
      , testCase "editable chat streaming splits long replies and yields aliases" testEditableChatStreamingSplitsLongReplies
      , testCase "chunked active thread aliases every sent reply" testChunkedActiveThreadAliasesEverySentReply
      , testCase "halt command cancels active run for current thread message" testHaltCommandCancelsCurrentThreadMessage
      , testCase "deleting a bot reply halts its active run" testDeletingBotReplyHaltsActiveRun
      , testCase "halt command prefers replied thread message over current message" testHaltCommandPrefersRepliedThreadMessage
      , testCase "halt command requires prompt sender or superuser" testHaltCommandRequiresOwnerOrSuperuser
      , testCase "active thread is listed before a platform reply exists" testActiveThreadWithoutPlatformReply
      , testCase "active thread ids are stable and chat scoped" testActiveThreadIdsAreStableAndChatScoped
      , testCase "active thread steering is FIFO, aliased, and closed atomically" testActiveThreadSteeringLifecycle
      , testCase "fetch_url max_uses limits fetch calls" testWebFetchMaxUsesLimitsCalls
      , testCase "thread replies keep parent and child snapshots" testThreadRepliesKeepSnapshots
      , testCase "thread branches do not overwrite siblings" testThreadBranchesDoNotOverwriteSiblings
      , testCase "thread lookup is scoped by chat" testThreadLookupIsScopedByChat
      , testCase "thread branches persist through SQLite reload" testThreadBranchesPersistThroughSQLiteReload
      , testCase "concurrent thread stores allocate distinct ids" testConcurrentThreadStoresAllocateDistinctIds
      , testCase "thread cache miss loads evicted parent from SQLite" testThreadCacheMissLoadsEvictedParent
      , testCase "thread storage omits large tool results" testThreadStorageOmitsLargeToolResults
      , testCase "transcript omits base64 generated image context" testTranscriptOmitsBase64GeneratedImageContext
      , testCase "LLM request omits base64 generated image context" testLLMRequestOmitsBase64GeneratedImageContext
      , testCase "transcript JSON remains list compatible" testTranscriptJsonRemainsListCompatible
      , testCase "memory tool manages current sender memory" testMemoryToolManagesCurrentSenderMemory
      , testCase "memory tool manages current chat memory" testMemoryToolManagesCurrentChatMemory
      , testCase "non-owner cannot change shared chat memory" testChatMemoryRejectsNonOwnerWrites
      , testCase "memory tool enforces non-superuser length limit" testMemoryToolEnforcesLengthLimit
      , testCase "private persona tool lets QQ private users manage only themselves" testPrivatePersonaToolSelfManagement
      , testCase "private persona tool lets superusers manage and list all users" testPrivatePersonaToolOwnerManagement
      , testCase "private persona prompt is isolated to the matching QQ private user" testPrivatePersonaPromptIsolation
      , testCase "private persona global default applies unless a user overrides it" testPrivatePersonaGlobalDefault
      , testCase "QQ private agent replies are standalone like group replies" testQqPrivateAgentReplyIsStandalone
      , testCase "group persona tool is owner-managed and group-readable" testGroupPersonaToolPermissions
      , testCase "group persona prompt is isolated by QQ group" testGroupPersonaPromptIsolation
      , testCase "group persona global default applies unless a group overrides it" testGroupPersonaGlobalDefault
      , testCase "member style follows a QQ member across groups" testMemberStyleAcrossGroups
      , testCase "group members can manage only their own reply style" testMemberStyleToolPermissions
      , testCase "memory update rolls back when git commit fails" testMemoryUpdateRollsBackOnCommitFailure
      , testCase "run_bash captures stdout and stderr" testRunBashCapturesStdoutAndStderr
      , testCase "run_bash kills timed out process" testRunBashKillsTimedOutProcess
      , testCase "run_bash kills process group when cancelled" testRunBashKillsProcessGroupWhenCancelled
      , testCase "LLM response timeout summary is concise" testLLMResponseTimeoutSummaryIsConcise
      , testCase "LLM exception summary describes LLM errors" testLLMExceptionSummaryDescribesLLMErrors
      , testCase "LLM status error summary is concise" testLLMStatusErrorSummaryIsConcise
      , testCase "agent failure summarizes Req HTTP errors" testAgentFailureSummarizesReqHttpErrors
      ]

testToolArgumentDSL :: IO ()
testToolArgumentDSL = do
  let definition :: AgentTool.Tool (Eff '[])
      definition =
        AgentTool.withDescription "Echo a value."
        $ AgentTool.tool "echo"
            ( (AgentTool.requiredArgument
                ("value", Aeson.object ["type" Aeson..= ("string" :: Text)])
                :: AgentTool.ToolArgument Text)
            , AgentTool.withDefault (2 :: Int)
                (AgentTool.optionalArgument
                  ("count", Aeson.object ["type" Aeson..= ("integer" :: Text)]))
            )
            \value count -> do
              context <- AgentTool.askToolContext
              metadata <- AgentTool.askToolCallMetadata
              let command = context.askCommand
                  runId = metadata.agentRunId
              pure (Agent.toolText [i|#{value}:#{count}:#{command}:#{runId}|])
      (schema, valid, invalid) = runPureEff do
        resolvedSchema <- AgentTool.resolveToolSchema definition agentContext (startWithUser "test") 0
        runner <- AgentTool.startTool definition agentContext
        validResult <- runner testToolCallMetadata (Aeson.object ["value" Aeson..= ("ok" :: Text)])
        invalidResult <- runner testToolCallMetadata (Aeson.object [])
        pure (resolvedSchema, validResult, invalidResult)
      encodedSchema = jsonText schema
  assertBool "schema includes the required field" ("\"required\":[\"value\"]" `Text.isInfixOf` encodedSchema)
  assertBool "schema includes the optional field" ("\"count\"" `Text.isInfixOf` encodedSchema)
  AgentTypes.toolResultContent valid @?= "ok:2:!ask:agent-test"
  assertBool "the same argument spec rejects missing required input" ("value" `Text.isInfixOf` AgentTypes.toolResultContent invalid)

testFMDomainToolIntentIsolation :: IO ()
testFMDomainToolIntentIsolation = do
  let libraryTool = FMDomainTools.fmLibrarySearchTool :: AgentTool.Tool (Eff '[HTTP.HTTP])
      contestTool = FMDomainTools.fmContestSearchTool :: AgentTool.Tool (Eff '[HTTP.HTTP])
      imageTool = ImageTools.generateImageTool :: AgentTool.Tool (Eff '[Chat.Chat, LLM.LLM])
      contextFor rawText = agentContext{Agent.input = inputWithImages rawText []}
      allowed definition rawText = AgentTool.toolAllowed definition (contextFor rawText)
  assertBool "detective cases are not typing contests" (not (allowed contestTool "最近有哪些案子不"))
  assertBool "outfit analysis is not a library request" (not (allowed libraryTool "fm 看看今日穿搭"))
  assertBool "outfit analysis is not image generation" (not (allowed imageTool "fm 看看今日穿搭"))
  assertBool "an explicit article request exposes the library" (allowed libraryTool "fm 来一篇闻闻味道")
  assertBool "an explicit contest request exposes contest tools" (allowed contestTool "fm 发今天赛文")
  assertBool "literal image generation remains available" (allowed imageTool "fm 生成一张今日穿搭图")

testDynamicToolVisibilitySnapshot :: IO ()
testDynamicToolVisibilitySnapshot = do
  visible <- IORef.newIORef True
  let definition :: AgentTool.Tool (Eff '[Concurrent, IOE])
      definition =
        AgentTool.hideUnlessM (\_ _ _ -> liftIO (IORef.readIORef visible))
        . AgentTool.withDescription "Dynamically visible."
        $ AgentTool.tool "dynamic" AgentTool.noArguments
            (pure (Agent.toolText "ran"))
      call = toolCall "call-1" "dynamic" (Aeson.object [])
  (initialSchemas, beforeRefresh, refreshedSchemas, afterRefresh) <-
    runEff $ runConcurrent do
      running <- ToolRegistry.startToolRun agentContext definition
      initialSchemas <- ToolRegistry.resolveToolSchemas (startWithUser "test") 0 [running]
      liftIO (IORef.writeIORef visible False)
      beforeRefresh <- ToolRegistry.runToolCall agentContext testToolCallMetadata [definition] [running] call
      refreshedSchemas <- ToolRegistry.resolveToolSchemas (startWithUser "test") 1 [running]
      afterRefresh <- ToolRegistry.runToolCall agentContext testToolCallMetadata [definition] [running] call
      pure (initialSchemas, beforeRefresh, refreshedSchemas, afterRefresh)
  map (.name) initialSchemas @?= ["dynamic"]
  AgentTypes.toolResultContent beforeRefresh @?= "ran"
  assertBool "the next turn hides the tool" (null refreshedSchemas)
  assertBool "hidden tool calls are rejected after the next schema snapshot" ("Unknown tool" `Text.isInfixOf` AgentTypes.toolResultContent afterRefresh)

testPythonNestedRegistryDispatch :: IO ()
testPythonNestedRegistryDispatch = do
  visible <- IORef.newIORef True
  nestedResults <- IORef.newIORef Nothing
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "outer-python" PythonTools.runPythonToolName (Aeson.object ["code" Aeson..= ("compose" :: Text)])]
    , chatAnswer "done" []
    ]
  let dynamicTool :: AgentTool.Tool (Eff AgentStack)
      dynamicTool =
        AgentTool.hideUnlessM (\_ _ _ -> liftIO (IORef.readIORef visible))
        . AgentTool.withDescription "Visible for the frozen model turn."
        $ AgentTool.tool "python_dynamic" AgentTool.noArguments (pure (Agent.toolText "dynamic-ran"))
      deniedTool :: AgentTool.Tool (Eff AgentStack)
      deniedTool =
        AgentTool.allowWhen (const False)
        . AgentTool.withDescription "Denied in this context."
        $ AgentTool.tool "python_denied" AgentTool.noArguments (pure (Agent.toolText "must-not-run"))
      interpreter runTools _ _ _ = do
        liftIO (IORef.writeIORef visible False)
        results <- runTools 1
          ( PythonProgram.PythonToolCall "python_dynamic" "{}"
          :| [PythonProgram.PythonToolCall "python_denied" "{}"]
          )
        liftIO (IORef.writeIORef nestedResults (Just results))
        pure (PythonProgram.PythonCompleted "nested complete")
  transcript <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    runtime <- startTestRuntime 3 agentContext [PythonTools.runPythonTool, dynamicTool, deniedTool]
    let pythonRuntime = PythonMiddleware.withPythonRunner interpreter runtime
    _outputs S.:> result <- S.toList (Agent.agentStream pythonRuntime (startWithEnabledTools ["special"] "compose"))
    pure result.transcript
  Just (dynamicResult :| [deniedResult]) <- IORef.readIORef nestedResults
  AgentTypes.toolResultContent dynamicResult @?= "dynamic-ran"
  fmap (.category) (AgentTypes.toolResultFailure deniedResult) @?= Just AgentTypes.PermissionDenied
  toolOutputs transcript @?= ["nested complete"]
  assertBool "nested calls must not add assistant/tool transcript entries" $
    null
      [ call
      | message <- Foldable.toList transcript.messages
      , call <- message.toolCalls
      , "/python/" `Text.isInfixOf` call.id
      ]

testPythonConfiguration :: IO ()
testPythonConfiguration = do
  let disabledPython = AgentTypes.defaultPythonConfig{AgentTypes.enabled = False}
      configuredPython = AgentTypes.PythonConfig
        { enabled = True
        , wallTimeoutSeconds = 7
        , cpuSeconds = 5
        , memoryMiB = 96
        , maxToolCalls = 3
        }
      configuredContext = agentContext
        { Agent.toolConfig = Agent.defaultToolConfig{AgentTypes.python = configuredPython}
        }
      disabledContext = agentContext
        { Agent.toolConfig = Agent.defaultToolConfig{AgentTypes.python = disabledPython}
        }
      tool = PythonTools.runPythonTool :: AgentTool.Tool (Eff '[IOE])
  assertBool "disabled Python config hides py" (not (AgentTool.toolAllowed tool disabledContext))
  schema <- runEff $ AgentTool.resolveToolSchema tool configuredContext (startWithUser "") 0
  let description = foldMap (.description) schema
  assertBool "py description uses configured limits" $
    "Configured limits: 7 s wall time, 5 s CPU, 96 MiB address space, and 3 total nested tool calls."
      `Text.isInfixOf` description

testPythonActionMiddlewareMetadata :: IO ()
testPythonActionMiddlewareMetadata = do
  observedMetadata <- IORef.newIORef ([] :: [Agent.ToolCallMetadata])
  observedOwners <- IORef.newIORef ([] :: [Maybe Concurrency.Handle])
  nestedResults <- IORef.newIORef ([] :: [[Agent.ToolResult]])
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "root-python" PythonTools.runPythonToolName (Aeson.object ["code" Aeson..= ("root" :: Text)])]
    , chatAnswer "root finished" []
    , chatAnswer "" [toolCall "enable-python" AgentTool.toolEnableName (Aeson.object ["tags" Aeson..= (["special"] :: [Text])])]
    , chatAnswer "" [toolCall "child-python" PythonTools.runPythonToolName (Aeson.object ["code" Aeson..= ("child" :: Text)])]
    , chatAnswer "child finished" []
    ]
  let configuredPython = AgentTypes.defaultPythonConfig{AgentTypes.maxToolCalls = 1}
      configuredContext = agentContext
        { Agent.toolConfig = Agent.defaultToolConfig{AgentTypes.python = configuredPython}
        }
      markerTool :: AgentTool.Tool (Eff AgentStack)
      markerTool = AgentTool.withDescription "Records action-level Python metadata." $
        AgentTool.tool "python_metadata_marker" AgentTool.noArguments do
          metadata <- AgentTool.askToolCallMetadata
          liftIO $ IORef.modifyIORef' observedMetadata (<> [metadata])
          pure (Agent.toolText "recorded")
      interpreter runTools _message resourceOwner _request = do
        successful <- runTools 1 (PythonProgram.PythonToolCall "python_metadata_marker" "{}" :| [])
        exhausted <- runTools 2 (PythonProgram.PythonToolCall "python_metadata_marker" "{}" :| [])
        liftIO $ do
          IORef.modifyIORef' observedOwners (<> [resourceOwner])
          IORef.modifyIORef' nestedResults (<> [toList successful <> toList exhausted])
        pure (PythonProgram.PythonCompleted "python complete")
      descendantMetadata = testToolCallMetadata
        { Agent.agentRunId = "agent-parent"
        , AgentTypes.originRunId = "agent-root"
        }
  runAgentWith answers (ChatMock Nothing Nothing Nothing) $
    PythonMiddleware.withPythonMiddleware interpreter do
      runtime <- startTestRuntime 3 configuredContext [PythonTools.runPythonTool, markerTool]
      AgentEffect.withRun runtime \configured ->
        void . S.toList $ Agent.agentStream configured (startWithEnabledTools ["special"] "run root Python")
      let subagentTool = SubAgentTools.subagentTool [MetaTools.toolEnableTool, PythonTools.runPythonTool, markerTool]
      subagentRun <- AgentTool.startTool subagentTool configuredContext
      created <- subagentRun descendantMetadata $
        Aeson.object
          [ "op" Aeson..= ("create" :: Text)
          , "name" Aeson..= ("python-child" :: Text)
          , "system_prompt" Aeson..= ("Use Python." :: Text)
          , "tools" Aeson..= ([PythonTools.runPythonToolName, "python_metadata_marker"] :: [Text])
          , "ttl_minutes" Aeson..= (5 :: Int)
          ]
      let resourceId = fromMaybe (error "missing Python subagent id") $
            Text.stripPrefix "Subagent created: " (AgentTypes.toolResultContent created)
      sent <- subagentRun descendantMetadata $
        Aeson.object
          [ "op" Aeson..= ("send" :: Text)
          , "resource" Aeson..= resourceId
          , "prompt" Aeson..= ("run child Python" :: Text)
          ]
      liftIO $ AgentTypes.toolResultContent sent @?= "Prompt sent."
      workers <- Concurrency.list
      let worker = fromMaybe (error "missing Python subagent worker") $
            find ((== "subagent") . (.label)) workers.entries
      Concurrency.await Concurrency.Handle{handleId = worker.id}
  [rootMetadata, childMetadata] <- IORef.readIORef observedMetadata
  [rootOwner, childOwner] <- IORef.readIORef observedOwners
  rootMetadata.agentRunId @?= rootMetadata.originRunId
  rootMetadata.resourceOwner @?= Nothing
  rootOwner @?= Nothing
  childMetadata.originRunId @?= "agent-root"
  assertBool "managed child receives its own run id" (childMetadata.agentRunId /= descendantMetadata.agentRunId)
  assertBool "managed child metadata owns resources through its worker" (isJust childMetadata.resourceOwner)
  childOwner @?= childMetadata.resourceOwner
  results <- IORef.readIORef nestedResults
  length results @?= 2
  for_ results \case
    [successful, exhausted] -> do
      AgentTypes.toolResultContent successful @?= "recorded"
      fmap (.category) (AgentTypes.toolResultFailure exhausted) @?= Just AgentTypes.BudgetExhausted
    other -> assertFailure [i|expected successful and exhausted nested results, got #{length other}|]

testPythonResourceContinuation :: IO ()
testPythonResourceContinuation = do
  seen <- IORef.newIORef ([] :: [Text])
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "outer-python" PythonTools.runPythonToolName (Aeson.object ["code" Aeson..= ("compose" :: Text)])]
    , chatAnswer "continued once" []
    ]
  let nestedTool :: AgentTool.Tool (Eff AgentStack)
      nestedTool = AgentTool.withDescription "Records protocol dispatch." $
        AgentTool.tool "python_protocol_marker" AgentTool.noArguments do
          liftIO $ IORef.modifyIORef' seen (<> ["ran"])
          pure (Agent.toolText "nested result")
  transcript <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    let interpreter runTools _request _outerCall _pythonRequest = do
          results <- runTools 1 (PythonProgram.PythonToolCall "python_protocol_marker" "{}" :| [])
          liftIO $ AgentTypes.toolResultContent (head results) @?= "nested result"
          pure (PythonProgram.PythonCompleted "resource complete")
    runtime <- startTestRuntime 3 agentContext [PythonTools.runPythonTool, nestedTool]
    _outputs S.:> result <- S.toList $ Agent.agentStream
      (PythonMiddleware.withPythonRunner interpreter runtime)
      (startWithEnabledTools ["special"] $ Text.unlines
        [ "import cosmobot"
        , "result = cosmobot.run_tool('python_protocol_marker', {})"
        , "assert result['content'] == 'nested result'"
        , "cosmobot.complete('resource complete')"
        ])
    pure result.transcript
  IORef.readIORef seen >>= (@?= ["ran"])
  toolOutputs transcript @?= ["resource complete"]
  assertBool "the existing continuation consumed the second model answer" . null =<< IORef.readIORef answers

testPythonRejectsProgramControls :: IO ()
testPythonRejectsProgramControls = do
  started <- IORef.newIORef (0 :: Int)
  observed <- IORef.newIORef ([] :: [Agent.ToolResult])
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "outer-python" PythonTools.runPythonToolName (Aeson.object ["code" Aeson..= ("controls" :: Text)])]
    , chatAnswer "done" []
    ]
  let markerTool :: AgentTool.Tool (Eff AgentStack)
      markerTool =
        AgentTool.withDescription "Records dispatch."
        $ AgentTool.tool "python_marker" AgentTool.noArguments do
            liftIO $ IORef.atomicModifyIORef' started (\count -> (count + 1, ()))
            pure (Agent.toolText "ran")
      controls = ["py", "tool_enable", "capture_continuation", "resume_continuation"]
      interpreter runTools _ _ _ = do
        results <- forM (zip [1 ..] controls) \(rpcId, control) ->
          runTools rpcId
            ( PythonProgram.PythonToolCall "python_marker" "{}"
            :| [PythonProgram.PythonToolCall control "{}"]
            )
        liftIO (IORef.writeIORef observed (concatMap toList results))
        pure (PythonProgram.PythonCompleted "controls rejected")
  runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    runtime <- startTestRuntime 3 agentContext [PythonTools.runPythonTool, markerTool]
    void . S.toList $ Agent.agentStream
      (PythonMiddleware.withPythonRunner interpreter runtime)
      (startWithEnabledTools ["special"] "reject controls")
  IORef.readIORef started >>= (@?= 0)
  results <- IORef.readIORef observed
  length results @?= 8
  assertBool "every rejected batch member receives the same argument failure" $
    all
      (maybe False (\failure -> failure.category == AgentTypes.PermanentArgumentError && "program-control" `Text.isInfixOf` failure.userMessage) . AgentTypes.toolResultFailure)
      results

testPythonNestedIdsAndBudget :: IO ()
testPythonNestedIdsAndBudget = do
  started <- IORef.newIORef (0 :: Int)
  observed <- IORef.newIORef Nothing
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "outer-python" PythonTools.runPythonToolName (Aeson.object ["code" Aeson..= ("budget" :: Text)])]
    , chatAnswer "done" []
    ]
  let markerTool :: AgentTool.Tool (Eff AgentStack)
      markerTool =
        AgentTool.withDescription "Counts nested dispatch."
        $ AgentTool.tool "python_budgeted" AgentTool.noArguments do
            liftIO $ IORef.atomicModifyIORef' started (\count -> (count + 1, ()))
            pure (Agent.toolText "ran")
      batch = PythonProgram.PythonToolCall "python_budgeted" "{}" :| replicate 15 (PythonProgram.PythonToolCall "python_budgeted" "{}")
      finalBatch = PythonProgram.PythonToolCall "python_budgeted" "{}" :| replicate 13 (PythonProgram.PythonToolCall "python_budgeted" "{}")
      interpreter runTools _ _ _ = do
        rejected <- runTools 1
          ( PythonProgram.PythonToolCall "python_budgeted" "{}"
          :| [PythonProgram.PythonToolCall "py" "{}"]
          )
        duplicate <- runTools 1 (PythonProgram.PythonToolCall "python_budgeted" "{}" :| [])
        fullBatches <- traverse (\rpcId -> runTools rpcId batch) (2 :| [3, 4])
        finalResults <- runTools 5 finalBatch
        exhausted <- runTools 6 (PythonProgram.PythonToolCall "python_budgeted" "{}" :| [])
        liftIO (IORef.writeIORef observed (Just (fullBatches <> (finalResults :| []), rejected, duplicate, exhausted)))
        pure (PythonProgram.PythonCompleted "budget checked")
  runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    runtime <- startTestRuntime 3 agentContext [PythonTools.runPythonTool, markerTool]
    let withIds = runtime
          { AgentCore.aroundToolCall = \turn call context action -> do
              _ <- runtime.aroundToolCall turn call context action
              pure (Agent.toolText call.id)
          }
    void . S.toList $ Agent.agentStream
      (PythonMiddleware.withPythonRunner interpreter withIds)
      (startWithEnabledTools ["special"] "check budget")
  IORef.readIORef started >>= (@?= 62)
  Just (successful, rejected, duplicate :| [], exhausted :| []) <- IORef.readIORef observed
  let returnedIds = map AgentTypes.toolResultContent (concatMap toList (toList successful))
      expectedIds =
        [[i|outer-python/python/#{rpcId}/#{index}|] | rpcId <- [2 :: Int .. 4], index <- [0 :: Int .. 15]]
        <> [[i|outer-python/python/5/#{index}|] | index <- [0 :: Int .. 13]]
  returnedIds @?= expectedIds
  assertBool "synthetic ids are bounded before audit middleware" $
    all ((<= 256) . Text.length) returnedIds
  assertBool "rejected calls consume the independent nested-call budget" $
    all (isJust . AgentTypes.toolResultFailure) rejected
  fmap (.category) (AgentTypes.toolResultFailure duplicate) @?= Just AgentTypes.PermanentArgumentError
  fmap (.category) (AgentTypes.toolResultFailure exhausted) @?= Just AgentTypes.BudgetExhausted

testPythonControlAndNestedScopes :: IO ()
testPythonControlAndNestedScopes = do
  events <- IORef.newIORef ([] :: [Agent.Event])
  replies <- IORef.newIORef ([] :: [Text])
  recorded <- IORef.newIORef ([] :: [Text])
  remembered <- IORef.newIORef ([] :: [Maybe MessageId])
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "outer-python" PythonTools.runPythonToolName (Aeson.object ["code" Aeson..= ("emit" :: Text)])]
    , chatAnswer "done" []
    ]
  let emittingTool :: AgentTool.Tool (Eff AgentStack)
      emittingTool =
        AgentTool.withDescription "Emits one platform message."
        $ AgentTool.tool "python_emit" AgentTool.noArguments do
            void $ Chat.replyTo testMessage "nested emitted"
            pure (Agent.toolText "nested result")
      largeResult = Text.replicate 5000 "x"
      interpreter runTools _ _ _ = do
        _ :| [] <- runTools 1 (PythonProgram.PythonToolCall "python_emit" "{}" :| [])
        pure (PythonProgram.PythonCompleted largeResult)
      observer event = do
        liftIO $ IORef.modifyIORef' events (<> [event])
        pure $ case event of
          Agent.ToolCallStarted{} -> ObservationTypes.ObservationContext (Just 1)
          _ -> ObservationTypes.emptyObservationContext
  runAgentWith answers (ChatMock (Just replies) (Just "42") Nothing) do
    runtime <- startTestRuntime 3 agentContext [PythonTools.runPythonTool, emittingTool]
    let observed =
          PythonMiddleware.withPythonRunner interpreter
          . ToolResultCompaction.withToolResultCompaction
          . AgentObservation.withObservation observer
          . ToolMiddleware.withToolMessage
          . ToolMiddleware.withToolFailureRecovery
          $ runtime
        sink = Agent.ToolEmittedMessageSink \messageId ->
          liftIO $ IORef.modifyIORef' remembered (<> [messageId])
        program =
          (Agent.withRecordingToolSelfMessages \body ->
            liftIO $ IORef.modifyIORef' recorded (<> [body]))
          . Agent.withLinkingToolEmittedMessagesToThread sink
          $ observed
    void . S.toList $ Agent.agentStream program (startWithEnabledTools ["special"] "compose and emit")
  allEvents <- IORef.readIORef events
  let lifecycle = toolLifecycle allEvents
  lifecycle @?=
    [ ("started", "outer-python", "py")
    , ("started", "outer-python/python/1/0", "python_emit")
    , ("finished", "outer-python/python/1/0", "python_emit")
    , ("finished", "outer-python", "py")
    ]
  let finishedResults =
        [ (toolName, result)
        | Agent.ToolCallFinished{toolName, result} <- allEvents
        ]
      finishedResult name = snd <$> find ((== name) . fst) finishedResults
  finishedResult "python_emit" @?= Just "nested result"
  assertBool "outer control result should use the normal audit compaction view" $
    maybe False ("[tool result omitted;" `Text.isPrefixOf`) (finishedResult "py")
  IORef.readIORef recorded >>= (@?= ["nested emitted"])
  IORef.readIORef remembered >>= (@?= [Just "42"])
  IORef.readIORef replies >>= \case
    [emitted] ->
      emitted @?= "nested emitted"
    sent ->
      assertFailure [i|expected only the nested message, got #{show sent :: String}|]
  where
    toolLifecycle :: [Agent.Event] -> [(Text, Text, Text)]
    toolLifecycle = mapMaybe \case
      Agent.ToolCallStarted{toolCall = call} ->
        Just ("started", call.id, call.name)
      Agent.ToolCallFinished{toolCallId, toolName} ->
        Just ("finished", toolCallId, toolName)
      _ ->
        Nothing

testPythonMalformedControlLifecycle :: IO ()
testPythonMalformedControlLifecycle = do
  events <- IORef.newIORef ([] :: [Agent.Event])
  replies <- IORef.newIORef ([] :: [Text])
  interpreterEntries <- IORef.newIORef (0 :: Int)
  answers <- IORef.newIORef
    [ chatAnswer ""
        [LLM.ToolCall "outer-malformed" PythonTools.runPythonToolName "[]"]
    , chatAnswer "done" []
    ]
  let interpreter _ _ _ _ = do
        liftIO $ IORef.modifyIORef' interpreterEntries (+ 1)
        pure (PythonProgram.PythonCompleted "must not enter")
      observer event = do
        liftIO $ IORef.modifyIORef' events (<> [event])
        pure $ case event of
          Agent.ToolCallStarted{} -> ObservationTypes.ObservationContext (Just 1)
          _ -> ObservationTypes.emptyObservationContext
  runAgentWith answers (ChatMock (Just replies) (Just "42") Nothing) do
    runtime <- startTestRuntime 3 agentContext [PythonTools.runPythonTool]
    let program =
          PythonMiddleware.withPythonRunner interpreter
          . ToolResultCompaction.withToolResultCompaction
          . AgentObservation.withObservation observer
          . ToolMiddleware.withToolMessage
          . ToolMiddleware.withToolFailureRecovery
          $ runtime
    void . S.toList $ Agent.agentStream program (startWithEnabledTools ["special"] "malformed")
  IORef.readIORef interpreterEntries >>= (@?= 0)
  lifecycle <- mapMaybe controlLifecycle <$> IORef.readIORef events
  lifecycle @?=
    [ ("started", "outer-malformed", "py")
    , ("finished:permanent_argument_error", "outer-malformed", "py")
    ]
  sent <- IORef.readIORef replies
  sent @?= []
  where
    controlLifecycle :: Agent.Event -> Maybe (Text, Text, Text)
    controlLifecycle = \case
      Agent.ToolCallStarted{toolCall = call} ->
        Just ("started", call.id, call.name)
      Agent.ToolCallFinished{toolCallId, toolName, status} ->
        Just ("finished:" <> status, toolCallId, toolName)
      _ ->
        Nothing

testPythonMixedBatchHasNoCallSideEffects :: IO ()
testPythonMixedBatchHasNoCallSideEffects = do
  controlScopes <- IORef.newIORef (0 :: Int)
  ordinaryScopes <- IORef.newIORef (0 :: Int)
  interpreterEntries <- IORef.newIORef (0 :: Int)
  markerRuns <- IORef.newIORef (0 :: Int)
  answers <- IORef.newIORef
    [ chatAnswer ""
        [ toolCall "outer-python" PythonTools.runPythonToolName (Aeson.object ["code" Aeson..= ("mixed" :: Text)])
        , toolCall "ordinary" "python_marker" (Aeson.object [])
        ]
    , chatAnswer "done" []
    ]
  let markerTool :: AgentTool.Tool (Eff AgentStack)
      markerTool =
        AgentTool.withDescription "Must not run in a mixed control batch."
        $ AgentTool.tool "python_marker" AgentTool.noArguments do
            liftIO $ IORef.modifyIORef' markerRuns (+ 1)
            pure (Agent.toolText "ran")
      interpreter _ _ _ _ = do
        liftIO $ IORef.modifyIORef' interpreterEntries (+ 1)
        pure (PythonProgram.PythonCompleted "must not enter")
  runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    runtime <- startTestRuntime 3 agentContext [PythonTools.runPythonTool, markerTool]
    let counted = runtime
          { AgentCore.aroundControlCall = \_ _ _ action ->
              liftIO (IORef.modifyIORef' controlScopes (+ 1)) >> action
          , AgentCore.aroundToolCall = \_ _ _ action ->
              liftIO (IORef.modifyIORef' ordinaryScopes (+ 1)) >> action
          }
    void . S.toList $ Agent.agentStream
      (PythonMiddleware.withPythonRunner interpreter counted)
      (startWithEnabledTools ["special"] "reject mixed")
  traverse_ (>>= (@?= 0)) [IORef.readIORef controlScopes, IORef.readIORef ordinaryScopes, IORef.readIORef interpreterEntries, IORef.readIORef markerRuns]

testToolTagsEnabledFromTranscript :: IO ()
testToolTagsEnabledFromTranscript = do
  let alwaysTool :: AgentTool.Tool (Eff '[Concurrent, IOE])
      alwaysTool =
        AgentTool.withDescription "Always available."
        $ AgentTool.tool "always_test" AgentTool.noArguments (pure (Agent.toolText "always"))
      chatTool :: AgentTool.Tool (Eff '[Concurrent, IOE])
      chatTool =
        AgentTool.tagged [chatTag]
        . AgentTool.withDescription "Chat tool."
        $ AgentTool.tool "chat_test" AgentTool.noArguments (pure (Agent.toolText "chat"))
      workTool :: AgentTool.Tool (Eff '[Concurrent, IOE])
      workTool =
        AgentTool.tagged [workTag]
        . AgentTool.withDescription "Work tool."
        $ AgentTool.tool "work_test" AgentTool.noArguments (pure (Agent.toolText "work"))
      definitions = [MetaTools.toolEnableTool, alwaysTool, chatTool, workTool]
      chatEnabled = startWithEnabledTools ["chat"] "continue"
      reloaded =
        fromRight (error "failed to reload enabled-tool transcript")
          (Aeson.eitherDecode (Aeson.encode chatEnabled))
  (initialSchemas, chatSchemas, reloadedSchemas, invalidSchemas, initialGroups, chatGroups) <-
    runEff $ runConcurrent do
      running <- traverse (ToolRegistry.startToolRun agentContext) definitions
      initialSchemas <- ToolRegistry.resolveToolSchemas (startWithUser "hello") 0 running
      chatSchemas <- ToolRegistry.resolveToolSchemas chatEnabled 1 running
      reloadedSchemas <- ToolRegistry.resolveToolSchemas reloaded 2 running
      invalidSchemas <- ToolRegistry.resolveToolSchemas (startWithEnabledTools ["unknown"] "continue") 3 running
      pure
        ( initialSchemas
        , chatSchemas
        , reloadedSchemas
        , invalidSchemas
        , ToolRegistry.enabledToolGroups (startWithUser "hello") running
        , ToolRegistry.enabledToolGroups chatEnabled running
        )
  map (.name) initialSchemas @?= ["tool_enable", "always_test"]
  map (.name) chatSchemas @?= ["tool_enable", "always_test", "chat_test"]
  map (.name) reloadedSchemas @?= map (.name) chatSchemas
  map (.name) invalidSchemas @?= map (.name) initialSchemas
  initialGroups @?= [("essential", 2)]
  chatGroups @?= [("essential", 2), ("chat", 1)]
  let enableSchema = fromMaybe (error "missing tool_enable schema") (find ((== "tool_enable") . (.name)) initialSchemas)
      description = enableSchema.description
      parameters = jsonText enableSchema.parameters
  assertBool "description encourages one early enable call" ("as early as possible" `Text.isInfixOf` description)
  assertBool "description lists chat tools" (all (`Text.isInfixOf` description) ["chat:", "chat_test"])
  assertBool "description lists work tools" (all (`Text.isInfixOf` description) ["work:", "work_test"])
  assertBool "parameters enumerate available tags" (all (`Text.isInfixOf` parameters) ["\"chat\"", "\"work\""])

testScheduleToolCreatesQueryableSchedule :: IO ()
testScheduleToolCreatesQueryableSchedule = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "schedule" (Aeson.object ["op" Aeson..= ("create" :: Text), "delay_seconds" Aeson..= (60 :: Int), "prompt" Aeson..= ("check oven" :: Text)])]
    , chatAnswer "" [toolCall "call-2" "schedule" (Aeson.object ["op" Aeson..= ("list" :: Text)])]
    , chatAnswer "" [toolCall "call-3" "schedule" (Aeson.object ["op" Aeson..= ("delete" :: Text), "schedule_id" Aeson..= (1 :: Integer)])]
    , chatAnswer "scheduled" []
    ]
  (answer, transcript, schedules) <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    result <- runTestAgent 4 agentContext AgentTools.defaultTools (startWithEnabledTools ["work"] "remind me")
    pending <- Scheduler.listScheduledMessages testMessage
    pure (fst result, snd result, pending)
  answer @?= "scheduled"
  assertBool "delete removes the schedule" (null schedules)
  let output = Text.unlines (toolOutputs transcript)
  assertBool "list output includes scheduled prompt" ("check oven" `Text.isInfixOf` output)
  assertBool "delete output confirms removal" ("Schedule 1 has been removed." `Text.isInfixOf` output)

testAcpClientFileToolsAreAcpOnly :: IO ()
testAcpClientFileToolsAreAcpOnly = do
  let readTool = FileTools.acpReadClientFileTool :: AgentTool.Tool (Eff AgentStack)
      writeTool = FileTools.acpWriteClientFileTool :: AgentTool.Tool (Eff AgentStack)
      acpContext = agentContext{Agent.message = testMessage{platform = PlatformACP, chatAliases = ["session-1"]}}
  AgentTool.toolName readTool @?= "acp_read_client_file"
  AgentTool.toolName writeTool @?= "acp_write_client_file"
  assertBool "read tool should be hidden outside ACP" (not (AgentTool.toolAllowed readTool agentContext))
  assertBool "write tool should be hidden outside ACP" (not (AgentTool.toolAllowed writeTool agentContext))
  assertBool "read tool should be visible for ACP" (AgentTool.toolAllowed readTool acpContext)
  assertBool "write tool should be visible for ACP" (AgentTool.toolAllowed writeTool acpContext)

testTerminalAndSandboxToolScopes :: IO ()
testTerminalAndSandboxToolScopes = do
  answers <- IORef.newIORef []
  let terminalTool = TerminalTools.terminalTool :: AgentTool.Tool (Eff AgentStack)
      sandboxTool = SandboxTools.sandboxTool :: AgentTool.Tool (Eff AgentStack)
      trustedBashTool = runBashTool :: AgentTool.Tool (Eff AgentStack)
      workspaceTool = WorkspaceTools.workspaceTool :: AgentTool.Tool (Eff AgentStack)
      acpContext = agentContext{Agent.message = testMessage{platform = PlatformACP, chatAliases = ["session-1"]}}
      missingIdentity = agentContext{Agent.message = testMessage{senderId = Nothing}}
  (sandboxSchema, workspaceSchema, trustedBashSchema) <-
    runAgentWith answers (ChatMock Nothing Nothing Nothing) do
      (,,)
        <$> encodedToolParameters sandboxTool
        <*> encodedToolParameters workspaceTool
        <*> encodedToolParameters trustedBashTool
  AgentTool.toolName terminalTool @?= "terminal"
  assertBool "terminal tool should be hidden outside ACP" (not (AgentTool.toolAllowed terminalTool agentContext))
  assertBool "terminal tool should be visible for ACP" (AgentTool.toolAllowed terminalTool acpContext)
  AgentTool.toolName sandboxTool @?= "sandbox"
  assertBool "sandbox bash schema should require a script" ("\"script\"" `Text.isInfixOf` sandboxSchema)
  assertBool "sandbox create schema should expose ttl_minutes" ("ttl_minutes" `Text.isInfixOf` sandboxSchema)
  assertBool "sandbox schema should expose media copies" (all (`Text.isInfixOf` sandboxSchema) ["file_to_media", "media_to_file"])
  assertBool "sandbox bash schema should not expose command ids" (not ("command_id" `Text.isInfixOf` sandboxSchema))
  assertBool "sandbox bash schema should not expose async actions" (not ("\"action\"" `Text.isInfixOf` sandboxSchema))
  assertBool "run_bash schema should not expose sandboxes" (not ("sandbox" `Text.isInfixOf` trustedBashSchema))
  assertBool "sandbox tool should be visible to non-superusers" (AgentTool.toolAllowed sandboxTool agentContext)
  assertBool "sandbox tool should require resource identity" (not (AgentTool.toolAllowed sandboxTool missingIdentity))
  AgentTool.toolName workspaceTool @?= "workspace"
  assertBool "workspace create schema should expose ttl_minutes" ("ttl_minutes" `Text.isInfixOf` workspaceSchema)
  assertBool "workspace should be hidden from non-superusers" (not (AgentTool.toolAllowed workspaceTool agentContext))
  assertBool "workspace should be visible to superusers" (AgentTool.toolAllowed workspaceTool superuserContext)
  assertBool "workspace should require resource identity" (not (AgentTool.toolAllowed workspaceTool superuserContext{Agent.message = testMessage{senderId = Nothing}}))

testMatrixRequestToolScope :: IO ()
testMatrixRequestToolScope = do
  let tool = MatrixTools.matrixRequestTool :: AgentTool.Tool (Eff AgentStack)
      matrixAdmin = superuserContext{Agent.message = testMessage{Message.platform = PlatformMatrix}}
      matrixUser = matrixAdmin{Agent.superuser = False}
  assertBool "matrix request is hidden outside Matrix" (not (AgentTool.toolAllowed tool superuserContext))
  assertBool "matrix request is hidden from Matrix non-superusers" (not (AgentTool.toolAllowed tool matrixUser))
  assertBool "matrix request is available to Matrix superusers" (AgentTool.toolAllowed tool matrixAdmin)

testChatModelToolScopes :: IO ()
testChatModelToolScopes = do
  let statusTool = ModelTools.chatModelStatusTool :: AgentTool.Tool (Eff AgentStack)
      switchTool = ModelTools.chatModelSwitchTool :: AgentTool.Tool (Eff AgentStack)
      resetTool = ModelTools.chatModelResetTool :: AgentTool.Tool (Eff AgentStack)
  AgentTool.toolName statusTool @?= "chat_model_status"
  AgentTool.toolName switchTool @?= "chat_model_switch"
  AgentTool.toolName resetTool @?= "chat_model_reset"
  assertBool "model status is available to regular users" (AgentTool.toolAllowed statusTool agentContext)
  assertBool "model switch is hidden from regular users" (not (AgentTool.toolAllowed switchTool agentContext))
  assertBool "model reset is hidden from regular users" (not (AgentTool.toolAllowed resetTool agentContext))
  assertBool "model switch is available to superusers" (AgentTool.toolAllowed switchTool superuserContext)
  assertBool "model reset is available to superusers" (AgentTool.toolAllowed resetTool superuserContext)

testSubAgentLifecycle :: IO ()
testSubAgentLifecycle = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "block" "block" (Aeson.object [])]
    , chatAnswer "finished" []
    ]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    started <- MVar.newEmptyMVar
    finish <- MVar.newEmptyMVar
    let blockTool = AgentTool.tool "block" AgentTool.noArguments do
          childMetadata <- AgentTool.askToolCallMetadata
          MVar.putMVar started childMetadata
          MVar.takeMVar finish
          pure (Agent.toolText "finished")
        availableTools = [MetaTools.toolEnableTool, SandboxTools.sandboxTool, blockTool]
        tool = SubAgentTools.subagentTool availableTools
        descendantMetadata =
          testToolCallMetadata
            { Agent.agentRunId = "agent-child"
            , AgentTypes.originRunId = "agent-root"
            }
        otherContext = agentContext{Agent.message = testMessage{senderId = Just "other"}}
        otherChatContext = agentContext{Agent.message = testMessage{Message.chatId = Just 999}}
    schema <- encodedToolParameters tool
    liftIO $ assertBool "subagent create schema should expose ttl_minutes" ("ttl_minutes" `Text.isInfixOf` schema)
    createRun <- AgentTool.startTool tool agentContext
    tooShort <- createRun testToolCallMetadata (Aeson.object ["op" Aeson..= ("create" :: Text), "ttl_minutes" Aeson..= (4 :: Int)])
    liftIO $ assertBool "subagent rejects TTL below five minutes" ("at least 5" `Text.isInfixOf` AgentTypes.toolResultContent tooShort)
    created <- createRun descendantMetadata (Aeson.object ["op" Aeson..= ("create" :: Text), "name" Aeson..= ("researcher" :: Text), "system_prompt" Aeson..= ("Research carefully." :: Text), "tools" Aeson..= (["sandbox", "block"] :: [Text]), "ttl_minutes" Aeson..= (5 :: Int)])
    let resourceId = fromMaybe (error "missing subagent id") (Text.stripPrefix "Subagent created: " (AgentTypes.toolResultContent created))
    liftIO $ resourceId @?= "researcher"
    otherChatRun <- AgentTool.startTool tool otherChatContext
    void $ otherChatRun testToolCallMetadata (Aeson.object ["op" Aeson..= ("create" :: Text), "name" Aeson..= ("hidden" :: Text), "system_prompt" Aeson..= ("" :: Text), "tools" Aeson..= ([] :: [Text]), "ttl_minutes" Aeson..= (5 :: Int)])
    listed <- createRun testToolCallMetadata (Aeson.object ["op" Aeson..= ("list" :: Text)])
    liftIO $ AgentTypes.toolResultContent listed @?= "[\"researcher\"]"
    sandboxRun <- AgentTool.startTool SandboxTools.sandboxTool agentContext
    listedSandboxes <- sandboxRun testToolCallMetadata (Aeson.object ["op" Aeson..= ("list" :: Text)])
    liftIO $ AgentTypes.toolResultContent listedSandboxes @?= "[]"
    workspaceRun <- AgentTool.startTool WorkspaceTools.workspaceTool superuserContext
    listedWorkspaces <- workspaceRun testToolCallMetadata (Aeson.object ["action" Aeson..= ("list" :: Text)])
    liftIO $ AgentTypes.toolResultContent listedWorkspaces @?= "[]"
    sendRun <- AgentTool.startTool tool otherContext
    sent <- sendRun descendantMetadata (Aeson.object ["op" Aeson..= ("send" :: Text), "resource" Aeson..= resourceId, "prompt" Aeson..= ("work" :: Text)])
    liftIO $ AgentTypes.toolResultContent sent @?= "Prompt sent."
    childMetadata <- MVar.takeMVar started
    liftIO $ do
      childMetadata.originRunId @?= "agent-root"
      assertBool "child has its own run id" (childMetadata.agentRunId /= descendantMetadata.agentRunId)
      assertBool "child resources are owned by its worker" (isJust childMetadata.resourceOwner)
    steered <- sendRun testToolCallMetadata (Aeson.object ["op" Aeson..= ("steer" :: Text), "resource" Aeson..= resourceId, "prompt" Aeson..= ("change direction" :: Text)])
    liftIO $ AgentTypes.toolResultContent steered @?= "Steer queued."
    void $ sendRun testToolCallMetadata (Aeson.object ["op" Aeson..= ("steer" :: Text), "resource" Aeson..= resourceId, "prompt" Aeson..= ("then verify" :: Text)])
    let access = fromRight (error "missing resource access") (ResourceEffect.accessFromMessage otherContext.message)
    rootResources <- ResourceEffect.listCreatedByRuns access ["agent-root"]
    liftIO $ map (.resourceId) rootResources @?= ["researcher"]
    generatingDetail <- ResourceEffect.detail access resourceId
    liftIO $ generatingDetail @?= Right (Text.intercalate "\n"
      [ "status: generating"
      , "tools: sandbox, block"
      , "system prompt:\nResearch carefully."
      , "output:\nGenerating"
      , "life: 5m"
      ])
    MVar.putMVar finish ()
    workers <- Concurrency.list
    let worker = fromMaybe (error "missing subagent worker") (find ((== "subagent") . (.label)) workers.entries)
    Concurrency.await Concurrency.Handle{handleId = worker.id}
    renamed <- sendRun testToolCallMetadata (Aeson.object ["op" Aeson..= ("rename" :: Text), "resource" Aeson..= resourceId, "name" Aeson..= ("reviewer" :: Text)])
    liftIO $ AgentTypes.toolResultContent renamed @?= "Subagent renamed: reviewer"
    queried <- sendRun testToolCallMetadata (Aeson.object ["op" Aeson..= ("query" :: Text), "resource" Aeson..= ("reviewer" :: Text)])
    liftIO $ AgentTypes.toolResultContent queried @?= "finished"
    requests <- liftIO (IORef.readIORef captured)
    liftIO $ case requests of
      [_initial, afterTool] ->
        chatMessageTextsByRole "user" afterTool @?= ["work", "change direction", "then verify"]
      other ->
        assertFailure [i|expected two subagent model requests, got #{length other}|]
    rejectedSteer <- sendRun testToolCallMetadata (Aeson.object ["op" Aeson..= ("steer" :: Text), "resource" Aeson..= ("reviewer" :: Text), "prompt" Aeson..= ("too late" :: Text)])
    liftIO $ AgentTypes.toolResultContent rejectedSteer @?= "Subagent is not generating."
    finishedDetail <- ResourceEffect.detail access "reviewer"
    liftIO $ assertBool "subagent detail reports final output" (either (const False) ("output:\nfinished" `Text.isInfixOf`) finishedDetail)
    destroyed <- sendRun testToolCallMetadata (Aeson.object ["op" Aeson..= ("delete" :: Text), "resource" Aeson..= ("reviewer" :: Text)])
    liftIO $ AgentTypes.toolResultContent destroyed @?= "Subagent destroyed."

testSubAgentWaitOperations :: IO ()
testSubAgentWaitOperations = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "block-1" "block" (Aeson.object [])]
    , chatAnswer "" [toolCall "block-2" "block" (Aeson.object [])]
    , chatAnswer "finished" []
    , chatAnswer "finished" []
    ]
  runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    firstGate <- MVar.newEmptyMVar
    secondGate <- MVar.newEmptyMVar
    firstStarted <- MVar.newEmptyMVar
    secondStarted <- MVar.newEmptyMVar
    let blockTool = AgentTool.tool "block" AgentTool.noArguments do
          context <- AgentTool.askToolContext
          let (started, gate)
                | context.systemContext == "first" = (firstStarted, firstGate)
                | otherwise = (secondStarted, secondGate)
          MVar.putMVar started ()
          MVar.takeMVar gate
          pure (Agent.toolText "finished")
        tool = SubAgentTools.subagentTool [blockTool]
    run <- AgentTool.startTool tool agentContext
    let create name =
          run testToolCallMetadata $
            Aeson.object
              [ "op" Aeson..= ("create" :: Text)
              , "name" Aeson..= (name :: Text)
              , "system_prompt" Aeson..= (name :: Text)
              , "tools" Aeson..= (["block"] :: [Text])
              , "ttl_minutes" Aeson..= (5 :: Int)
              ]
        sendPrompt resourceId =
          run testToolCallMetadata $
            Aeson.object
              [ "op" Aeson..= ("send" :: Text)
              , "resource" Aeson..= (resourceId :: Text)
              , "prompt" Aeson..= ("work" :: Text)
              ]
        wait operation =
          run testToolCallMetadata $
            Aeson.object
              [ "op" Aeson..= (operation :: Text)
              , "resources" Aeson..= (["first", "second"] :: [Text])
              ]
        decodeResult result =
          fromRight (error "invalid subagent wait JSON") $
            Aeson.eitherDecodeStrict (TextEncoding.encodeUtf8 (AgentTypes.toolResultContent result))

    void (create "first")
    void (create "second")
    void (sendPrompt "first")
    void (sendPrompt "second")
    MVar.takeMVar firstStarted
    MVar.takeMVar secondStarted

    (waitedAny, ()) <- Async.concurrently
      (wait "wait_any")
      (MVar.putMVar secondGate ())
    liftIO $ decodeResult waitedAny @?=
      Aeson.object
        [ "resource" Aeson..= ("second" :: Text)
        , "output" Aeson..= ("finished" :: Text)
        ]

    firstStillRunning <- run testToolCallMetadata $
      Aeson.object
        [ "op" Aeson..= ("query" :: Text)
        , "resource" Aeson..= ("first" :: Text)
        ]
    liftIO $ AgentTypes.toolResultContent firstStillRunning @?= "The subagent is still generating."

    (waitedAll, ()) <- Async.concurrently
      (wait "wait_all")
      (MVar.putMVar firstGate ())
    liftIO $ decodeResult waitedAll @?=
      Aeson.toJSON
        [ Aeson.object
            [ "resource" Aeson..= ("first" :: Text)
            , "output" Aeson..= ("finished" :: Text)
            ]
        , Aeson.object
            [ "resource" Aeson..= ("second" :: Text)
            , "output" Aeson..= ("finished" :: Text)
            ]
        ]

testSendReplyToolUsesChatEffect :: IO ()
testSendReplyToolUsesChatEffect = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "send_reply" (Aeson.object ["text" Aeson..= ("hello" :: Text), "image_urls" Aeson..= ["https://example.test/image.png" :: Text]])]
    , chatAnswer "sent" []
    ]
  replies <- IORef.newIORef ([] :: [Text])
  recorded <- IORef.newIORef ([] :: [Text])
  remembered <- IORef.newIORef ([] :: [Maybe MessageId])
  (answer, _) <- runAgentWith answers (ChatMock (Just replies) (Just "42") Nothing) do
    runAgentWithToolMessageCapture 4 (agentContext{Agent.input = inputWithImages "再发一条消息" []}) AgentTools.defaultTools (startWithEnabledTools ["chat"] "再发一条消息") recorded remembered
  answer @?= "sent"
  IORef.readIORef replies >>= (@?= ["😻 FM：正在调用 send_reply 工具…", "😻 FM：hello\n[image] https://example.test/image.png"])
  IORef.readIORef recorded >>= (@?= ["😻 FM：正在调用 send_reply 工具…", "😻 FM：hello\n[image] https://example.test/image.png"])
  IORef.readIORef remembered >>= (@?= [Just "42", Just "42"])

testToolReplyMiddlewareNormalizesReplyImages :: IO ()
testToolReplyMiddlewareNormalizesReplyImages = do
  replies <- IORef.newIORef ([] :: [Text])
  runEff $
    runConcurrent $
    runMediaNormalizingRefs $
      Chat.runChatWith defaultAgentMockChatDriver { agentReply = \_ body -> do
          liftIO $ IORef.modifyIORef' replies (<> [body])
          pure (Right "42")
        } do
          runtime <- startTestRuntime maxBound agentContext []
          let program = Agent.withNormalizingToolReplies runtime
          _ <- program.aroundToolCall 1 (toolCall "call-1" "send_reply" (Aeson.object [])) HList.HNil do
            void $ Chat.replyTo testMessage (ReplyBody.imageDirective "https://example.test/image.png")
            pure (Agent.toolText "done")
          pure ()
  (map Text.strip <$> IORef.readIORef replies) >>= (@?= ["[image] media:https://example.test/image.png"])

testToolReplyMiddlewareRejectsUncachedRemoteImages :: IO ()
testToolReplyMiddlewareRejectsUncachedRemoteImages = do
  replies <- IORef.newIORef ([] :: [Text])
  result <- runEff $
    runConcurrent $
    runMediaLeavingRefs $
      Chat.runChatWith defaultAgentMockChatDriver { agentReply = \_ body -> do
          liftIO $ IORef.modifyIORef' replies (<> [body])
          pure (Right "42")
        } do
          runtime <- startTestRuntime maxBound agentContext []
          let program = Agent.withNormalizingToolReplies runtime
          program.aroundToolCall 1 (toolCall "call-1" "send_reply" (Aeson.object [])) HList.HNil do
            sent <- Chat.replyTo testMessage (ReplyBody.imageDirective "https://example.test/image.png")
            pure (Agent.toolText if null (rights sent) then Text.intercalate "\n" (lefts sent) else "sent")
  AgentTypes.toolResultContent result @?= "Image reply contains remote image URLs that could not be cached: https://example.test/image.png"
  IORef.readIORef replies >>= (@?= [])

testSendFileToolUploadsViaChatEffect :: IO ()
testSendFileToolUploadsViaChatEffect = do
  uploads <- IORef.newIORef ([] :: [(FilePath, Maybe Text)])
  replies <- IORef.newIORef ([] :: [Text])
  result <- runSendFileTool replies \_ path fileName -> do
    liftIO $ IORef.modifyIORef' uploads (<> [(path, fileName)])
    pure (Right "900")
  case result of
    Agent.ToolSucceeded{content} ->
      assertBool "tool result should describe sent file" ("Sent file /tmp/report.txt" `Text.isInfixOf` content)
    Agent.ToolFailed{failure} ->
      assertFailure [i|expected file upload success, got #{show failure :: String}|]
  IORef.readIORef uploads >>= (@?= [("/tmp/report.txt", Nothing)])
  IORef.readIORef replies >>= (@?= [])

testSendFileToolReportsUploadFailure :: IO ()
testSendFileToolReportsUploadFailure = do
  replies <- IORef.newIORef ([] :: [Text])
  result <- runSendFileTool replies \_ _ _ ->
    pure (Left "upload failed")
  case result of
    Agent.ToolFailed{failure} -> do
      failure.userMessage @?= "发送文件失败：upload failed"
      failure.category @?= Agent.ExternalServiceUnavailable
    Agent.ToolSucceeded{} ->
      assertFailure "expected file upload failure"
  IORef.readIORef replies >>= (@?= ["发送文件失败：upload failed"])

testSendFileToolIsNoisyAndSuperuserOnly :: IO ()
testSendFileToolIsNoisyAndSuperuserOnly = do
  let tool = ChatTools.sendFileTool :: AgentTool.Tool (Eff '[Chat.Chat, IOE])
      qqContext =
        superuserContext
          { Agent.message = superuserContext.message{Message.platform = PlatformQQ}
          }
  AgentTool.toolIsNoisy tool @?= True
  AgentTool.toolAllowed tool agentContext @?= False
  AgentTool.toolAllowed tool superuserContext @?= True
  (telegramDescription, qqDescription) <-
    runEff
    . Chat.runChatWith defaultAgentMockChatDriver
    $ do
      telegramSchema <- AgentTool.resolveToolSchema tool superuserContext (startWithUser "") 0
      qqSchema <- AgentTool.resolveToolSchema tool qqContext (startWithUser "") 0
      pure (foldMap (.description) telegramSchema, foldMap (.description) qqSchema)
  assertBool "Telegram description should not mention NapCat" (not ("NapCat" `Text.isInfixOf` telegramDescription))
  assertBool "QQ description should mention NapCat" ("NapCat" `Text.isInfixOf` qqDescription)

testUploadFileName :: IO ()
testUploadFileName = do
  Driver.uploadFileName "/tmp/cache.bin" Nothing @?= "cache.bin"
  Driver.uploadFileName "/tmp/cache.bin" (Just "../report.txt") @?= "report.txt"
  Driver.uploadFileName "/tmp/cache.bin" (Just " ") @?= "cache.bin"

testSendMediaToolUploadsCachedMedia :: IO ()
testSendMediaToolUploadsCachedMedia =
  withSQLiteTempPath "send-media" \dbPath ->
    withTempDir "send-media-cache" \dir -> do
      uploads <- IORef.newIORef ([] :: [(FilePath, Maybe Text)])
      let cfg = MediaConfig.defaultConfig{MediaConfig.cacheDir = dir </> "cache"}
          driver = defaultAgentMockChatDriver
            { agentUploadFile = \_ path fileName -> do
                liftIO $ IORef.modifyIORef' uploads (<> [(path, fileName)])
                pure (Right "902")
            }
          runStack =
            runFileSystem
              . runProcess
              . runFail
              . runConcurrent
              . runTestLog
              . StorageSQLite.runStorageSQLitePath dbPath
              . HTTP.runHTTP
              . runTimeout
              . MediaInterpreter.runMedia cfg
              . Chat.runChatWith driver
      runResult <- runEff $ runStack do
        mediaRef <- fromMaybe (error "expected media ref") <$> Media.storeMediaObject Media.MediaObject
          { bytes = Q.fromStrict "generated file"
          , mimeType = "application/octet-stream"
          , sourceName = Just "generated.bin"
          }
        expectedPath <- fromMaybe (error "expected local media path") <$> Media.localMediaPath mediaRef
        let mediaContext = agentContext{Agent.input = inputWithImages "发送文件" []}
        runner <- AgentTool.startTool MediaTools.sendMediaTool mediaContext
        defaultResult <- runner testToolCallMetadata (Aeson.object ["media_id" Aeson..= mediaRef])
        namedResult <- runner testToolCallMetadata (Aeson.object
          [ "media_id" Aeson..= mediaRef
          , "filename" Aeson..= ("../report.bin" :: Text)
          ])
        pure (expectedPath, defaultResult, namedResult)
      (expectedPath, defaultResult, namedResult) <- either assertFailure pure runResult
      let tool = MediaTools.sendMediaTool :: AgentTool.Tool (Eff AgentStack)
      AgentTool.toolIsNoisy tool @?= True
      assertBool "send_media should be available to normal users" (AgentTool.toolAllowed tool (agentContext{Agent.input = inputWithImages "发送文件" []}))
      IORef.readIORef uploads >>= (@?= [(expectedPath, Nothing), (expectedPath, Just "../report.bin")])
      traverse_ assertSendMediaSucceeded [defaultResult, namedResult]
  where
    assertSendMediaSucceeded = \case
      Agent.ToolSucceeded{content} ->
        assertBool "tool result should report sent media" ("Sent media media:mf_" `Text.isInfixOf` content)
      Agent.ToolFailed{failure} ->
        assertFailure [i|send_media failed: #{show failure :: String}|]

runSendFileTool
  :: IORef.IORef [Text]
  -> (IncomingMessage -> FilePath -> Maybe Text -> Eff '[IOE] (Either Text MessageId))
  -> IO Agent.ToolResult
runSendFileTool replies upload =
  runEff $
    Chat.runChatWith
      defaultAgentMockChatDriver
        { agentReply = \_ body -> do
            liftIO $ IORef.modifyIORef' replies (<> [body])
            pure (Right "901")
        , agentUploadFile = upload
        } do
        runner <- AgentTool.startTool ChatTools.sendFileTool superuserContext
        runner testToolCallMetadata (Aeson.object ["path" Aeson..= ("file:///tmp/report.txt" :: Text)])

testCurrentSenderChatLogToolQueriesChatLog :: IO ()
testCurrentSenderChatLogToolQueriesChatLog = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "sender_log" (Aeson.object ["scope" Aeson..= ("global" :: Text), "keywords" Aeson..= ([["needle"] :: [Text]] :: [[Text]]), "limit" Aeson..= (10 :: Int), "before" Aeson..= ("2100-01-01T00:00:00Z" :: Text)])]
    , chatAnswer "found" []
    ]
  (answer, transcript) <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    ChatLog.recordMessage (chatLogMessage 301 "200" 100 "older needle")
    ChatLog.recordMessage (chatLogMessage 302 "201" 100 "other sender needle")
    ChatLog.recordMessage (chatLogMessage 303 "200" 101 "other chat needle")
    ChatLog.recordMessage (chatLogMessage 304 "200" 100 "newer needle")
    runTestAgent 4 (agentContext{Agent.input = inputWithImages "search my history" []}) AgentTools.defaultTools (startWithEnabledTools ["chat"] "search my history")
  answer @?= "found"
  entries <- decodeSingleChatLogToolOutput transcript
  texts <- traverse (either assertFailure pure . AesonTypes.parseEither (Aeson.withObject "chat log entry" (\entry -> entry Aeson..: "text" :: AesonTypes.Parser Text))) entries
  texts @?= ["newer needle", "other chat needle", "older needle"]
  for_ entries \case
    Aeson.Object entry ->
      sort (AesonKeyMap.keys entry) @?= sort (map AesonKey.fromText ["timestamp", "chatId", "senderId", "senderUsername", "messageId", "imageUrls", "text"])
    _ -> assertFailure "expected chat log entry object"

testChatLogToolFiltersBySender :: IO ()
testChatLogToolFiltersBySender = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "chat_log" (Aeson.object ["sender" Aeson..= ("201" :: Text), "limit" Aeson..= (10 :: Int)])]
    , chatAnswer "found" []
    ]
  (_, transcript) <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    ChatLog.recordMessage (chatLogMessage 301 "200" 100 "alice")
    ChatLog.recordMessage (chatLogMessage 302 "201" 100 "bob")
    ChatLog.recordMessage (chatLogMessage 303 "201" 101 "bob elsewhere")
    runTestAgent 4 (agentContext{Agent.input = inputWithImages "聊天记录" []}) AgentTools.defaultTools (startWithEnabledTools ["chat"] "search this chat")
  entries <- decodeSingleChatLogToolOutput transcript
  texts <- traverse (either assertFailure pure . AesonTypes.parseEither (Aeson.withObject "chat log entry" (\entry -> entry Aeson..: "text" :: AesonTypes.Parser Text))) entries
  texts @?= ["bob"]

testChatLogToolIgnoresBlankSender :: IO ()
testChatLogToolIgnoresBlankSender = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "chat_log" (Aeson.object
        [ "before" Aeson..= ("2100-01-01T00:00:00Z" :: Text)
        , "include_bot_messages" Aeson..= True
        , "limit" Aeson..= (100 :: Int)
        , "sender" Aeson..= ("" :: Text)
        , "since" Aeson..= ("1970-01-01T00:00:00Z" :: Text)
        ])]
    , chatAnswer "found" []
    ]
  (_, transcript) <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    ChatLog.recordMessage (chatLogMessage 301 "200" 100 "alice")
    ChatLog.recordMessage (chatLogMessage 302 "201" 100 "bob")
    ChatLog.recordMessage (chatLogMessage 303 "201" 101 "bob elsewhere")
    runTestAgent 4 (agentContext{Agent.input = inputWithImages "show recent messages" []}) AgentTools.defaultTools (startWithEnabledTools ["chat"] "show recent messages")
  entries <- decodeSingleChatLogToolOutput transcript
  texts <- traverse (either assertFailure pure . AesonTypes.parseEither (Aeson.withObject "chat log entry" (\entry -> entry Aeson..: "text" :: AesonTypes.Parser Text))) entries
  texts @?= ["alice", "bob"]

testUserAvatarToolQueriesChatEffect :: IO ()
testUserAvatarToolQueriesChatEffect = do
  let avatar = Aeson.object
        [ "platform" Aeson..= ("telegram" :: Text)
        , "user_id" Aeson..= (200 :: Integer)
        , "avatar_url" Aeson..= ("https://example.test/avatar.jpg" :: Text)
        ]
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "user_avatar" (Aeson.object ["user_id" Aeson..= ("200" :: Text)])]
    , chatAnswer "found" []
    ]
  replies <- IORef.newIORef ([] :: [Text])
  recorded <- IORef.newIORef ([] :: [Text])
  remembered <- IORef.newIORef ([] :: [Maybe MessageId])
  (answer, transcript) <- runAgentWith answers (ChatMock (Just replies) (Just "44") (Just avatar)) do
    runAgentWithToolMessageCapture 4 (agentContext{Agent.input = inputWithImages "看头像" []}) AgentTools.defaultTools (startWithEnabledTools ["chat"] "avatar?") recorded remembered
  answer @?= "found"
  Text.unlines (toolOutputs transcript) @?= jsonText avatar <> "\n"
  imageContextUrls transcript @?= ["https://example.test/avatar.jpg"]
  -- The avatar tool should emit the avatar as a chat image, not only return JSON to the model.
  IORef.readIORef replies >>= (@?= ["😻 FM：正在调用 user_avatar 工具…", "[image] https://example.test/avatar.jpg"])
  IORef.readIORef recorded >>= (@?= ["😻 FM：正在调用 user_avatar 工具…", "[image] https://example.test/avatar.jpg"])
  IORef.readIORef remembered >>= (@?= [Just "44", Just "44"])

testUserAvatarToolRequiresUserId :: IO ()
testUserAvatarToolRequiresUserId = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "user_avatar" (Aeson.object [])]
    , chatAnswer "rejected" []
    ]
  replies <- IORef.newIORef ([] :: [Text])
  (answer, transcript) <- runAgentWith answers (ChatMock (Just replies) (Just "44") Nothing) do
    runTestAgent 4 (agentContext{Agent.input = inputWithImages "看头像" []}) AgentTools.defaultTools (startWithEnabledTools ["chat"] "avatar?")
  answer @?= "rejected"
  Text.unlines (toolOutputs transcript) @?= "Error in $: key \"user_id\" not found\n"
  IORef.readIORef replies >>= (@?= ["😻 FM：正在调用 user_avatar 工具…"])

testUserAvatarToolRejectsZeroUserId :: IO ()
testUserAvatarToolRejectsZeroUserId = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "user_avatar" (Aeson.object ["user_id" Aeson..= (0 :: Integer)])]
    , chatAnswer "rejected" []
    ]
  replies <- IORef.newIORef ([] :: [Text])
  (answer, transcript) <- runAgentWith answers (ChatMock (Just replies) (Just "44") Nothing) do
    runTestAgent 4 (agentContext{Agent.input = inputWithImages "看头像" []}) AgentTools.defaultTools (startWithEnabledTools ["chat"] "avatar?")
  answer @?= "rejected"
  Text.unlines (toolOutputs transcript) @?= "Error in $: user_id must not be 0.\n"
  IORef.readIORef replies >>= (@?= ["😻 FM：正在调用 user_avatar 工具…"])

testTypstToImageToolRendersAndSendsImage :: IO ()
testTypstToImageToolRendersAndSendsImage = do
  let source = "#set page(width: auto, height: auto)\nHello from Typst"
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "typst_render" (Aeson.object ["source" Aeson..= source, "format" Aeson..= ("png" :: Text), "caption" Aeson..= ("demo" :: Text)])]
    , chatAnswer "sent" []
    ]
  replies <- IORef.newIORef ([] :: [Text])
  rendered <- IORef.newIORef ([] :: [Text])
  recorded <- IORef.newIORef ([] :: [Text])
  remembered <- IORef.newIORef ([] :: [Maybe MessageId])
  (answer, _) <- runAgentWithTypst rendered answers (ChatMock (Just replies) (Just "43") Nothing) do
    runAgentWithToolMessageCapture 4 agentContext AgentTools.defaultTools (startWithEnabledTools ["work"] "render typst") recorded remembered
  answer @?= "sent"
  IORef.readIORef rendered >>= (@?= [source])
  IORef.readIORef replies >>= (@?= ["😻 FM：正在调用 typst_render 工具…", "[image] file:///tmp/cosmobot-agent-spec-typst.png"])
  IORef.readIORef recorded >>= (@?= ["😻 FM：正在调用 typst_render 工具…", "[image] file:///tmp/cosmobot-agent-spec-typst.png"])
  IORef.readIORef remembered >>= (@?= [Just "43", Just "43"])

testEditImageToolEditsCurrentMessageImageAndSendsResult :: IO ()
testEditImageToolEditsCurrentMessageImageAndSendsResult = do
  let inputImage = "https://example.test/input.png"
      maskImage = "https://example.test/mask.png"
      editedMediaRef = "media:mf_edited"
      editedImage = "[image] " <> editedMediaRef
      message = testMessageWithImages [inputImage]
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "image_edit" (Aeson.object ["prompt" Aeson..= ("make it brighter" :: Text), "mask_image_url" Aeson..= maskImage])]
    , chatAnswer "done" []
    ]
  editCalls <- IORef.newIORef ([] :: [ImageEditCall])
  replies <- IORef.newIORef ([] :: [Text])
  recorded <- IORef.newIORef ([] :: [Text])
  remembered <- IORef.newIORef ([] :: [Maybe MessageId])
  (answer, transcript) <- runAgentWithImageEdit answers editCalls editedImage (ChatMock (Just replies) (Just "47") Nothing) do
    runAgentWithToolMessageCapture 4 (agentContext{Agent.message = message, Agent.input = inputWithImages message.text message.imageUrls}) AgentTools.defaultTools (startWithEnabledTools ["work"] "edit this") recorded remembered
  answer @?= "done"
  IORef.readIORef editCalls >>= (@?= [ImageEditCall "make it brighter" [inputImage] (Just maskImage) LLM.defaultImageRequestOptions])
  IORef.readIORef replies >>= assertElem editedImage
  IORef.readIORef recorded >>= assertElem editedImage
  assertBool "tool result should include edited media id" (editedMediaRef `Text.isInfixOf` Text.unlines (toolOutputs transcript))
  imageContextUrls transcript @?= [editedMediaRef]

testAskHandlerPassesReferencedImagesToEditImageTool :: IO ()
testAskHandlerPassesReferencedImagesToEditImageTool = do
  let referencedImage = "https://example.test/replied.png"
      editedImage = "[image] data:image/png;base64,edited"
      prompt = "make the replied image brighter"
      referenced = ReferencedMessage
        { messageId = Just "70001"
        , senderDisplayName = Just "Bob"
        , senderIdentifier = Just "10001"
        , senderIsBot = False
        , text = ""
        , imageUrls = [referencedImage]
        , files = []
        }
      message = askHandlerMessage
        { replyToMessageId = Just "70001"
        , imageUrls = []
        , text = "krkr 把回复里的图调亮"
        }
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-enable" "tool_enable" (Aeson.object ["tags" Aeson..= ["work" :: Text]])]
    , chatAnswer "" [toolCall "call-1" "image_edit" (Aeson.object ["prompt" Aeson..= prompt])]
    , chatAnswer "done" []
    ]
  editCalls <- IORef.newIORef ([] :: [ImageEditCall])
  replies <- IORef.newIORef ([] :: [Text])
  _ <- runAgentWithImageEditAndReferencedMessage answers editCalls editedImage (Just referenced) (ChatMock (Just replies) (Just "47") Nothing) do
    threads <- newThreadStore
    runAskHandlersAndWait Agent.defaultToolConfig askHandlerConfig threads message
  IORef.readIORef editCalls >>= (@?= [ImageEditCall prompt [referencedImage] Nothing LLM.defaultImageRequestOptions])

testAskHandlerIncludesReferencedImageUrlsInTextContext :: IO ()
testAskHandlerIncludesReferencedImageUrlsInTextContext = do
  let referencedImage = "media:mf_replied"
      referenced = ReferencedMessage
        { messageId = Just "70001"
        , senderDisplayName = Just "Bob"
        , senderIdentifier = Just "10001"
        , senderIsBot = False
        , text = "original image"
        , imageUrls = [referencedImage]
        , files = []
        }
      message = askHandlerMessage
        { replyToMessageId = Just "70001"
        , imageUrls = []
        , text = "krkr 重发被回复的图"
        }
  answers <- IORef.newIORef [chatAnswer "done" []]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  rendered <- IORef.newIORef ([] :: [Text])
  _ <- runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEditAndReferenced
    (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused")
    defaultTestSkillsConfig
    rendered
    (Just captured)
    answers
    (ChatMock Nothing Nothing Nothing)
    (Just referenced)
    (\_ _ -> pure "unused image answer")
    (\_ _ _ _ -> pure "unused image edit answer") do
      threads <- newThreadStore
      runAskHandlersAndWait Agent.defaultToolConfig askHandlerConfig threads message
  requests <- IORef.readIORef captured
  case viaNonEmpty head requests of
    Just request -> do
      let userText = Text.unlines (chatMessageTextsByRole "user" request)
      assertBool "referenced image URL should appear in text context" ("被回复图片：media:mf_replied" `Text.isInfixOf` userText)
      requestUserImageUrls request @?= [referencedImage]
    Nothing ->
      assertFailure "expected captured LLM request"

testAskHandlerIncludesFilesInTextContext :: IO ()
testAskHandlerIncludesFilesInTextContext = do
  let referencedFile = MessageFile{name = "old.txt", ref = "media:mf_old"}
      currentFile = MessageFile{name = "new.txt", ref = "media:mf_new"}
      referenced = ReferencedMessage
        { messageId = Just "70001"
        , senderDisplayName = Just "Bob"
        , senderIdentifier = Just "10001"
        , senderIsBot = False
        , text = ""
        , imageUrls = []
        , files = [referencedFile]
        }
      message = askHandlerMessage
        { replyToMessageId = Just "70001"
        , files = [currentFile]
        , text = "krkr compare files"
        }
  answers <- IORef.newIORef [chatAnswer "done" []]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  rendered <- IORef.newIORef ([] :: [Text])
  _ <- runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEditAndReferenced
    (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused")
    defaultTestSkillsConfig
    rendered
    (Just captured)
    answers
    (ChatMock Nothing Nothing Nothing)
    (Just referenced)
    (\_ _ -> pure "unused image answer")
    (\_ _ _ _ -> pure "unused image edit answer") do
      threads <- newThreadStore
      runAskHandlersAndWait Agent.defaultToolConfig askHandlerConfig threads message
  requests <- IORef.readIORef captured
  case viaNonEmpty head requests of
    Just request -> do
      let userText = Text.unlines (chatMessageTextsByRole "user" request)
      assertBool ("referenced file should appear in text context: " <> Text.unpack userText) ("被回复文件：old.txt (media:mf_old)" `Text.isInfixOf` userText)
      assertBool ("current file should appear in text context: " <> Text.unpack userText) ("附件：new.txt (media:mf_new)" `Text.isInfixOf` userText)
    Nothing ->
      assertFailure "expected captured LLM request"

testAskHandlerHandlesReplyToNonBotMessageByChatKind :: IO ()
testAskHandlerHandlesReplyToNonBotMessageByChatKind = do
  answers <- IORef.newIORef [chatAnswer "mentioned" [], chatAnswer "private" []]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  rendered <- IORef.newIORef ([] :: [Text])
  let referenced = ReferencedMessage
        { messageId = Just "70001"
        , senderDisplayName = Just "Bob"
        , senderIdentifier = Just "10001"
        , senderIsBot = False
        , text = "not a bot message"
        , imageUrls = []
        , files = []
        }
      message = askHandlerMessage
        { replyToMessageId = Just "70001"
        , text = "follow up"
        }
      mentioned =
        message
          { messageId = Just "70002"
          , digest = message.digest{mentionsBot = True}
          , text = "@bot follow up"
          }
      privateReply =
        message
          { messageId = Just "70003"
          , kind = ChatPrivate
          , chatId = Nothing
          }
  _ <- runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEditAndReferenced
    (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused")
    defaultTestSkillsConfig
    rendered
    (Just captured)
    answers
    (ChatMock Nothing Nothing Nothing)
    (Just referenced)
    (\_ _ -> pure "unused image answer")
    (\_ _ _ _ -> pure "unused image edit answer") do
      threads <- newThreadStore
      before <- map (.id) . (.entries) <$> Concurrency.list
      runHandlers (askHandlers Agent.defaultToolConfig AgentTools.defaultTools askHandlerConfig threads) message
      afterSnapshot <- map (.id) . (.entries) <$> Concurrency.list
      liftIO $ afterSnapshot @?= before
      liftIO $ IORef.readIORef captured >>= assertBool "unmentioned reply should not call the LLM" . null
      runAskHandlersAndWait Agent.defaultToolConfig askHandlerConfig threads mentioned
      runAskHandlersAndWait Agent.defaultToolConfig askHandlerConfig threads privateReply
  IORef.readIORef captured >>= assertEqual "mentioned group and unmentioned private replies should call the LLM" 2 . length

testAskHandlerRoutesActiveReplyAsSteering :: IO ()
testAskHandlerRoutesActiveReplyAsSteering = do
  answers <- IORef.newIORef [chatAnswer "continued by another user" []]
  queued <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    threads <- newThreadStore
    active <- fromMaybe (error "expected active thread") <$>
      rememberActiveThread threads "test-run" Nothing (Just (messageKey 1)) testMessage "start" (Concurrency.Handle (Concurrency.Id 1)) (startWithUser "start")
    let steer :: IncomingMessage
        steer =
          testMessage
            { messageId = Just (integerMessageId 2)
            , replyToMessageId = Just (integerMessageId 1)
            , digest = testMessage.digest{senderIsAllowed = True}
            , text = "change direction"
            }
        otherSender =
          steer
            { messageId = Just (integerMessageId 3)
            , senderId = Just "201"
            , text = "hijack"
            }
        askCommand =
          testMessage
            { messageId = Just (integerMessageId 4)
            , replyToMessageId = Just (integerMessageId 1)
            , digest = testMessage.digest{senderIsAllowed = True}
            , text = "!ask start over"
            }
        imageSteer =
          testMessage
            { messageId = Just (integerMessageId 5)
            , replyToMessageId = Just (integerMessageId 1)
            , digest = testMessage.digest{senderIsAllowed = True}
            , imageUrls = ["media:image"]
            , text = ""
            }
        steerFile = MessageFile{name = "notes.txt", ref = "media:file"}
        fileSteer =
          testMessage
            { messageId = Just (integerMessageId 6)
            , replyToMessageId = Just (integerMessageId 1)
            , digest = testMessage.digest{senderIsAllowed = True}
            , files = [steerFile]
            , text = ""
            }
    runHandlers (askHandlers Agent.defaultToolConfig AgentTools.defaultTools askHandlerConfig threads) steer
    runHandlers (askHandlers Agent.defaultToolConfig AgentTools.defaultTools askHandlerConfig threads) imageSteer
    runHandlers (askHandlers Agent.defaultToolConfig AgentTools.defaultTools askHandlerConfig threads) fileSteer
    runAskHandlersAndWait Agent.defaultToolConfig askHandlerConfig threads otherSender
    runHandlers (askHandlers Agent.defaultToolConfig AgentTools.defaultTools askHandlerConfig threads) askCommand
    drainActiveThreadSteers active
  map (.text) queued @?= ["change direction", "请根据图片回答。", "附件：notes.txt (media:file)\n", "!ask start over"]
  map messageInputImageUrls queued @?= [[], ["media:image"], [], []]
  map messageInputFiles queued @?= [[], [], [MessageFile{name = "notes.txt", ref = "media:file"}], []]
  IORef.readIORef answers >>= assertBool "other sender should continue from the active snapshot" . null

testGroupReplyDoesNotContinueFinishedUserAlias :: IO ()
testGroupReplyDoesNotContinueFinishedUserAlias = do
  let parentId = "294869878"
      parentMessage =
        askHandlerMessage
          { digest = askHandlerMessage.digest{senderIsSuperuser = False}
          , text = "krkr hi"
          }
      parentTranscript =
        appendAssistant "hi" (startWithUser "krkr hi")
      referenced =
        ReferencedMessage
          { messageId = Just parentId
          , senderDisplayName = Just "Alice"
          , senderIdentifier = parentMessage.senderId
          , senderIsBot = False
          , text = "krkr hi"
          , imageUrls = []
          , files = []
          }
      followUp :: IncomingMessage
      followUp =
        askHandlerMessage
          { messageId = Just "70002"
          , senderId = Just "another-user"
          , digest = askHandlerMessage.digest{senderIsSuperuser = False}
          , replyToMessageId = Just parentId
          , text = "follow up"
          }
  answers <- IORef.newIORef []
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  rendered <- IORef.newIORef ([] :: [Text])
  _ <- runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEditAndReferenced
    (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused")
    defaultTestSkillsConfig
    rendered
    (Just captured)
    answers
    (ChatMock Nothing Nothing Nothing)
    (Just referenced)
    (\_ _ -> pure "unused image answer")
    (\_ _ _ _ -> pure "unused image edit answer") do
      threads <- newThreadStore
      active <- fromMaybe (error "expected active thread") <$>
        rememberActiveThread threads "test-run" Nothing (Just (threadMessageKey parentMessage parentId)) parentMessage "krkr hi" (Concurrency.Handle (Concurrency.Id 1)) parentTranscript
      finishActiveThread threads active parentTranscript
      runHandlers (askHandlers Agent.defaultToolConfig AgentTools.defaultTools askHandlerConfig threads) followUp
  IORef.readIORef captured >>= assertBool "replying to a finished user alias should not call the LLM" . null

testAskHandlerContinuesFinishedBotReply :: IO ()
testAskHandlerContinuesFinishedBotReply = do
  let botReplyId = "900"
      followUp =
        askHandlerMessage
          { kind = ChatPrivate
          , messageId = Just "70002"
          , senderId = Just "another-user"
          , replyToMessageId = Just botReplyId
          , text = "再见"
          }
  answers <- IORef.newIORef [chatAnswer "你好" [], chatAnswer "再见" []]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  replies <- IORef.newIORef ([] :: [Text])
  rendered <- IORef.newIORef ([] :: [Text])
  _ <- runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEditAndReferenced
    (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused")
    defaultTestSkillsConfig
    rendered
    (Just captured)
    answers
    (ChatMock (Just replies) (Just botReplyId) Nothing)
    Nothing
    (\_ _ -> pure "unused image answer")
    (\_ _ _ _ -> pure "unused image edit answer") do
      threads <- newThreadStore
      runAskHandlersAndWait Agent.defaultToolConfig askHandlerConfig threads askHandlerMessage
      linked <- lookupThreadTranscript threads (threadMessageKey askHandlerMessage botReplyId)
      liftIO $ assertBool "first bot reply should be a finished thread alias" (isJust linked)
      liftIO $ assertBool "persisted transcript should not contain a system prompt" $
        all ((/= "system") . (.role)) (maybe [] (Foldable.toList . (.messages)) linked)
      rememberThreadTranscript threads (Just (threadMessageKey askHandlerMessage botReplyId)) $
        appendAssistant "你好" (startWithSystemAndUser "legacy memory for sender 295947730" "krkr 看下我的头像")
      runAskHandlersAndWait Agent.defaultToolConfig askHandlerConfig threads followUp
  IORef.readIORef replies >>= (@?= ["😻 FM：你好", "😻 FM：再见"])
  requests <- IORef.readIORef captured
  case requests of
    [_first, continued] -> do
      chatMessageTextsByRole "assistant" continued @?= ["你好"]
      assertBool "continued request contains the new sender message"
        (any (Text.isInfixOf "再见") (chatMessageTextsByRole "user" continued))
      case chatMessageTextsByRole "system" continued of
        [systemPrompt] -> do
          assertBool "continued request uses the current sender" ("- sender_id: another-user" `Text.isInfixOf` systemPrompt)
          assertBool "continued request drops the original sender context" (not ("- sender_id: 295947730" `Text.isInfixOf` systemPrompt))
          assertBool "continued request drops a legacy persisted system prompt" (not ("legacy memory" `Text.isInfixOf` systemPrompt))
        other ->
          assertFailure [i|expected one rebuilt system prompt, got #{show other :: String}|]
    other ->
      assertFailure [i|expected two bot-reply model requests, got #{length other}|]

testLLMFailureReplyLinksThread :: IO ()
testLLMFailureReplyLinksThread = do
  answers <- IORef.newIORef [error "simulated LLM failure"]
  replies <- IORef.newIORef []
  linked <- runAgentWith answers (ChatMock (Just replies) (Just "900") Nothing) do
    threads <- newThreadStore
    runAskHandlersAndWait Agent.defaultToolConfig askHandlerConfig threads askHandlerMessage
    lookupThreadTranscript threads (threadMessageKey askHandlerMessage "900")
  assertBool "failure reply should resolve to the original thread" (isJust linked)
  sent <- IORef.readIORef replies
  assertBool "expected a user-facing LLM failure reply" (any (Text.isInfixOf "这次处理没有成功完成") sent)

testGenerateImageToolPassesImageRequestOptions :: IO ()
testGenerateImageToolPassesImageRequestOptions = do
  let generatedMediaRef = "media:mf_generated"
      generatedImage = "[image] " <> generatedMediaRef
      expectedOptions = imageOptions "high" "1024x1536" "transparent" "low"
      args =
        Aeson.object
          [ "prompt" Aeson..= ("draw a glass tower" :: Text)
          , "quality" Aeson..= (" high " :: Text)
          , "size" Aeson..= ("1024x1536" :: Text)
          , "background" Aeson..= ("transparent" :: Text)
          , "moderation" Aeson..= ("low" :: Text)
          ]
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "image_generate" args]
    , chatAnswer "done" []
    ]
  generateCalls <- IORef.newIORef ([] :: [ImageGenerateCall])
  replies <- IORef.newIORef ([] :: [Text])
  recorded <- IORef.newIORef ([] :: [Text])
  remembered <- IORef.newIORef ([] :: [Maybe MessageId])
  (answer, transcript) <- runAgentWithImageGenerate answers generateCalls generatedImage (ChatMock (Just replies) (Just "48") Nothing) do
    runAgentWithToolMessageCapture 4 (agentContext{Agent.input = inputWithImages "draw this" []}) AgentTools.defaultTools (startWithEnabledTools ["work"] "draw this") recorded remembered
  answer @?= "done"
  IORef.readIORef generateCalls >>= (@?= [ImageGenerateCall "draw a glass tower" [] expectedOptions])
  IORef.readIORef replies >>= assertElem generatedImage
  IORef.readIORef recorded >>= assertElem generatedImage
  assertBool "tool result should include generated media id" (generatedMediaRef `Text.isInfixOf` Text.unlines (toolOutputs transcript))
  imageContextUrls transcript @?= [generatedMediaRef]

testViewImageToolCachesImageForContext :: IO ()
testViewImageToolCachesImageForContext =
  withSQLiteTempPath "view-image" \dbPath ->
    withTempDir "view-image-media" \dir -> do
      let cacheDir = dir </> "cache"
          cfg = MediaConfig.defaultConfig{MediaConfig.cacheDir = cacheDir}
          imageUrl = "data:image/png;base64,iVBORw0KGgpmYWtl" :: Text
          runStack =
            runFileSystem
              . runProcess
              . runFail
              . runConcurrent
              . runTestLog
              . StorageSQLite.runStorageSQLitePath dbPath
              . HTTP.runHTTP
              . runTimeout
              . MediaInterpreter.runMedia cfg
      runResult <- runEff $ runStack do
        runner <- AgentTool.startTool ImageTools.viewImageTool agentContext
        runner testToolCallMetadata (Aeson.object ["url" Aeson..= imageUrl])
      toolResult <- either assertFailure pure runResult
      case toolResult of
        Agent.ToolSucceeded{content, imageUrls} -> do
          assertBool "tool result should mention media ref" ("media:" `Text.isInfixOf` content)
          case imageUrls of
            [mediaRef] ->
              assertBool "expected cached media ref" ("media:" `Text.isPrefixOf` mediaRef)
            other ->
              assertFailure [i|expected one image context ref, got #{show other :: String}|]
        Agent.ToolFailed{failure} ->
          assertFailure [i|image_cache failed: #{show failure :: String}|]

testModelImageRefEncodesCachedImage :: IO ()
testModelImageRefEncodesCachedImage =
  withSQLiteTempPath "model-image-ref" \dbPath ->
    withTempDir "model-image-ref-cache" \dir -> do
      let cfg = MediaConfig.defaultConfig{MediaConfig.cacheDir = dir </> "cache"}
          runStack =
            runFileSystem
              . runProcess
              . runFail
              . runConcurrent
              . runTestLog
              . StorageSQLite.runStorageSQLitePath dbPath
              . HTTP.runHTTP
              . runTimeout
              . MediaInterpreter.runMedia cfg
      runResult <- runEff $ runStack do
        mediaRef <- fromMaybe (error "expected media ref") <$> Media.storeMediaObject Media.MediaObject
          { bytes = Q.fromStrict "png bytes"
          , mimeType = "image/png"
          , sourceName = Just "sample.png"
          }
        Media.modelImageRef mediaRef
      resolved <- either assertFailure pure runResult
      resolved @?= "data:image/png;base64,cG5nIGJ5dGVz"

testViewImageToolRejectsLocalFileUrls :: IO ()
testViewImageToolRejectsLocalFileUrls =
  withSQLiteTempPath "view-image-local-file" \dbPath ->
    withTempDir "view-image-local-file-media" \dir -> do
      let cacheDir = dir </> "cache"
          secretPath = dir </> "secret.png"
          fileRef = "file://" <> Text.pack secretPath
          cfg = MediaConfig.defaultConfig{MediaConfig.cacheDir = cacheDir}
          runStack =
            runFileSystem
              . runProcess
              . runFail
              . runConcurrent
              . runTestLog
              . StorageSQLite.runStorageSQLitePath dbPath
              . HTTP.runHTTP
              . runTimeout
              . MediaInterpreter.runMedia cfg
      runResult <- runEff $ runStack do
        void Media.mediaCacheStats
        FSByteString.writeFile secretPath "secret"
        runner <- AgentTool.startTool ImageTools.viewImageTool agentContext
        result <- runner testToolCallMetadata (Aeson.object ["url" Aeson..= fileRef])
        cached <- Media.mediaRefForSource fileRef
        pure (result, cached)
      (result, cached) <- either assertFailure pure runResult
      cached @?= Nothing
      case result of
        Agent.ToolFailed{} -> pure ()
        Agent.ToolSucceeded{} -> assertFailure "image_view accepted a local file URL"

testReadMediaTextToolReadsCachedSlices :: IO ()
testReadMediaTextToolReadsCachedSlices =
  withSQLiteTempPath "read-media-text" \dbPath ->
    withTempDir "read-media-text-cache" \dir -> do
      let cfg = MediaConfig.defaultConfig{MediaConfig.cacheDir = dir </> "cache"}
          content = "abcdefg" :: Text
          runStack =
            runFileSystem
              . runProcess
              . runFail
              . runConcurrent
              . runTestLog
              . StorageSQLite.runStorageSQLitePath dbPath
              . HTTP.runHTTP
              . runTimeout
              . MediaInterpreter.runMedia cfg
      runResult <- runEff $ runStack do
        mediaRef <- Media.storeMediaObject Media.MediaObject
          { bytes = Q.fromStrict (TextEncoding.encodeUtf8 content)
          , mimeType = "text/plain; charset=utf-8"
          , sourceName = Just "sample.txt"
          }
        let mediaId = maybe "" (\ref -> fromMaybe ref (Text.stripPrefix "media:" ref)) mediaRef
        runner <- AgentTool.startTool MediaTools.readMediaTextTool agentContext
        result <- runner testToolCallMetadata (Aeson.object
          [ "media_id" Aeson..= mediaId
          , "offset" Aeson..= (2 :: Int)
          , "size" Aeson..= (3 :: Int)
          ])
        pure (mediaRef, result)
      (mediaRef, result) <- either assertFailure pure runResult
      let tool = MediaTools.readMediaTextTool :: AgentTool.Tool (Eff AgentStack)
      assertBool "expected stored media ref" (maybe False ("media:mf_" `Text.isPrefixOf`) mediaRef)
      AgentTool.toolIsNoisy tool @?= False
      assertBool "media_text should be available to everyone" (AgentTool.toolAllowed tool agentContext)
      case result of
        Agent.ToolSucceeded{content = output} -> do
          assertBool "tool output should include requested slice" ("\"content\":\"cde\"" `Text.isInfixOf` output)
          assertBool "tool output should include returned count" ("\"returned_chars\":3" `Text.isInfixOf` output)
          assertBool "tool output should include total chars" ("\"total_chars\":7" `Text.isInfixOf` output)
        Agent.ToolFailed{failure} ->
          assertFailure [i|media_text failed: #{show failure :: String}|]

testMediaToFileReturnsCachePath :: IO ()
testMediaToFileReturnsCachePath =
  withSQLiteTempPath "media-to-file" \dbPath ->
    withTempDir "media-to-file-cache" \dir -> do
      let cfg = MediaConfig.defaultConfig{MediaConfig.cacheDir = dir </> "cache"}
          runStack =
            runFileSystem
              . runProcess
              . runFail
              . runConcurrent
              . runTestLog
              . StorageSQLite.runStorageSQLitePath dbPath
              . HTTP.runHTTP
              . runTimeout
              . MediaInterpreter.runMedia cfg
      runResult <- runEff $ runStack do
        mediaRef <- fromMaybe (error "expected media ref") <$> Media.storeMediaObject Media.MediaObject
          { bytes = Q.fromStrict "file bytes"
          , mimeType = "application/octet-stream"
          , sourceName = Just "sample.bin"
          }
        expectedPath <- fromMaybe (error "expected local media path") <$> Media.localMediaPath mediaRef
        runner <- AgentTool.startTool MediaTools.mediaToFileTool agentContext
        result <- runner testToolCallMetadata (Aeson.object ["media_id" Aeson..= mediaRef])
        pure (expectedPath, result)
      (expectedPath, result) <- either assertFailure pure runResult
      case result of
        Agent.ToolSucceeded{content, imageUrls} -> do
          assertBool "tool output should contain the cache path" (Text.pack expectedPath `Text.isInfixOf` content)
          imageUrls @?= []
        Agent.ToolFailed{failure} ->
          assertFailure [i|media_to_file failed: #{show failure :: String}|]

testGenerateAudioToolUsesConfiguredAudioOptions :: IO ()
testGenerateAudioToolUsesConfiguredAudioOptions = do
  let generatedAudio = "data:audio/mp3;base64,generated"
      expectedOptions = LLM.defaultAudioRequestOptions
      args =
        Aeson.object
          [ "prompt" Aeson..= ("say hello" :: Text)
          , "voice" Aeson..= (" verse " :: Text)
          , "format" Aeson..= ("mp3" :: Text)
          , "speed" Aeson..= (1.25 :: Double)
          , "instructions" Aeson..= (" speak warmly " :: Text)
          ]
  generateCalls <- IORef.newIORef ([] :: [AudioGenerateCall])
  audioReplies <- IORef.newIORef ([] :: [(Text, Maybe Text)])
  result <- runEff $
    LLMTest.runLLMWith
      (\_ -> S.yield "unused text stream answer" $> "unused text stream answer")
      (\_ _ -> S.yield "unused image answer" $> "unused image answer")
      (\_ _ _ _ -> S.yield "unused image edit answer" $> "unused image edit answer")
      (\options messages -> do
          liftIO $ IORef.modifyIORef' generateCalls (<> map (audioGenerateCall options) messages)
          S.yield generatedAudio
          pure generatedAudio)
      (\_ _ -> S.each ["to", "ol"] $> chatAnswer "tool" []) $
      Chat.runChatWith defaultAgentMockChatDriver { agentReplyAudio = \_ audioRef caption -> do
          liftIO $ IORef.modifyIORef' audioReplies (<> [(audioRef, caption)])
          pure (Right "50")
        } do
          runner <- AgentTool.startTool AudioTools.generateAudioTool agentContext
          runner testToolCallMetadata args
  case result of
    Agent.ToolSucceeded{content} ->
      assertBool "tool result should describe sent audio" ("Generated and sent audio message id" `Text.isInfixOf` content)
    Agent.ToolFailed{failure} ->
      assertFailure [i|expected audio generation success, got #{show failure :: String}|]
  IORef.readIORef generateCalls >>= (@?= [AudioGenerateCall "say hello" expectedOptions])
  IORef.readIORef audioReplies >>= (@?= [(generatedAudio, Nothing)])
  let tool = AudioTools.generateAudioTool :: AgentTool.Tool (Eff '[Chat.Chat, LLM.LLM])
  AgentTool.toolIsNoisy tool @?= True

testEditImageToolPassesImageRequestOptions :: IO ()
testEditImageToolPassesImageRequestOptions = do
  let inputImage = "https://example.test/input.png"
      editedMediaRef = "media:mf_cinematic"
      editedImage = "[image] " <> editedMediaRef
      expectedOptions = imageOptions "medium" "1536x1024" "opaque" "auto"
      message = testMessageWithImages [inputImage]
      args =
        Aeson.object
          [ "prompt" Aeson..= ("make it cinematic" :: Text)
          , "quality" Aeson..= ("medium" :: Text)
          , "size" Aeson..= ("1536x1024" :: Text)
          , "background" Aeson..= (" opaque " :: Text)
          , "moderation" Aeson..= ("auto" :: Text)
          ]
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "image_edit" args]
    , chatAnswer "done" []
    ]
  editCalls <- IORef.newIORef ([] :: [ImageEditCall])
  replies <- IORef.newIORef ([] :: [Text])
  recorded <- IORef.newIORef ([] :: [Text])
  remembered <- IORef.newIORef ([] :: [Maybe MessageId])
  (answer, transcript) <- runAgentWithImageEdit answers editCalls editedImage (ChatMock (Just replies) (Just "49") Nothing) do
    runAgentWithToolMessageCapture 4 (agentContext{Agent.message = message, Agent.input = inputWithImages message.text message.imageUrls}) AgentTools.defaultTools (startWithEnabledTools ["work"] "edit this") recorded remembered
  answer @?= "done"
  IORef.readIORef editCalls >>= (@?= [ImageEditCall "make it cinematic" [inputImage] Nothing expectedOptions])
  IORef.readIORef replies >>= assertElem editedImage
  IORef.readIORef recorded >>= assertElem editedImage
  assertBool "tool result should include edited media id" (editedMediaRef `Text.isInfixOf` Text.unlines (toolOutputs transcript))
  imageContextUrls transcript @?= [editedMediaRef]

imageOptions :: Text -> Text -> Text -> Text -> LLM.ImageRequestOptions
imageOptions quality size background moderation =
  LLM.ImageRequestOptions
    { quality = Just quality
    , size = Just size
    , background = Just background
    , moderation = Just moderation
    }

testAgentRequestMergesCurrentMessageContextIntoSystemPrompt :: IO ()
testAgentRequestMergesCurrentMessageContextIntoSystemPrompt = do
  answers <- IORef.newIORef [chatAnswer "done" []]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  _ <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 4
      (agentContext
        { Agent.systemContext = Text.unlines
            [ "Current message context:"
            , "- platform: PlatformQQ"
            , "- bot_id: 2044933066"
            , "- sender_id: 295947730"
            ]
        })
      AgentTools.defaultTools
      (startWithSystemAndUser "base system prompt" "hello")
  requests <- IORef.readIORef captured
  case viaNonEmpty head requests of
    Just (message : secondMessage : _) -> do
      message.role @?= "system"
      assertBool "second request message is not system" (secondMessage.role /= "system")
      case message.content of
        Just (LLM.TextContent content) -> do
          assertBool "system context preserves configured prompt" ("base system prompt" `Text.isInfixOf` content)
          assertBool "system context contains bot id" ("- bot_id: 2044933066" `Text.isInfixOf` content)
          assertBool "system context contains sender id" ("- sender_id: 295947730" `Text.isInfixOf` content)
        other ->
          assertFailure [i|expected text system content, got #{show other :: String}|]
    other ->
      assertFailure [i|expected at least two captured LLM request messages, got #{show (requestRoles <$> other) :: String}|]

testAgentCompactsOldTranscriptContextBeforeModelTurn :: IO ()
testAgentCompactsOldTranscriptContextBeforeModelTurn = do
  answers <- IORef.newIORef
    [ chatAnswerWithUsage highTokenUsage "" [toolCall "call-1" "message_info" (Aeson.object [])]
    , chatAnswerWithUsage (LLM.TokenUsage 500 50 550 (Just 100)) "summary" []
    , chatAnswer "done" []
    ]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  let Transcript enabledPrefix = startWithEnabledTools ["chat"] "message 1"
      longTranscript =
        Transcript (enabledPrefix <> Seq.fromList [LLM.userText [i|message #{index}|] | index <- [2 .. 51 :: Int]])
  records <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    agentRun <- startTestRuntime 4 agentContext AgentTools.defaultTools
    let program = Agent.defaultRuntime AgentAudit.agentAuditObserver 1000 agentRun
    _ <- S.mapM_ (\_ -> pure ()) (Agent.agentStream program longTranscript)
    AgentAudit.queryRunAudit (Agent.runIdOf agentRun)
  requests <- IORef.readIORef captured
  case requests of
    [_firstModelRequest, _summaryRequest, compactedRequest] ->
      case compactedRequest of
        summaryMessage : remainingMessages -> do
          summaryMessage.role @?= "system"
          case summaryMessage.content of
            Just (LLM.TextContent content) ->
              assertBool "compacted summary is included" ("The earlier transcript was compacted." `Text.isInfixOf` content)
            other ->
              assertFailure [i|expected compacted summary text, got #{show other :: String}|]
          length remainingMessages @?= 22
          assertBool "retained messages include the recent tool exchange" ("tool" `elem` map (.role) remainingMessages)
          AgentTool.toolEnableTagRequests (Transcript (Seq.fromList compactedRequest)) @?= [["chat"]]
        other ->
          assertFailure [i|expected compacted request messages, got #{show other :: String}|]
    other ->
      assertFailure [i|expected first request, summary request, and compacted request, got #{length other}|]
  assertBool "context compaction records its own token usage" $
    any
      (\case
        AgentAudit.ContextCompacted{tokenUsage = Just usage} ->
          usage == LLM.TokenUsage 500 50 550 (Just 100)
        _ -> False
      )
      (map (.event) records)

testAgentAnnouncesContextCompaction :: IO ()
testAgentAnnouncesContextCompaction = do
  answers <- IORef.newIORef
    [ chatAnswerWithUsage highTokenUsage "" [toolCall "call-1" "message_info" (Aeson.object [])]
    , chatAnswerWithUsage (LLM.TokenUsage 500 50 550 (Just 100)) "summary" []
    , chatAnswer "done" []
    ]
  replies <- IORef.newIORef ([] :: [Text])
  let longTranscript =
        Transcript (Seq.fromList [LLM.userText [i|message #{index}|] | index <- [1 .. 51 :: Int]])
  _ <- runAgentWith answers (ChatMock (Just replies) (Just "46") Nothing) do
    agentRun <- startTestRuntime 4 agentContext AgentTools.defaultTools
    let program = Agent.defaultRuntime AgentAudit.agentAuditObserver 1000 agentRun
    _ <- S.mapM_ (\_ -> pure ()) (Agent.agentStream program longTranscript)
    pure ()
  sent <- IORef.readIORef replies
  sent @?= ["正在整理较早的对话上下文..."]

testAgentResumesNestedContinuations :: IO ()
testAgentResumesNestedContinuations = do
  let innerValue =
        Aeson.object
          [ "conclusion" Aeson..= ("inner complete" :: Text)
          , "evidence" Aeson..= ([1, 2] :: [Int])
          ]
      outerValue =
        Aeson.object
          [ "selected" Aeson..= ("outer" :: Text)
          , "nested" Aeson..= innerValue
          ]
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "outer" "capture_continuation" (Aeson.object ["label" Aeson..= ("outer branch" :: Text)])]
    , chatAnswer "" [toolCall "inner" "capture_continuation" (Aeson.object ["label" Aeson..= ("inner branch" :: Text)])]
    , chatAnswer "" [toolCall "branch-tool" "message_info" (Aeson.object [])]
    , chatAnswer "" [toolCall "resume-inner" "resume_continuation" (Aeson.object ["continuation_id" Aeson..= ("inner" :: Text), "value" Aeson..= innerValue])]
    , chatAnswer "" [toolCall "resume-outer" "resume_continuation" (Aeson.object ["continuation_id" Aeson..= ("outer" :: Text), "value" Aeson..= outerValue])]
    , chatAnswer "done" []
    ]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  ((answer, transcript), toolUses) <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    agentRun <- startTestRuntime 6 agentContext AgentTools.defaultTools
    let program = Agent.defaultRuntime AgentAudit.agentAuditObserver 1000000 agentRun
    outputs S.:> result <- S.toList (Agent.agentStream program (startWithUser "explore"))
    uses <- AgentAudit.queryRecentToolUses 10
    pure ((agentOutputText outputs, result.transcript), uses)
  answer @?= "done"
  map (.turn) toolUses @?= [1 .. 5]
  requests <- IORef.readIORef captured
  case requests of
    [_initial, _afterOuterCapture, _afterInnerCapture, _afterBranchTool, afterInnerResume, afterOuterResume] -> do
      let afterInnerJson = jsonText afterInnerResume
          afterOuterJson = jsonText afterOuterResume
      assertBool "inner resume discards its explored tool turn" (not ("branch-tool" `Text.isInfixOf` afterInnerJson))
      assertBool "inner resume returns the JSON value" (innerValue `elem` resumedContinuationValues afterInnerResume)
      assertBool "outer resume discards the inner continuation" (not ("\"inner\"" `Text.isInfixOf` afterOuterJson))
      assertBool "outer resume returns the nested JSON value" (outerValue `elem` resumedContinuationValues afterOuterResume)
    other ->
      assertFailure [i|expected six continuation model requests, got #{length other}|]
  let finalJson = jsonText transcript
  assertBool "durable transcript omits abandoned inner branch" (not ("branch-tool" `Text.isInfixOf` finalJson))
  assertBool
    "durable transcript keeps the outer resumed value"
    (outerValue `elem` resumedContinuationValues (Foldable.toList transcript.messages))

testAgentRejectsConcurrentContinuationCalls :: IO ()
testAgentRejectsConcurrentContinuationCalls = do
  calls <- IORef.newIORef (0 :: Int)
  answers <- IORef.newIORef
    [ chatAnswer ""
        [ toolCall "capture" "capture_continuation" (Aeson.object [])
        , toolCall "sibling" "side_effect" (Aeson.object [])
        ]
    , chatAnswer "done" []
    ]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  let sideEffectTool =
        AgentTool.withDescription "record one execution"
        $ AgentTool.tool "side_effect" AgentTool.noArguments do
            liftIO $ IORef.modifyIORef' calls (+ 1)
            pure (Agent.toolText "executed")
      tools =
        [ ContinuationTools.captureContinuationTool
        , ContinuationTools.resumeContinuationTool
        , sideEffectTool
        ]
  _ <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 2 agentContext tools (startWithUser "capture and execute")
  IORef.readIORef calls >>= (@?= 0)
  requests <- IORef.readIORef captured
  case requests of
    [_initial, rejected] -> do
      let failures = chatMessageTextsByRole "tool" rejected
      length failures @?= 2
      assertBool "every sibling call is rejected" (all ("must be called alone" `Text.isInfixOf`) failures)
    other ->
      assertFailure [i|expected initial and rejected requests, got #{length other}|]

testAgentDoesNotInterceptUnexposedContinuationTools :: IO ()
testAgentDoesNotInterceptUnexposedContinuationTools = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "capture" "capture_continuation" (Aeson.object [])]
    , chatAnswer "done" []
    ]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  _ <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 2 agentContext [] (startWithUser "capture")
  requests <- IORef.readIORef captured
  case requests of
    [_initial, rejected] ->
      assertBool
        "normal registry rejects the unexposed control tool"
        (any ("Unknown tool: capture_continuation" `Text.isInfixOf`) (chatMessageTextsByRole "tool" rejected))
    other ->
      assertFailure [i|expected initial and rejected requests, got #{length other}|]

testAgentResumesContinuationAtToolLimit :: IO ()
testAgentResumesContinuationAtToolLimit = do
  let resumedValue = Aeson.object ["finished" Aeson..= True]
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "capture" "capture_continuation" (Aeson.object [])]
    , chatAnswer "" [toolCall "explore" "message_info" (Aeson.object [])]
    , chatAnswer "" [toolCall "resume" "resume_continuation" (Aeson.object ["continuation_id" Aeson..= ("capture" :: Text), "value" Aeson..= resumedValue])]
    , chatAnswer "done after resume" []
    ]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  (answer, _) <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 3 agentContext AgentTools.defaultTools (startWithUser "explore within budget")
  answer @?= "done after resume"
  requests <- IORef.readIORef captured
  case requests of
    [_initial, _afterCapture, _afterExplore, afterResume] ->
      assertBool "resume at the limit returns its JSON value" (resumedValue `elem` resumedContinuationValues afterResume)
    other ->
      assertFailure [i|expected resume to reach a fourth model request, got #{length other}|]

testAgentSteeringContinuesAfterFinalAnswer :: IO ()
testAgentSteeringContinuesAfterFinalAnswer = do
  answers <- IORef.newIORef [chatAnswer "first answer" [], chatAnswer "steered answer" []]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  drains <- IORef.newIORef [[], []]
  completions <- IORef.newIORef [Just [inputWithImages "change direction" []], Nothing]
  (outputs, transcript) <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    agentRun <- startTestRuntime 4 agentContext AgentTools.defaultTools
    let steering =
          Agent.SteeringControl
            { Agent.drain = liftIO (popSteering [] drains)
            , Agent.complete = liftIO (popSteering Nothing completions)
            }
        program =
          Agent.withSteering steering $
            Agent.defaultRuntime AgentAudit.agentAuditObserver 1000000 agentRun
    streamOutputs S.:> result <- S.toList (Agent.agentStream program (startWithUser "start"))
    pure (streamOutputs, result.transcript)
  length [() | Agent.ReplyBoundary <- outputs] @?= 1
  requests <- IORef.readIORef captured
  case requests of
    [_initial, steered] -> do
      chatMessageTextsByRole "assistant" steered @?= ["first answer"]
      chatMessageTextsByRole "user" steered @?= ["start", "change direction"]
    other ->
      assertFailure [i|expected two steering model requests, got #{length other}|]
  chatMessageTextsByRole "assistant" (transcriptMessagesList transcript) @?= ["first answer", "steered answer"]

testAgentSteeringPreservesModelMiddleware :: IO ()
testAgentSteeringPreservesModelMiddleware = do
  answers <- IORef.newIORef [chatAnswer "first answer" [], chatAnswer "steered answer" []]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  drains <- IORef.newIORef [[], []]
  completions <- IORef.newIORef [Just [inputWithImages "change direction" []], Nothing]
  outsideTurns <- IORef.newIORef (0 :: Int)
  insideTurns <- IORef.newIORef (0 :: Int)
  _ <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    agentRun <- startTestRuntime 4 agentContext AgentTools.defaultTools
    let steering =
          Agent.SteeringControl
            { Agent.drain = liftIO (popSteering [] drains)
            , Agent.complete = liftIO (popSteering Nothing completions)
            }
        countModelTurns counter runtime =
          runtime
            { AgentCore.aroundModelTurn = \context agentState action -> do
                liftIO $ IORef.modifyIORef' counter (+ 1)
                runtime.aroundModelTurn context agentState action
            }
        program =
          countModelTurns outsideTurns
            . Agent.withSteering steering
            . countModelTurns insideTurns
            $ Agent.defaultRuntime AgentAudit.agentAuditObserver 1000000 agentRun
    void $ S.toList (Agent.agentStream program (startWithUser "start"))
  IORef.readIORef outsideTurns >>= (@?= 2)
  IORef.readIORef insideTurns >>= (@?= 2)

testAgentSteeringWaitsForToolResults :: IO ()
testAgentSteeringWaitsForToolResults = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "info" "message_info" (Aeson.object [])]
    , chatAnswer "done" []
    ]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  drains <- IORef.newIORef [[], [inputWithImages "after the tool" []]]
  completions <- IORef.newIORef [Nothing]
  outputs <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    agentRun <- startTestRuntime 4 agentContext AgentTools.defaultTools
    let steering =
          Agent.SteeringControl
            { Agent.drain = liftIO (popSteering [] drains)
            , Agent.complete = liftIO (popSteering Nothing completions)
            }
        program =
          Agent.withSteering steering $
            Agent.defaultRuntime AgentAudit.agentAuditObserver 1000000 agentRun
    streamOutputs S.:> _ <- S.toList (Agent.agentStream program (startWithUser "start"))
    pure streamOutputs
  length [() | Agent.ReplyBoundary <- outputs] @?= 0
  requests <- IORef.readIORef captured
  case requests of
    [_initial, afterTool] -> do
      map (.role) (drop 1 afterTool) @?= ["assistant", "tool", "user"]
      chatMessageTextsByRole "user" afterTool @?= ["start", "after the tool"]
    other ->
      assertFailure [i|expected two steering model requests, got #{length other}|]

testAgentSteeringClearsContinuations :: IO ()
testAgentSteeringClearsContinuations = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "capture" "capture_continuation" (Aeson.object [])]
    , chatAnswer "branch answer" []
    , chatAnswer "" [toolCall "resume" "resume_continuation" (Aeson.object ["continuation_id" Aeson..= ("capture" :: Text), "value" Aeson..= Aeson.Null])]
    , chatAnswer "done" []
    ]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  drains <- IORef.newIORef [[], [], [], []]
  completions <- IORef.newIORef [Just [inputWithImages "keep this instruction" []], Nothing]
  _ <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    agentRun <- startTestRuntime 5 agentContext AgentTools.defaultTools
    let steering =
          Agent.SteeringControl
            { Agent.drain = liftIO (popSteering [] drains)
            , Agent.complete = liftIO (popSteering Nothing completions)
            }
        program =
          Agent.withSteering steering $
            Agent.defaultRuntime AgentAudit.agentAuditObserver 1000000 agentRun
    void $ S.toList (Agent.agentStream program (startWithUser "start"))
  requests <- IORef.readIORef captured
  case requests of
    [_initial, _captured, _steered, rejectedResume] ->
      assertBool
        "steering invalidates continuations captured before the user message"
        (any ("Continuation not found" `Text.isInfixOf`) (chatMessageTextsByRole "tool" rejectedResume))
    other ->
      assertFailure [i|expected four steering model requests, got #{length other}|]

testAskHandlerSystemContextIncludesConfiguredBotAndSenderIds :: IO ()
testAskHandlerSystemContextIncludesConfiguredBotAndSenderIds = do
  answers <- IORef.newIORef [chatAnswer "done" []]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  _ <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    threads <- newThreadStore
    runAskHandlersAndWait Agent.defaultToolConfig askHandlerConfig threads askHandlerMessage
  requests <- IORef.readIORef captured
  case viaNonEmpty head requests of
    Just (message : secondMessage : _) -> do
      message.role @?= "system"
      assertBool "second request message is not system" (secondMessage.role /= "system")
      case message.content of
        Just (LLM.TextContent content) -> do
          assertBool "ask handler system context preserves configured prompt" ("base system prompt" `Text.isInfixOf` content)
          assertBool "ask handler system context contains configured bot id" ("- bot_id: 2044933066 (cosmobot's own platform user id)" `Text.isInfixOf` content)
          assertBool "ask handler system context contains sender id" ("- sender_id: 295947730 (the platform user id of the user who sent this message)" `Text.isInfixOf` content)
        other ->
          assertFailure [i|expected text system content, got #{show other :: String}|]
    other ->
      assertFailure [i|expected at least two captured ask-handler LLM request messages, got #{show (requestRoles <$> other) :: String}|]

testAskHandlerSystemContextUsesMessageBotId :: IO ()
testAskHandlerSystemContextUsesMessageBotId = do
  answers <- IORef.newIORef [chatAnswer "done" []]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  _ <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    threads <- newThreadStore
    let cfg = askHandlerConfig{botIds = []}
        message = askHandlerMessage{digest = askHandlerMessage.digest{botId = Just "2044933066"}}
    runAskHandlersAndWait Agent.defaultToolConfig cfg threads message
  requests <- IORef.readIORef captured
  case viaNonEmpty head requests of
    Just (message : secondMessage : _) -> do
      message.role @?= "system"
      assertBool "second request message is not system" (secondMessage.role /= "system")
      case message.content of
        Just (LLM.TextContent content) -> do
          assertBool "ask handler system context contains message bot id" ("- bot_id: 2044933066 (cosmobot's own platform user id)" `Text.isInfixOf` content)
          assertBool "ask handler system context contains sender id" ("- sender_id: 295947730 (the platform user id of the user who sent this message)" `Text.isInfixOf` content)
        other ->
          assertFailure [i|expected text system content, got #{show other :: String}|]
    other ->
      assertFailure [i|expected at least two captured ask-handler LLM request messages, got #{show (requestRoles <$> other) :: String}|]

testAskHandlerInjectsStartupSkillMetadata :: IO ()
testAskHandlerInjectsStartupSkillMetadata = withTempDir "skills-test" \skillsDir -> do
  createDirectoryIfMissing True (skillsDir </> "haskell")
  TextIO.writeFile (skillsDir </> "haskell" </> "SKILL.md") $
    Text.unlines
      [ "---"
      , "name: haskell-refactor"
      , "description: Improve Haskell modules safely."
      , "---"
      , "Full skill body is loaded only when needed."
      ]
  answers <- IORef.newIORef [chatAnswer "done" []]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  _ <- runAgentCapturingMessagesWithSkills (SkillsStore.SkillsConfig skillsDir) captured answers (ChatMock Nothing Nothing Nothing) do
    threads <- newThreadStore
    runAskHandlersAndWait Agent.defaultToolConfig askHandlerConfig threads askHandlerMessage
  requests <- IORef.readIORef captured
  case viaNonEmpty head requests of
    Just (message : _) ->
      case message.content of
        Just (LLM.TextContent content) -> do
          assertBool "skill metadata block is included" ("<SKILLS>" `Text.isInfixOf` content)
          assertBool "skill name is included" ("haskell-refactor" `Text.isInfixOf` content)
          assertBool "skill description is included" ("Improve Haskell modules safely." `Text.isInfixOf` content)
          assertBool "skill path is omitted" (not (Text.pack (skillsDir </> "haskell" </> "SKILL.md") `Text.isInfixOf` content))
        other ->
          assertFailure [i|expected text system content, got #{show other :: String}|]
    other ->
      assertFailure [i|expected captured ask-handler LLM request messages, got #{show (requestRoles <$> other) :: String}|]

testLoadSkillLoadsAdvertisedSkillInstructions :: IO ()
testLoadSkillLoadsAdvertisedSkillInstructions = withTempDir "skills-test" \skillsDir -> do
  let skillDir = skillsDir </> "haskell"
      skillPath = skillDir </> "SKILL.md"
      skillBody = "---\nname: haskell-refactor\n---\nUse small pure functions.\n"
  createDirectoryIfMissing True skillDir
  TextIO.writeFile skillPath skillBody
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "load_skill" (Aeson.object ["name" Aeson..= ("haskell-refactor" :: Text)])]
    , chatAnswer "" [toolCall "call-2" "load_skill" (Aeson.object ["name" Aeson..= ("missing" :: Text)])]
    , chatAnswer "done" []
    ]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  _ <- runAgentCapturingMessagesWithSkills (SkillsStore.SkillsConfig skillsDir) captured answers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 3 agentContext AgentTools.defaultTools (startWithUser "refactor this")
  requests <- IORef.readIORef captured
  case requests of
    _ : secondRequest : thirdRequest : _ -> do
      assertBool "next model request includes skill body" $ or
        [ skillBody `Text.isInfixOf` content
        | message <- secondRequest
        , Just (LLM.TextContent content) <- [message.content]
        ]
      assertBool "unadvertised name is rejected" $ or
        [ content == "Skill not found."
        | message <- thirdRequest
        , message.role == "tool"
        , Just (LLM.TextContent content) <- [message.content]
        ]
    other ->
      assertFailure [i|expected three model requests, got #{length other}|]

testAgentAuditRecordsToolEvents :: IO ()
testAgentAuditRecordsToolEvents = do
  answers <- IORef.newIORef
    [ chatAnswerWithUsage highTokenUsage "" [toolCall "call-1" "fetch_url" (Aeson.object ["url" Aeson..= ("https://example.test" :: Text)])]
    , chatAnswer "done" []
    ]
  fetches <- IORef.newIORef (0 :: Int)
  (toolUses, records) <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    agentRun <- startTestRuntime 4 (agentContext{Agent.toolConfig = Agent.defaultToolConfig{Agent.webFetch = True}}) [fakeWebFetchTool fetches]
    let program = Agent.defaultRuntime AgentAudit.agentAuditObserver 1000000 agentRun
    _ <- S.mapM_ (\_ -> pure ()) (Agent.agentStream program (startWithUser "fetch it"))
    (,) <$> AgentAudit.queryRecentToolUses 10 <*> AgentAudit.queryRecentAuditRecords 10
  case toolUses of
    [toolUse] -> do
      toolUse.toolName @?= "fetch_url"
      case toolUse.status of
        AgentAudit.ToolUseFinished{status} ->
          status @?= "ok"
        other ->
          assertFailure ("expected finished tool use, got " <> show other)
      toolUse.result @?= Just "fetched"
    _ ->
      assertFailure [i|expected one tool use, got #{length toolUses}|]
  assertBool "expected model token usage in audit records" (any hasHighTokenUsage records)
  assertBool "expected run start in audit records" (any (\case AgentAudit.AgentRunStarted{} -> True; _ -> False) (map (.event) records))
  assertBool "expected model start in audit records" (any (\case AgentAudit.ModelTurnStarted{} -> True; _ -> False) (map (.event) records))
  assertBool "expected run finish in audit records" (any (\case AgentAudit.AgentRunFinished{} -> True; _ -> False) (map (.event) records))

testAgentAuditMigratesLegacyRecords :: IO ()
testAgentAuditMigratesLegacyRecords =
  withSQLiteTempPath "audit-migration" \dbPath -> do
    let started = AgentAudit.ToolCallStarted
          { runId = "legacy-run"
          , turn = 1
          , toolCall = AgentAudit.ToolCallTrace "legacy-call" "fetch_url" "{}"
          }
        finished = AgentAudit.ToolCallFinished
          { runId = "legacy-run"
          , turn = 1
          , toolCallId = "legacy-call"
          , toolName = "fetch_url"
          , status = "ok"
          , result = "legacy result"
          , resultLength = 13
          , messageIds = []
          }
        legacyRow occurredAt event = LegacyAuditRow
          { legacy_id = Selda.def
          , legacy_run_id = AgentAudit.eventRunId event
          , legacy_occurred_at = occurredAt
          , legacy_linked_message_id = Nothing
          , legacy_parent_message_id = Nothing
          , legacy_event_json = jsonText event
          }
    startId <- runEff $
      runConcurrent $
        runPrim $
          runTestLog $
            StorageSQLite.runStorageSQLitePath dbPath $
              StorageEffect.runSelda do
                SeldaBackend.withBackend \backend -> liftIO $ void $
                  SeldaBackend.runStmt backend
                    "CREATE TABLE \"audit_log\"\n  ( id INTEGER PRIMARY KEY\n  , run_id TEXT NOT NULL\n  , occurred_at DATETIME NOT NULL\n  , linked_message_id TEXT\n  , parent_message_id TEXT\n  , event_json TEXT NOT NULL\n  )"
                    []
                key <- Selda.insertWithPK legacyAuditRows [legacyRow staleAuditTime started]
                Selda.insert_ legacyAuditRows [legacyRow (addUTCTime 1 staleAuditTime) finished]
                pure (fromIntegral (Selda.fromId key))
    (stored, toolUse) <- runEff $
      runConcurrent $
        runPrim $
          runTestLog $
            StorageSQLite.runStorageSQLitePath dbPath $
              AgentAudit.runAgentAudit $
                (,) <$> AgentAudit.queryAuditRecord startId <*> AgentAudit.queryToolUse startId
    stored @?= Just AgentAudit.AgentAuditRecord
      { id = startId
      , occurredAt = staleAuditTime
      , event = started
      }
    case toolUse of
      Just use -> do
        use.auditId @?= startId
        use.result @?= Just "legacy result"
        case use.status of
          AgentAudit.ToolUseFinished{status} -> status @?= "ok"
          other -> assertFailure ("expected migrated finished tool use, got " <> show other)
      Nothing ->
        assertFailure "expected migrated tool use"

testSQLiteStoragePoolRunsActionsConcurrently :: IO ()
testSQLiteStoragePoolRunsActionsConcurrently =
  withSQLiteTempPath "storage-pool" \dbPath -> do
    concurrent <- runEff $
      runTimeout $
        runConcurrent $
          runPrim $
            StorageSQLite.runStorageSQLitePath dbPath $
              withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
                runInIO do
                  firstStarted <- MVar.newEmptyMVar
                  secondStarted <- MVar.newEmptyMVar
                  release <- MVar.newEmptyMVar
                  let holdConnection started =
                        StorageEffect.runSelda $
                          liftIO $
                            runInIO do
                              MVar.putMVar started ()
                              MVar.takeMVar release
                  firstWorker <- Async.async (holdConnection firstStarted)
                  MVar.takeMVar firstStarted
                  secondWorker <- Async.async (holdConnection secondStarted)
                  startedTogether <- isJust <$> timeout 1_000_000 (MVar.takeMVar secondStarted)
                  MVar.putMVar release ()
                  unless startedTogether (MVar.takeMVar secondStarted)
                  MVar.putMVar release ()
                  Async.wait firstWorker
                  Async.wait secondWorker
                  pure startedTogether
    assertBool "separate pooled connections should execute concurrently" concurrent

testThreadStatsAccumulateRepliedBranch :: IO ()
testThreadStatsAccumulateRepliedBranch = do
  answers <- IORef.newIORef []
  replies <- IORef.newIORef []
  runAgentWith answers (ChatMock (Just replies) (Just "stats-reply") Nothing) do
    threads <- newThreadStore
    let makeMessageKey = threadMessageKey askHandlerMessage
        user1 = makeMessageKey "user-1"
        answer1 = makeMessageKey "answer-1"
        user2 = makeMessageKey "user-2"
        answer2 = makeMessageKey "answer-2"
        transcript1 = appendAssistant "A1" (startWithUser "U1")
        transcript2 = appendAssistant "A2" (appendUser "U2" transcript1)
        resource = Concurrency.Handle (Concurrency.Id 1)
    active1 <- fromMaybe (error "expected first active thread") <$>
      rememberActiveThread threads "run-1" Nothing (Just user1) askHandlerMessage "U1" resource (startWithUser "U1")
    addActiveThreadMessage threads active1 answer1
    finishActiveThread threads active1 transcript1
    active2 <- fromMaybe (error "expected second active thread") <$>
      rememberActiveThread threads "run-2" (Just answer1) (Just user2) askHandlerMessage "U2" resource transcript1
    addActiveThreadMessage threads active2 answer2
    finishActiveThread threads active2 transcript2
    persistStatsRun "run-1" answer1 Nothing (LLM.TokenUsage 100 10 110 (Just 40))
    persistStatsRun "run-2" answer2 (Just answer1.messageId) (LLM.TokenUsage 200 20 220 (Just 120))
    void $ AgentAuditStorage.persistEvent (addUTCTime 4 staleAuditTime) AgentAudit.SubAgentRunStarted
      { runId = "run-2"
      , childRunId = "child-run-1"
      , subagentId = "researcher"
      }
    void $ AgentAuditStorage.persistEvent (addUTCTime 4 staleAuditTime) AgentAudit.AgentThreadLinked
      { runId = "run-2"
      , linkedMessageId = answer2.messageId
      , linkedMessageKey = Just answer2
      , parentMessageId = Just answer1.messageId
      }
    persistChildStatsRun "child-run-1" (LLM.TokenUsage 80 10 90 (Just 20))
    void $ AgentAuditStorage.persistEvent (addUTCTime 9 staleAuditTime) AgentAudit.SubAgentRunStarted
      { runId = "child-run-1"
      , childRunId = "child-run-2"
      , subagentId = "reviewer"
      }
    persistChildStatsRun "child-run-2" (LLM.TokenUsage 60 5 65 (Just 30))
    let createResource runId resourceId =
          ResourceEffect.createNamedForRun @SubAgentResource.SubAgent runId Nothing resourceId ResourceEffect.Init
            { message = askHandlerMessage
            , arguments = SubAgentResource.SubAgentArgs
                { systemContext = ""
                , toolNames = []
                , ttlMinutes = 5
                }
            }
    void (createResource "run-1" "first-resource")
    void (createResource "run-2" "second-resource")
    void (createResource "unrelated-run" "unrelated-resource")
    runHandlers (auditHandlers threads) (statsMessage "stats-1" answer1.messageId)
    runHandlers (auditHandlers threads) (statsMessage "stats-2" answer2.messageId)
  IORef.readIORef replies >>= \case
    [firstStats, secondStats] -> do
      assertBool [i|first answer stats should include one run; got #{firstStats}|] ("- runs: 1" `Text.isInfixOf` firstStats)
      assertBool [i|first answer stats should include lifecycle timing; got #{firstStats}|] ("- run wall time: 3.0s total (includes model and tools)" `Text.isInfixOf` firstStats)
      assertBool [i|first answer stats should include model timing; got #{firstStats}|] ("- model time: 1.0s completed" `Text.isInfixOf` firstStats)
      assertBool [i|first answer stats should include context messages; got #{firstStats}|] ("- context messages: 2 now / 2 peak" `Text.isInfixOf` firstStats)
      assertBool [i|first answer stats should include run config; got #{firstStats}|] ("- run config: 8 max tool turns, 2 exposed tools" `Text.isInfixOf` firstStats)
      assertBool [i|first answer stats should include only first-run tokens; got #{firstStats}|] ("- tokens: 110 total (100 prompt, 10 completion;" `Text.isInfixOf` firstStats)
      assertBool [i|first answer stats should include only its resource; got #{firstStats}|] ("- resources: 1" `Text.isInfixOf` firstStats && "`first-resource` (`SubAgent`): ready" `Text.isInfixOf` firstStats)
      assertBool [i|first answer stats should exclude later resources; got #{firstStats}|] (not ("`second-resource`" `Text.isInfixOf` firstStats))
      assertBool [i|second answer stats should include both runs; got #{secondStats}|] ("- runs: 2" `Text.isInfixOf` secondStats)
      assertBool [i|second answer stats should sum the replied branch; got #{secondStats}|] ("- tokens: 330 total (300 prompt, 30 completion; request cache: 160 hit, 53.3%)" `Text.isInfixOf` secondStats)
      assertBool [i|second answer stats should sum the latest agent run; got #{secondStats}|] ("- current run: 220 total (200 prompt, 20 completion; request cache: 120 hit, 60.0%)" `Text.isInfixOf` secondStats)
      assertBool [i|second answer stats should count both model turns; got #{secondStats}|] ("- model turns: 2" `Text.isInfixOf` secondStats)
      assertBool [i|second answer stats should show enabled tool groups above tool calls; got #{secondStats}|]
        ("- tool enabled: essential (8), work (2)\n- tool calls:" `Text.isInfixOf` secondStats)
      assertBool [i|second answer stats should report recursive subagents separately; got #{secondStats}|] (all (`Text.isInfixOf` secondStats) ["- subagents: 2 runs", "`researcher` (`child-run-1`)", "    - subagents: 1 runs", "`reviewer` (`child-run-2`)", "- tokens: 90 total (80 prompt, 10 completion;", "- tokens: 65 total (60 prompt, 5 completion;"])
      assertBool [i|second answer stats should include branch resources; got #{secondStats}|] ("- resources: 2" `Text.isInfixOf` secondStats && all (`Text.isInfixOf` secondStats) ["`first-resource`", "`second-resource`"])
      assertBool [i|thread stats should exclude unrelated resources; got #{secondStats}|] (not ("`unrelated-resource`" `Text.isInfixOf` secondStats))
    other ->
      assertFailure [i|expected two thread stats replies, got #{length other}|]

testThreadStatsShowActiveRunningTools :: IO ()
testThreadStatsShowActiveRunningTools = do
  answers <- IORef.newIORef []
  replies <- IORef.newIORef []
  runAgentWith answers (ChatMock (Just replies) (Just "stats-reply") Nothing) do
    threads <- newThreadStore
    let makeMessageKey = threadMessageKey askHandlerMessage
        original = makeMessageKey "user-1"
        assistant = makeMessageKey "assistant-1"
        toolMessage = makeMessageKey "tool-message-1"
        steer =
          askHandlerMessage
            { messageId = Just "user-2"
            , replyToMessageId = Just assistant.messageId
            , text = "steer"
            }
        resource = Concurrency.Handle (Concurrency.Id 1)
        transcript = startWithUser "U1"
    active <- fromMaybe (error "expected active thread") <$>
      rememberActiveThread threads "active-run" Nothing (Just original) askHandlerMessage "U1" resource transcript
    addActiveThreadMessage threads active assistant
    addActiveThreadMessage threads active toolMessage
    void $ enqueueActiveThreadSteer threads steer (inputWithImages steer.text [])
    now <- liftIO getCurrentTime
    void $ AgentAuditStorage.persistEvent now AgentAudit.AgentRunStarted
      { runId = "active-run"
      , messageId = Just original.messageId
      , maxTurns = 8
      , exposedTools = ["run_bash"]
      }
    void $ AgentAuditStorage.persistEvent now AgentAudit.ModelTurnStarted
      { runId = "active-run"
      , turn = 1
      , messageCount = 2
      , exposedTools = ["run_bash"]
      , toolGroups = Just [("essential", 8)]
      }
    void $ AgentAuditStorage.persistEvent now AgentAudit.ModelTurnFinished
      { runId = "active-run"
      , turn = 1
      , answerKind = "tool_request"
      , contentLength = 0
      , toolCalls = []
      , tokenUsage = Just (LLM.TokenUsage 100 10 110 (Just 40))
      }
    void $ AgentAuditStorage.persistEvent now AgentAudit.ModelTurnStarted
      { runId = "active-run"
      , turn = 2
      , messageCount = 4
      , exposedTools = ["run_bash"]
      , toolGroups = Just [("essential", 8), ("work", 1)]
      }
    void $ AgentAuditStorage.persistEvent now AgentAudit.ModelTurnFinished
      { runId = "active-run"
      , turn = 2
      , answerKind = "tool_request"
      , contentLength = 0
      , toolCalls = [AgentAudit.ToolCallTrace "call-1" "run_bash" "{}"]
      , tokenUsage = Just (LLM.TokenUsage 300 30 330 (Just 200))
      }
    void $ AgentAuditStorage.persistEvent now AgentAudit.ToolCallStarted
      { runId = "active-run"
      , turn = 2
      , toolCall = AgentAudit.ToolCallTrace "call-1" "run_bash" "{}"
      }
    let staleTime = addUTCTime (-10) now
    void $ AgentAuditStorage.persistEvent staleTime AgentAudit.AgentRunStarted
      { runId = "stale-run"
      , messageId = Just original.messageId
      , maxTurns = 8
      , exposedTools = ["fetch_url"]
      }
    void $ AgentAuditStorage.persistEvent staleTime AgentAudit.ToolCallStarted
      { runId = "stale-run"
      , turn = 1
      , toolCall = AgentAudit.ToolCallTrace "stale-call" "fetch_url" "{}"
      }
    void $ AgentAuditStorage.persistEvent staleTime AgentAudit.AgentRunFinished
      { runId = "stale-run"
      , status = "answered"
      , finalLength = 0
      , turnsUsed = 1
      }
    void $ AgentAuditStorage.persistEvent staleTime AgentAudit.AgentThreadLinked
      { runId = "stale-run"
      , linkedMessageId = original.messageId
      , linkedMessageKey = Just original
      , parentMessageId = Nothing
      }
    for_ ["user-1", "assistant-1", "tool-message-1", "user-2"] \messageId ->
      runHandlers (auditHandlers threads) (statsMessage (textMessageId ("stats-" <> messageIdText messageId)) messageId)
    finishActiveThread threads active transcript
  statsReplies <- IORef.readIORef replies
  length statsReplies @?= 4
  for_ statsReplies \reply -> do
    assertBool "every active alias should report active status" ("- status: active" `Text.isInfixOf` reply)
    assertBool "active stats should include every current-run model turn" ("- model turns: 2" `Text.isInfixOf` reply)
    assertBool "active stats should sum every model request in the replied branch" ("- tokens: 440 total (400 prompt, 40 completion; request cache: 240 hit, 60.0%)" `Text.isInfixOf` reply)
    assertBool "active stats should sum every model request in the current agent run" ("- current run: 440 total (400 prompt, 40 completion; request cache: 240 hit, 60.0%)" `Text.isInfixOf` reply)
    assertBool "active stats should expose current run phase and steer queue" ("- current run: `active-run` (phase: tools, 1 pending steers)" `Text.isInfixOf` reply)
    assertBool "active stats should expose context message growth" ("- context messages: 4 now / 4 peak" `Text.isInfixOf` reply)
    assertBool "active stats should expose enabled tool groups above tool calls"
      ("- tool enabled: essential (8), work (1)\n- tool calls:" `Text.isInfixOf` reply)
    assertBool "active stats should separate stale historical tools" ("- tool calls: 2 (0 ok, 0 failed, 0 interrupted, 1 running, 1 stale/unreported)" `Text.isInfixOf` reply)
    assertBool "active stats should name the running tool" ("`run_bash`" `Text.isInfixOf` reply)
    assertBool "active stats should not list a stale tool as running" (not ("`fetch_url` (`id=" `Text.isInfixOf` reply))

persistStatsRun
  :: (StorageEffect.Storage :> es, KatipE :> es, IOE :> es)
  => Text
  -> ThreadMessageKey
  -> Maybe MessageId
  -> LLM.TokenUsage
  -> Eff es ()
persistStatsRun runId linkedKey parentMessageId tokenUsage = do
  void $ AgentAuditStorage.persistEvent staleAuditTime AgentAudit.AgentRunStarted
    { runId
    , messageId = Just linkedKey.messageId
    , maxTurns = 8
    , exposedTools = ["sandbox", "subagent"]
    }
  void $ AgentAuditStorage.persistEvent (addUTCTime 1 staleAuditTime) AgentAudit.ModelTurnStarted
    { runId
    , turn = 1
    , messageCount = 2
    , exposedTools = ["sandbox", "subagent"]
    , toolGroups = Just [("essential", 8), ("work", 2)]
    }
  void $ AgentAuditStorage.persistEvent (addUTCTime 2 staleAuditTime) AgentAudit.ModelTurnFinished
    { runId
    , turn = 1
    , answerKind = "final"
    , contentLength = 2
    , toolCalls = []
      , tokenUsage = Just tokenUsage
      }
  void $ AgentAuditStorage.persistEvent (addUTCTime 3 staleAuditTime) AgentAudit.AgentRunFinished
    { runId
    , status = "answered"
    , finalLength = 2
    , turnsUsed = 1
    }
  void $ AgentAuditStorage.persistEvent (addUTCTime 3 staleAuditTime) AgentAudit.AgentThreadLinked
    { runId
    , linkedMessageId = linkedKey.messageId
    , linkedMessageKey = Just linkedKey
    , parentMessageId
    }

persistChildStatsRun
  :: (StorageEffect.Storage :> es, KatipE :> es, IOE :> es)
  => Text
  -> LLM.TokenUsage
  -> Eff es ()
persistChildStatsRun runId tokenUsage = do
  void $ AgentAuditStorage.persistEvent (addUTCTime 5 staleAuditTime) AgentAudit.AgentRunStarted
    { runId
    , messageId = Nothing
    , maxTurns = 8
    , exposedTools = ["sandbox"]
    }
  void $ AgentAuditStorage.persistEvent (addUTCTime 6 staleAuditTime) AgentAudit.ModelTurnStarted
    { runId
    , turn = 1
    , messageCount = 1
    , exposedTools = ["sandbox"]
    , toolGroups = Just [("essential", 8), ("sandbox", 7)]
    }
  void $ AgentAuditStorage.persistEvent (addUTCTime 7 staleAuditTime) AgentAudit.ModelTurnFinished
    { runId
    , turn = 1
    , answerKind = "final"
    , contentLength = 4
    , toolCalls = []
    , tokenUsage = Just tokenUsage
    }
  void $ AgentAuditStorage.persistEvent (addUTCTime 8 staleAuditTime) AgentAudit.AgentRunFinished
    { runId
    , status = "answered"
    , finalLength = 4
    , turnsUsed = 1
    }

statsMessage :: MessageId -> MessageId -> IncomingMessage
statsMessage messageId parentId =
  askHandlerMessage
    { messageId = Just messageId
    , replyToMessageId = Just parentId
    , text = "!stats"
    }

testThreadAuditScope :: IO ()
testThreadAuditScope = do
  (firstRecords, secondRecords, otherPlatformRecords) <- runEff $
    runConcurrent $
      runPrim $
        runTestLog $
          StorageSQLite.runStorageSQLitePath ":memory:" do
            AgentAuditStorage.ensureAgentAuditTable
            let firstKey = ThreadMessageKey PlatformQQ (Just 1) "same-message"
                secondKey = ThreadMessageKey PlatformQQ (Just 2) "same-message"
                otherPlatformKey = ThreadMessageKey PlatformTelegram (Just 1) "same-message"
            persistAuditOccurrence firstKey "first"
            persistAuditOccurrence secondKey "second"
            persistAuditOccurrence otherPlatformKey "other-platform"
            (,,)
              <$> AgentAuditStorage.queryStoredThreadAudit firstKey
              <*> AgentAuditStorage.queryStoredThreadAudit secondKey
              <*> AgentAuditStorage.queryStoredThreadAudit otherPlatformKey
  auditToolNames firstRecords @?= ["first"]
  auditToolNames secondRecords @?= ["second"]
  auditToolNames otherPlatformRecords @?= ["other-platform"]
  where
    persistAuditOccurrence linkedKey toolName = do
      void $ AgentAuditStorage.persistEvent staleAuditTime AgentAudit.ToolCallStarted
        { runId = "agent-reused"
        , turn = 1
        , toolCall = AgentAudit.ToolCallTrace{id = "call", name = toolName, arguments = "{}"}
        }
      void $ AgentAuditStorage.persistEvent staleAuditTime AgentAudit.AgentThreadLinked
        { runId = "agent-reused"
        , linkedMessageId = linkedKey.messageId
        , linkedMessageKey = Just linkedKey
        , parentMessageId = Nothing
        }

    auditToolNames records =
      [ auditCall.name
      | AgentAudit.AgentAuditRecord{event = AgentAudit.ToolCallStarted{toolCall = auditCall}} <- records
      ]

testAgentAuditRecentRecordsExcludeSyntheticRestartedRuns :: IO ()
testAgentAuditRecentRecordsExcludeSyntheticRestartedRuns = do
  (records, toolUses) <- runEff $
    runConcurrent $
      runPrim $
        runTestLog $
          StorageSQLite.runStorageSQLitePath ":memory:" do
            AgentAuditStorage.ensureAgentAuditTable
            void $ AgentAuditStorage.persistEvent staleAuditTime AgentAudit.ToolCallStarted
              { runId = "run-stale"
              , turn = 1
              , toolCall = AgentAudit.ToolCallTrace
                  { id = "call-stale"
                  , name = "fetch_url"
                  , arguments = "{}"
                  }
              }
            AgentAudit.runAgentAudit do
              (,) <$> AgentAudit.queryRecentAuditRecords 10 <*> AgentAudit.queryRecentToolUses 10
  case records of
    [record] ->
      assertBool "recent raw audit records should keep persisted ids" (record.id > 0)
    _ ->
      assertFailure [i|expected one persisted audit record, got #{length records}|]
  case toolUses of
    [toolUse] ->
      case toolUse.status of
        AgentAudit.ToolUseInterrupted{reason} ->
          reason @?= "restarted"
        other ->
          assertFailure ("expected restarted stale tool use, got " <> show other)
    _ ->
      assertFailure [i|expected one projected tool use, got #{length toolUses}|]

staleAuditTime :: UTCTime
staleAuditTime =
  UTCTime (fromGregorian 2020 1 1) 0

testAgentAuditStorageOmitsLargeToolResults :: IO ()
testAgentAuditStorageOmitsLargeToolResults =
  withSQLiteTempPath "audit-large-tool-result" \dbPath ->
    withTempDir "audit-large-tool-result-media" \dir -> do
      let cfg = MediaConfig.defaultConfig{MediaConfig.cacheDir = dir </> "cache"}
          toolResultText = "{\"items\":[" <> Text.intercalate "," (replicate 5000 "\"value\"") <> "]}"
          resultBytes = TextEncoding.encodeUtf8 toolResultText
      answers <- IORef.newIORef
        [ chatAnswer "" [toolCall "call-1" "large_audit_result" (Aeson.object [])]
        , chatAnswer "done" []
        ]
      let runStack =
            runFileSystem
              . runProcess
              . runFail
              . runConcurrent
              . runPrim
              . runTestLog
              . StorageSQLite.runStorageSQLitePath dbPath
              . ConcurrencyManager.runConcurrencyManager
              . ResourceManager.runResourceManager
              . HTTP.runHTTP
              . runTimeout
              . MediaInterpreter.runMedia cfg
              . LLMTest.runLLMWith
                  (\_ -> S.yield "unused text stream answer" $> "unused text stream answer")
                  (\_ _ -> S.yield "unused image answer" $> "unused image answer")
                  (\_ _ _ _ -> S.yield "unused image edit answer" $> "unused image edit answer")
                  (\_ _ -> S.yield "unused audio answer" $> "unused audio answer")
                  (\_ _ -> do
                      answer <- liftIO (popAnswer answers)
                      case answer of
                        LLM.ChatFinalAnswer{content} ->
                          S.yield content
                        LLM.ChatToolRequest{content}
                          | Text.null content -> pure ()
                          | otherwise -> S.yield content
                      pure answer)
              . AgentAudit.runAgentAudit
              . Chat.runChatWith NoopChatDriver
      runResult <- runEff $ runStack do
        agentRun <- startTestRuntime 4 agentContext [largeAuditResultTool toolResultText]
        void $ S.toList (Agent.agentStream (Agent.defaultRuntime AgentAudit.agentAuditObserver 1000000 agentRun) (startWithUser "audit large result"))
        uses <- AgentAudit.queryRecentToolUses 10
        mediaFiles <- Media.listMediaFiles
        pure (uses, mediaFiles)
      (toolUses, files) <- either assertFailure pure runResult
      case toolUses of
        [toolUse] -> do
          case toolUse.result of
            Just stored -> do
              assertBool "large audit result is replaced by omitted marker" ("[tool result omitted;" `Text.isPrefixOf` stored)
              assertBool "audit marker keeps inferred JSON MIME" ("mime=application/json" `Text.isInfixOf` stored)
              assertBool "audit marker points to media cache" ("media_id=mf_" `Text.isInfixOf` stored)
              assertBool "audit marker keeps a preview" ("preview=\"{\\\"items\\\"" `Text.isInfixOf` stored)
              assertBool "audit row should not retain the full result tail" (not ("\"value\"]}" `Text.isInfixOf` stored))
            Nothing ->
              assertFailure "expected stored audit result"
          case toolUse.status of
            AgentAudit.ToolUseFinished{} ->
              pure ()
            other ->
              assertFailure ("expected finished tool use, got " <> show other)
        _ ->
          assertFailure [i|expected one tool use, got #{length toolUses}|]
      case files of
        [file] -> do
          file.mimeType @?= "application/json"
          file.size @?= StrictByteString.length resultBytes
        other ->
          assertFailure [i|expected one cached result file, got #{length other}|]

largeAuditResultTool :: Text -> AgentTool.Tool (Eff es)
largeAuditResultTool result =
  AgentTool.withDescription "fake large audit result"
  $ AgentTool.tool "large_audit_result" AgentTool.noArguments
      (pure (Agent.toolText result))

testAgentOmitsLargeToolResultAfterOneModelTurnConsumesIt :: IO ()
testAgentOmitsLargeToolResultAfterOneModelTurnConsumesIt = do
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "large_result" (Aeson.object [])]
    , chatAnswer "done" []
    ]
  let largeResult = "large-result:" <> Text.replicate 5000 "x"
      oneShotLargeResultTool =
        AgentTool.withDescription "return a large result"
        $ AgentTool.tool "large_result" AgentTool.noArguments
            (pure (Agent.toolText largeResult))
  (_, transcript) <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 4 agentContext [oneShotLargeResultTool] (startWithUser "run it")
  requests <- IORef.readIORef captured
  case requests of
    [_firstRequest, secondRequest] -> do
      let encoded = jsonText secondRequest
      assertBool "current model turn keeps full large tool result" (largeResult `Text.isInfixOf` encoded)
      assertBool "current model turn is not replaced by persistence marker" (not ("[tool result omitted;" `Text.isInfixOf` encoded))
    other ->
      assertFailure [i|expected two LLM requests, got #{length other}|]
  continuationAnswers <- IORef.newIORef [chatAnswer "continued" []]
  _ <- runAgentCapturingMessages captured continuationAnswers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 1 agentContext [] transcript
  continuedRequests <- IORef.readIORef captured
  case drop 2 continuedRequests of
    [continuedRequest] -> do
      let encoded = jsonText continuedRequest
      assertBool "later model turn sees omitted tool result" ("[tool result omitted;" `Text.isInfixOf` encoded)
      assertBool "later model turn does not keep full large tool result" (not (largeResult `Text.isInfixOf` encoded))
    other ->
      assertFailure [i|expected one continuation LLM request, got #{length other}|]

testAgentHardLimitsImmediateToolResults :: IO ()
testAgentHardLimitsImmediateToolResults = do
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "huge_result" (Aeson.object [])]
    , chatAnswer "done" []
    ]
  let hugeResult = "huge-result:" <> Text.replicate 10001 "x"
      hugeResultTool =
        AgentTool.withDescription "return a huge result"
        $ AgentTool.tool "huge_result" AgentTool.noArguments
            (pure (Agent.toolText hugeResult))
  _ <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 4 agentContext [hugeResultTool] (startWithUser "run it")
  requests <- IORef.readIORef captured
  case requests of
    [_firstRequest, secondRequest] -> do
      let encoded = jsonText secondRequest
      assertBool "immediate model input uses the omitted marker" ("[tool result omitted;" `Text.isInfixOf` encoded)
      assertBool "immediate model input excludes the huge result" (not (hugeResult `Text.isInfixOf` encoded))
    other ->
      assertFailure [i|expected two LLM requests, got #{length other}|]

testAgentAuditRecordsStructuredToolFailureCategory :: IO ()
testAgentAuditRecordsStructuredToolFailureCategory = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "run_bash" (Aeson.object ["script" Aeson..= ("echo nope" :: Text)])]
    , chatAnswer "done" []
    ]
  toolUses <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    agentRun <- startTestRuntime 4 agentContext AgentTools.defaultTools
    let program = Agent.defaultRuntime AgentAudit.agentAuditObserver 1000000 agentRun
    _ <- S.mapM_ (\_ -> pure ()) (Agent.agentStream program (startWithUser "run command"))
    AgentAudit.queryRecentToolUses 10
  case toolUses of
    [toolUse] ->
      case toolUse.status of
        AgentAudit.ToolUseFinished{status} ->
          status @?= "permission_denied"
        other ->
          assertFailure ("expected finished tool use, got " <> show other)
    _ ->
      assertFailure [i|expected one tool use, got #{length toolUses}|]

testAskHandlerAnnouncesNoisyToolCallsWithoutInternalIds :: IO ()
testAskHandlerAnnouncesNoisyToolCallsWithoutInternalIds = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "image_generate" (Aeson.object ["prompt" Aeson..= ("cat" :: Text)])]
    , chatAnswer "done" []
    ]
  replies <- IORef.newIORef ([] :: [Text])
  _ <- runAgentWith answers (ChatMock (Just replies) (Just "45") Nothing) do
    threads <- newThreadStore
    runAskHandlersAndWait Agent.defaultToolConfig askHandlerConfig threads askHandlerMessage
  sent <- IORef.readIORef replies
  case sent of
    progress : _ ->
      progress @?= "😻 FM：正在调用 image_generate 工具…"
    _ ->
      assertFailure [i|expected noisy tool progress reply, got #{show sent :: String}|]

testAskHandlerAnnouncesEachNoisyToolTypeOnlyOnce :: IO ()
testAskHandlerAnnouncesEachNoisyToolTypeOnlyOnce = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "first-a" "noisy_a" (Aeson.object [])]
    , chatAnswer "" [toolCall "second-a" "noisy_a" (Aeson.object [])]
    , chatAnswer "" [toolCall "first-b" "noisy_b" (Aeson.object [])]
    , chatAnswer "done" []
    ]
  replies <- IORef.newIORef ([] :: [Text])
  let noisyProbe toolName =
        AgentTool.noisy
          . AgentTool.withDescription "Noisy progress deduplication probe."
          $ AgentTool.tool toolName AgentTool.noArguments (pure (Agent.toolText "ok"))
      tools = [noisyProbe "noisy_a", noisyProbe "noisy_b"]
  _ <- runAgentWith answers (ChatMock (Just replies) (Just "45-dedup") Nothing) do
    threads <- newThreadStore
    runAskHandlersWithToolsAndWait Agent.defaultToolConfig tools askHandlerConfig threads askHandlerMessage
  IORef.readIORef replies >>= (@?=
    [ "😻 FM：正在调用 noisy_a 工具…"
    , "😻 FM：正在调用 noisy_b 工具…"
    , "😻 FM：done"
    ])

testAskHandlerHidesStreamedNarrationBeforeToolCalls :: IO ()
testAskHandlerHidesStreamedNarrationBeforeToolCalls = do
  answers <- IORef.newIORef
    [ StreamingAnswer
        { chunks = ["我会", "查天气"]
        , answer = chatAnswer "我会查天气" [toolCall "call-1" "get_weather" (Aeson.object ["location" Aeson..= ("Berlin" :: Text)])]
        }
    , StreamingAnswer
        { chunks = ["我已经查", "到天气"]
        , answer = chatAnswer "我已经查到天气" []
        }
    ]
  replies <- IORef.newIORef ([] :: [Text])
  _ <- runAgentWithStreamingAnswers answers (ChatMock (Just replies) (Just "46") Nothing) do
    threads <- newThreadStore
    runAskHandlersAndWait Agent.defaultToolConfig askHandlerConfig threads askHandlerMessage
  IORef.readIORef replies >>= (@?= ["😻 FM：我已经查到天气"])

testLongFinalRepliesUseMatrixEditChunks :: IO ()
testLongFinalRepliesUseMatrixEditChunks = do
  let reply = Text.replicate 300 "长"
      chunks = AgentRun.streamingReplyChunks reply
  Text.concat chunks @?= "😻 FM：" <> reply
  viaNonEmpty head chunks @?= Just "😻 FM：长长"
  assertBool "long reply should produce multiple edit chunks" (length chunks > 1)
  assertBool "tail chunks should fit the Matrix edit cadence" (all ((<= 128) . Text.length) (drop 1 chunks))

testShortFinalRepliesRemainSingleChunk :: IO ()
testShortFinalRepliesRemainSingleChunk =
  AgentRun.streamingReplyChunks "短回复" @?= ["😻 FM：短回复"]

testAskHandlerAutomaticallyContinuesAfterToolLimit :: IO ()
testAskHandlerAutomaticallyContinuesAfterToolLimit = do
  answers <- IORef.newIORef
    [ streamingToolAnswer "I will inspect this first." "first" "message_info"
    , streamingToolAnswer "Let me view the image next." "limited" "message_info"
    , streamingToolAnswer "Retrying the pending tool." "retry" "message_info"
    , StreamingAnswer{chunks = ["已经", "完成"], answer = chatAnswer "已经完成" []}
    ]
  replies <- IORef.newIORef ([] :: [Text])
  _ <- runAgentWithStreamingAnswers answers (ChatMock (Just replies) (Just "47") Nothing) do
    threads <- newThreadStore
    let cfg = askHandlerConfig{agentMaxTurns = 2}
    runAskHandlersAndWait Agent.defaultToolConfig cfg threads askHandlerMessage
  IORef.readIORef replies >>= (@?= ["😻 FM：已经完成"])

testAskHandlerBoundsAutomaticToolLimitContinuations :: IO ()
testAskHandlerBoundsAutomaticToolLimitContinuations = do
  answers <- IORef.newIORef
    [ streamingToolAnswer "run one" "run-1" "message_info"
    , streamingToolAnswer "limit one" "limit-1" "message_info"
    , streamingToolAnswer "run two" "run-2" "message_info"
    , streamingToolAnswer "limit two" "limit-2" "message_info"
    , streamingToolAnswer "run three" "run-3" "message_info"
    , streamingToolAnswer "limit three" "limit-3" "message_info"
    ]
  replies <- IORef.newIORef ([] :: [Text])
  _ <- runAgentWithStreamingAnswers answers (ChatMock (Just replies) (Just "48") Nothing) do
    threads <- newThreadStore
    let cfg = askHandlerConfig{agentMaxTurns = 2}
    runAskHandlersAndWait Agent.defaultToolConfig cfg threads askHandlerMessage
  sent <- IORef.readIORef replies
  sent @?= ["😻 FM：这次任务步骤太多，我连续尝试后仍没能完整做完。"]
  let visible = Text.intercalate "\n" sent
  assertBool "does not expose the agent tool budget" (not ("工具调用轮数" `Text.isInfixOf` visible))
  assertBool "does not ask for another user message" (not ("回复下一条消息" `Text.isInfixOf` visible))
  assertBool "does not expose a tool name" (not ("message_info" `Text.isInfixOf` visible))

streamingToolAnswer :: Text -> Text -> Text -> StreamingAnswer
streamingToolAnswer narration callId toolName =
  StreamingAnswer
    { chunks = [narration]
    , answer = chatAnswer narration [toolCall callId toolName (Aeson.object [])]
    }

testAgentStreamsToolRequestContentBeforeToolNotification :: IO ()
testAgentStreamsToolRequestContentBeforeToolNotification = do
  answers <- IORef.newIORef
    [ chatAnswer "我先查看当前消息。" [toolCall "call-1" "message_info" (Aeson.object [])]
    , chatAnswer "done" []
    ]
  outputs S.:> result <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    S.toList (runTestAgentStreaming 4 agentContext AgentTools.defaultTools (startWithUser "inspect"))
  streamAnswerText outputs @?= "我先查看当前消息。done"
  case outputs of
    [Agent.ContentDelta progress, Agent.ToolCallNotification toolCalls, Agent.ContentDelta finalChunk] -> do
      progress @?= "我先查看当前消息。"
      map (.name) (toList toolCalls) @?= ["message_info"]
      finalChunk @?= "done"
      case find ((not . null) . (.toolCalls)) (transcriptMessagesList result) of
        Just LLM.ChatMessage{role, content = Just (LLM.TextContent content), toolCalls = savedToolCalls} -> do
          role @?= "assistant"
          content @?= "我先查看当前消息。"
          map (.name) savedToolCalls @?= ["message_info"]
        other ->
          assertFailure [i|expected assistant tool request snapshot, got #{show other :: String}|]
    other ->
      assertFailure [i|expected separated intermediate and final output, got #{showSeparatedOutputs other}|]

testChatAnswerJsonRemainsObjectCompatible :: IO ()
testChatAnswerJsonRemainsObjectCompatible = do
  let call = toolCall "call-1" "fetch_url" (Aeson.object ["url" Aeson..= ("https://example.test" :: Text)])
  Aeson.toJSON (chatAnswer "done" []) @?=
    Aeson.object
      [ "content" Aeson..= ("done" :: Text)
      , "toolCalls" Aeson..= ([] :: [LLM.ToolCall])
      ]
  Aeson.toJSON (chatAnswer "checking" [call]) @?=
    Aeson.object
      [ "content" Aeson..= ("checking" :: Text)
      , "toolCalls" Aeson..= [call]
      ]

testReplyBodyParsesStructuredContent :: IO ()
testReplyBodyParsesStructuredContent = do
  ReplyBody.replyContentFromBody
    (Text.unlines ["hello", "[image] https://example.test/a.png", "world", "  [image] file:///tmp/b.png  "])
    @?= ReplyBody.ReplyContent
      { text = "hello\nworld"
      , images = ["https://example.test/a.png", "file:///tmp/b.png"]
      }
  ReplyBody.replyContentToBody
    ReplyBody.ReplyContent
      { text = "hello"
      , images = ["https://example.test/a.png", "file:///tmp/b.png"]
      }
    @?= "hello\n[image] https://example.test/a.png\n[image] file:///tmp/b.png"
  ReplyBody.renderReplyBody "hello\n[image] https://example.test/a.png\nworld" @?= "hello\nworld"
  ReplyBody.replyImageUrls "hello\n[image] https://example.test/a.png\n[image] file:///tmp/b.png" @?=
    ["https://example.test/a.png", "file:///tmp/b.png"]

testLLMToolRequestContentStreamsImmediatelyWhenEnabled :: IO ()
testLLMToolRequestContentStreamsImmediatelyWhenEnabled = do
  let payloads =
        [ streamPayload (Aeson.object ["content" Aeson..= ("我先查看当前消息。" :: Text)])
        , streamPayload
            ( Aeson.object
                [ "tool_calls" Aeson..=
                    [ Aeson.object
                        [ "index" Aeson..= (0 :: Int)
                        , "id" Aeson..= ("call-1" :: Text)
                        , "function" Aeson..=
                            Aeson.object
                              [ "name" Aeson..= ("message_info" :: Text)
                              , "arguments" Aeson..= ("{}" :: Text)
                              ]
                        ]
                    ]
                ]
            )
        ]
  case LLMTransport.chatStreamTextFromPayloads True payloads of
    Right (outputs, LLM.ChatToolRequest{content, toolCalls}) -> do
      outputs @?= ["我先查看当前消息。"]
      content @?= "我先查看当前消息。"
      map (.name) (toList toolCalls) @?= ["message_info"]
    Right other ->
      assertFailure [i|expected tool request stream result, got #{show other :: String}|]
    Left err ->
      assertFailure (Text.unpack err)

testLLMStreamingResponsePreservesTokenUsage :: IO ()
testLLMStreamingResponsePreservesTokenUsage = do
  let usage = Aeson.object
        [ "prompt_tokens" Aeson..= (900 :: Int)
        , "completion_tokens" Aeson..= (200 :: Int)
        , "total_tokens" Aeson..= (1100 :: Int)
        , "prompt_tokens_details" Aeson..= Aeson.object
            ["cached_tokens" Aeson..= (600 :: Int)]
        ]
      payloads =
        [ streamPayload (Aeson.object ["content" Aeson..= ("done" :: Text)])
        , Aeson.object
            [ "choices" Aeson..= ([] :: [Aeson.Value])
            , "usage" Aeson..= usage
            ]
        ]
  case LLMTransport.chatStreamTextFromPayloads True payloads of
    Right (_outputs, answer) ->
      LLM.chatAnswerTokenUsage answer @?= Just highTokenUsage
    Left err ->
      assertFailure (Text.unpack err)

testLLMImageStreamRequestAsksOnlyForFinalImage :: IO ()
testLLMImageStreamRequestAsksOnlyForFinalImage =
  LLMTransport.imageGenerationStreamingRequestPayload imageStreamTestConfig LLM.defaultImageRequestOptions "gpt-image-2" "draw this"
    @?=
      Aeson.object
        [ "model" Aeson..= ("gpt-image-2" :: Text)
        , "prompt" Aeson..= ("draw this" :: Text)
        , "output_format" Aeson..= ("webp" :: Text)
        , "stream" Aeson..= True
        , "partial_images" Aeson..= (0 :: Int)
        ]

testLLMAudioSpeechRequestIncludesProviderOptions :: IO ()
testLLMAudioSpeechRequestIncludesProviderOptions =
  LLMTransport.audioSpeechRequestPayload audioSpeechTestConfig options "tts-model" "say this"
    @?=
      Aeson.object
        [ "model" Aeson..= ("tts-model" :: Text)
        , "input" Aeson..= ("say this" :: Text)
        , "voice" Aeson..= ("verse" :: Text)
        , "response_format" Aeson..= ("wav" :: Text)
        , "speed" Aeson..= (1.25 :: Double)
        , "instructions" Aeson..= ("speak warmly" :: Text)
        ]
  where
    options = LLM.AudioRequestOptions
      { LLM.voice = Just "verse"
      , LLM.responseFormat = Just "wav"
      , LLM.speed = Nothing
      , LLM.instructions = Nothing
      }

testLLMImageStreamCompletedEventYieldsFinalImage :: IO ()
testLLMImageStreamCompletedEventYieldsFinalImage =
  case LLMTransport.imageGenerationStreamBytesFromPayloads [completed] of
    Right bytes ->
      bytes @?= "final-image"
    Left err ->
      assertFailure (Text.unpack err)
  where
    completed =
      Aeson.object
        [ "type" Aeson..= ("image_generation.completed" :: Text)
        , "b64_json" Aeson..= ("ZmluYWwtaW1hZ2U=" :: Text)
        ]

testLLMImageEditStreamCompletedEventYieldsFinalImage :: IO ()
testLLMImageEditStreamCompletedEventYieldsFinalImage =
  case LLMTransport.imageGenerationStreamBytesFromPayloads [completed] of
    Right bytes ->
      bytes @?= "edited-image"
    Left err ->
      assertFailure (Text.unpack err)
  where
    completed =
      Aeson.object
        [ "type" Aeson..= ("image_edit.completed" :: Text)
        , "b64_json" Aeson..= ("ZWRpdGVkLWltYWdl" :: Text)
        ]

testLLMImageStreamIgnoresPartialEventWithoutFinalImage :: IO ()
testLLMImageStreamIgnoresPartialEventWithoutFinalImage =
  case LLMTransport.imageGenerationStreamBytesFromPayloads [partial] of
    Left err ->
      err @?= "Image generation streaming response was empty: no image output."
    Right bytes ->
      assertFailure [i|expected empty stream error, got #{show bytes :: String}|]
  where
    partial =
      Aeson.object
        [ "type" Aeson..= ("image_generation.partial_image" :: Text)
        , "b64_json" Aeson..= ("cGFydGlhbC1pbWFnZQ==" :: Text)
        , "partial_image_index" Aeson..= (0 :: Int)
        ]

testLLMLogJsonTruncatesBase64ImagePayloads :: IO ()
testLLMLogJsonTruncatesBase64ImagePayloads = do
  let payload = Text.replicate 160 "A"
      imageRef = "data:image/png;base64," <> payload
      logged = Log.logJsonText [LLM.userWithImages "look" [imageRef]]
  assertBool "log JSON should not contain the full base64 payload" (not (payload `Text.isInfixOf` logged))
  assertBool "log JSON should keep a recognizable truncated data URL" (("data:image/png;base64," <> Text.replicate 96 "A" <> "...") `Text.isInfixOf` logged)

imageStreamTestConfig :: LLMConfig.ImageProviderConfig
imageStreamTestConfig =
  LLMConfig.defaultImageProviderConfig
    { LLMConfig.outputFormat = Just "webp"
    }

audioSpeechTestConfig :: LLMConfig.AudioProviderConfig
audioSpeechTestConfig =
  LLMConfig.defaultAudioProviderConfig
    { LLMConfig.speed = Just 1.25
    , LLMConfig.instructions = Just "speak warmly"
    }

testLLMStreamingEffectPreservesYieldedChunks :: IO ()
testLLMStreamingEffectPreservesYieldedChunks = do
  chunks S.:> answer <- runEff $
    LLMTest.runLLMWith
      (\_ -> S.each ["he", "llo"] $> "hello")
      (\_ _ -> S.yield "unused image answer" $> "unused image answer")
      (\_ _ _ _ -> S.yield "unused image edit answer" $> "unused image edit answer")
      (\_ _ -> S.yield "unused audio answer" $> "unused audio answer")
      (\_ _ -> S.each ["to", "ol"] $> chatAnswer "tool" []) do
        S.toList (LLM.askWithToolsStreaming [] [LLM.userText "hello"])
  chunks @?= ["to", "ol"]
  case answer of
    LLM.ChatFinalAnswer{content} ->
      content @?= "tool"
    other ->
      assertFailure [i|expected final streaming answer, got #{show other :: String}|]

testEmptyChatReplySendsNothing :: IO ()
testEmptyChatReplySendsNothing = do
  replies <- IORef.newIORef ([] :: [(Maybe MessageId, Text)])
  nextReplyId <- IORef.newIORef (1 :: Integer)
  _ <- runEff $ runPrim $
    Chat.runChatWith
      defaultAgentMockChatDriver{agentReply = recordReply replies nextReplyId} $
        Chat.replyTo testMessage ""
  IORef.readIORef replies >>= (@?= [])

testEmptyStreamingChunksAreIgnored :: IO ()
testEmptyStreamingChunksAreIgnored = do
  replies <- IORef.newIORef ([] :: [(Maybe MessageId, Text)])
  edits <- IORef.newIORef ([] :: [(MessageId, Text)])
  updates <- IORef.newIORef ([] :: [Text])
  nextReplyId <- IORef.newIORef (1 :: Integer)
  let stream chunks = runEff $ runPrim $
        Chat.runChatWith
          defaultAgentMockChatDriver
            { agentReply = recordReply replies nextReplyId
            , agentEditMessage = recordEdit edits
            , agentMessageOutPolicy = \_ -> pure (Chat.EditableMessage 2 100)
            } $
            S.mapM_
              (\update -> liftIO $ IORef.modifyIORef' updates (<> [update.answer]))
              (Chat.streamReplyTo testMessage (S.each chunks $> ()))
  _ <- stream [""]
  IORef.readIORef replies >>= (@?= [])
  IORef.readIORef updates >>= (@?= [])
  _ <- stream ["", "answer"]
  IORef.readIORef replies >>= (@?= [(Just "300", "answer")])
  IORef.readIORef edits >>= (@?= [])
  IORef.readIORef updates >>= (@?= ["answer", "answer"])

testEmptyChunkedStreamingReplySendsNothing :: IO ()
testEmptyChunkedStreamingReplySendsNothing = do
  replies <- IORef.newIORef ([] :: [(Maybe MessageId, Text)])
  nextReplyId <- IORef.newIORef (1 :: Integer)
  _ <- runEff $ runPrim $
    Chat.runChatWith
      defaultAgentMockChatDriver
        { agentReply = recordReply replies nextReplyId
        , agentMessageOutPolicy = \_ -> pure (Chat.ChunkedMessage 100)
        } $
        S.mapM_ (const (pure ())) (Chat.streamReplyTo testMessage (S.each [""] $> ()))
  IORef.readIORef replies >>= (@?= [])

testChatStreamingChunksRepliesAndYieldsUpdates :: IO ()
testChatStreamingChunksRepliesAndYieldsUpdates = do
  replies <- IORef.newIORef ([] :: [(Maybe MessageId, Text)])
  updates <- IORef.newIORef ([] :: [(Maybe MessageId, [MessageId], Text)])
  nextReplyId <- IORef.newIORef (1 :: Integer)
  (lastReply, result) <- runEff $ runPrim $
    Chat.runChatWith
      defaultAgentMockChatDriver
        { agentReply = recordReply replies nextReplyId
        , agentMessageOutPolicy = \_ -> pure (Chat.ChunkedMessage 4)
        } $
        S.mapM_
          (\update -> liftIO $ IORef.modifyIORef' updates (<> [(update.responseId, rights update.sentMessageResults, update.answer)]))
          (Chat.streamReplyTo testMessage (S.each ["ab", "cd", "ef"] $> ("abcdef" :: Text)))
  let responseId = lastReply.responseId
  responseId @?= Just "1"
  result @?= "abcdef"
  IORef.readIORef replies >>= (@?= [(Just "300", "abcd"), (Just "1", "ef")])
  IORef.readIORef updates >>= (@?= [(Nothing, [], "ab"), (Just "1", ["1"], "abcd"), (Just "1", [], "abcdef"), (Just "1", ["2"], "abcdef")])

testEditableSegmentedRepliesOpenNewTail :: IO ()
testEditableSegmentedRepliesOpenNewTail = do
  replies <- IORef.newIORef ([] :: [(Maybe MessageId, Text)])
  edits <- IORef.newIORef ([] :: [(MessageId, Text)])
  updates <- IORef.newIORef ([] :: [(Maybe MessageId, [MessageId], Text)])
  nextReplyId <- IORef.newIORef (1 :: Integer)
  (lastReply, result) <- runEff $ runPrim $
    Chat.runChatWith
      defaultAgentMockChatDriver
        { agentReply = recordReply replies nextReplyId
        , agentEditMessage = recordEdit edits
        , agentMessageOutPolicy = \_ -> pure (Chat.EditableMessage 2 100)
        } $
        S.mapM_
          (\update -> liftIO $ IORef.modifyIORef' updates (<> [(update.responseId, rights update.sentMessageResults, update.answer)]))
          ( Chat.streamMultipleRepliesTo
              testMessage
              (S.breaks Text.null (S.each ["ab", "", "cd", "ef"] $> ("cdef" :: Text)))
          )
  let responseId = lastReply.responseId
  responseId @?= Just "2"
  result @?= "cdef"
  IORef.readIORef replies >>= (@?= [(Just "300", "ab"), (Just "300", "cd")])
  IORef.readIORef edits >>= (@?= [("2", "cdef")])
  IORef.readIORef updates >>= (@?= [(Just "1", ["1"], "ab"), (Just "1", [], "ab"), (Just "2", ["2"], "cd"), (Just "2", [], "cdef"), (Just "2", [], "cdef")])

testSegmentedRepliesFlushFinalOpenSegment :: IO ()
testSegmentedRepliesFlushFinalOpenSegment = do
  replies <- IORef.newIORef ([] :: [(Maybe MessageId, Text)])
  updates <- IORef.newIORef ([] :: [(Maybe MessageId, [MessageId], Text)])
  nextReplyId <- IORef.newIORef (1 :: Integer)
  (lastReply, result) <- runEff $ runPrim $
    Chat.runChatWith
      defaultAgentMockChatDriver
        { agentReply = recordReply replies nextReplyId
        , agentMessageOutPolicy = \_ -> pure (Chat.ChunkedMessage 100)
        } $
        S.mapM_
          (\update -> liftIO $ IORef.modifyIORef' updates (<> [(update.responseId, rights update.sentMessageResults, update.answer)]))
          ( Chat.streamMultipleRepliesTo
              testMessage
              (S.breaks Text.null (S.each ["last ", "segment"] $> ("last segment" :: Text)))
          )
  let responseId = lastReply.responseId
  responseId @?= Just "1"
  result @?= "last segment"
  IORef.readIORef replies >>= (@?= [(Just "300", "last segment")])
  IORef.readIORef updates >>= (@?= [(Nothing, [], "last "), (Nothing, [], "last segment"), (Just "1", ["1"], "last segment")])

testEditableChatStreamingSplitsLongReplies :: IO ()
testEditableChatStreamingSplitsLongReplies = do
  replies <- IORef.newIORef ([] :: [(Maybe MessageId, Text)])
  edits <- IORef.newIORef ([] :: [(MessageId, Text)])
  updates <- IORef.newIORef ([] :: [(Maybe MessageId, [MessageId], Text)])
  nextReplyId <- IORef.newIORef (1 :: Integer)
  (lastReply, result) <- runEff $ runPrim $
    Chat.runChatWith
      defaultAgentMockChatDriver
        { agentReply = recordReply replies nextReplyId
        , agentEditMessage = recordEdit edits
        , agentMessageOutPolicy = \_ -> pure (Chat.EditableMessage 2 4)
        } $
        S.mapM_
          (\update -> liftIO $ IORef.modifyIORef' updates (<> [(update.responseId, rights update.sentMessageResults, update.answer)]))
          (Chat.streamReplyTo testMessage (S.each ["ab", "cd", "ef", "gh", "ij", "kl"] $> ("abcdefghijkl" :: Text)))
  let responseId = lastReply.responseId
  responseId @?= Just "1"
  result @?= "abcdefghijkl"
  IORef.readIORef replies >>= (@?= [(Just "300", "ab"), (Just "1", "efgh"), (Just "2", "ijkl")])
  IORef.readIORef edits >>= (@?= [("1", "abcd")])
  IORef.readIORef updates >>= (@?= [(Just "1", ["1"], "ab"), (Just "1", [], "abcd"), (Just "1", [], "abcdef"), (Just "1", [], "abcdefgh"), (Just "1", [], "abcdefghij"), (Just "1", [], "abcdefghijkl"), (Just "1", ["2", "3"], "abcdefghijkl")])

testChunkedActiveThreadAliasesEverySentReply :: IO ()
testChunkedActiveThreadAliasesEverySentReply = runEff $ runConcurrent $ runPrim $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" $ Media.runMediaPassthrough do
  store <- newThreadStore
  cancelled <- liftIO (IORef.newIORef [])
  let baseTranscript = startWithUser "hello"
      partialTranscript = appendAssistant "partial answer" baseTranscript
      resource = Concurrency.Handle (Concurrency.Id 1)
      cancel handleId = do
        liftIO $ IORef.modifyIORef' cancelled (handleId :)
        pure True
  active <- fromMaybe (error "expected active thread") <$> rememberActiveThread store "test-run" Nothing (Just (messageKey 1)) testMessage "hello" resource baseTranscript
  addActiveThreadMessage store active (messageKey 2)
  updateActiveThread active partialTranscript
  halted <- haltThread store cancel (messageKey 2)
  firstLookup <- lookupThreadTranscript store (messageKey 1)
  secondLookup <- lookupThreadTranscript store (messageKey 2)
  cancelledResources <- liftIO (IORef.readIORef cancelled)
  liftIO do
    halted @?= True
    cancelledResources @?= [Concurrency.Id 1]
    assertBool "finished user aliases should not become continuation points" (isNothing firstLookup)
    (show secondLookup :: String) @?= show (Just partialTranscript)

testHaltCommandCancelsCurrentThreadMessage :: IO ()
testHaltCommandCancelsCurrentThreadMessage = runEff $ runConcurrent $ runPrim $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" $ Media.runMediaPassthrough do
  store <- newThreadStore
  cancelled <- liftIO (IORef.newIORef [])
  let baseTranscript = startWithUser "hello"
      partialTranscript = appendAssistant "partial answer" baseTranscript
      activeHandle = Concurrency.Handle (Concurrency.Id 1)
      cancel handleId = do
        liftIO $ IORef.modifyIORef' cancelled (handleId :)
        pure True
      haltMessage = testMessage{text = "!halt", messageId = Just (integerMessageId 2), replyToMessageId = Nothing}
  active <- fromMaybe (error "expected active thread") <$> rememberActiveThread store "test-run" Nothing (Just (messageKey 1)) testMessage "hello" activeHandle baseTranscript
  addActiveThreadMessage store active (messageKey 2)
  updateActiveThread active partialTranscript
  halted <- haltThreadForMessage store cancel haltMessage
  currentLookup <- lookupThreadTranscript store (messageKey 2)
  cancelledHandles <- liftIO (IORef.readIORef cancelled)
  liftIO do
    halted @?= True
    cancelledHandles @?= [Concurrency.Id 1]
    (show currentLookup :: String) @?= show (Just partialTranscript)

testDeletingBotReplyHaltsActiveRun :: IO ()
testDeletingBotReplyHaltsActiveRun = do
  answers <- IORef.newIORef []
  remaining <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    store <- newThreadStore
    let triggerKey = messageKey 1
        replyKey = messageKey 2
        deleted = testMessage
          { eventKind = IncomingMessageDeleted
          , messageId = Just (integerMessageId 2)
          , text = ""
          }
    active <- fromMaybe (error "expected active thread") <$> rememberActiveThread store "deleted-run" Nothing (Just triggerKey) testMessage "hello" (Concurrency.Handle (Concurrency.Id 999)) (startWithUser "hello")
    addActiveThreadMessage store active replyKey
    runHandlers (askHandlers Agent.defaultToolConfig AgentTools.defaultTools askHandlerConfig store) deleted
    lookupActiveThreadRunId store triggerKey
  remaining @?= Nothing

testHaltCommandPrefersRepliedThreadMessage :: IO ()
testHaltCommandPrefersRepliedThreadMessage = runEff $ runConcurrent $ runPrim $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" $ Media.runMediaPassthrough do
  store <- newThreadStore
  cancelled <- liftIO (IORef.newIORef [])
  let transcript = startWithUser "hello"
      repliedHandle = Concurrency.Handle (Concurrency.Id 1)
      currentHandle = Concurrency.Handle (Concurrency.Id 2)
      cancel handleId = do
        liftIO $ IORef.modifyIORef' cancelled (handleId :)
        pure True
      haltMessage = testMessage{text = "!halt", messageId = Just (integerMessageId 2), replyToMessageId = Just (integerMessageId 1)}
  void $ rememberActiveThread store "replied-run" Nothing (Just (messageKey 1)) testMessage "hello" repliedHandle transcript
  void $ rememberActiveThread store "current-run" Nothing (Just (messageKey 2)) testMessage "hello" currentHandle transcript
  halted <- haltThreadForMessage store cancel haltMessage
  currentStillHalted <- haltThread store cancel (messageKey 2)
  cancelledHandles <- liftIO (IORef.readIORef cancelled)
  liftIO do
    halted @?= True
    currentStillHalted @?= True
    cancelledHandles @?= [Concurrency.Id 2, Concurrency.Id 1]

testHaltCommandRequiresOwnerOrSuperuser :: IO ()
testHaltCommandRequiresOwnerOrSuperuser = runEff $ runConcurrent $ runPrim $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" $ Media.runMediaPassthrough do
  store <- newThreadStore
  cancelled <- liftIO (IORef.newIORef [])
  let transcript = startWithUser "hello"
      cancel handleId = liftIO (IORef.modifyIORef' cancelled (handleId :)) $> True
      outsider = testMessage{senderId = Just "other", replyToMessageId = Just (integerMessageId 1)}
      superuser = outsider{digest = outsider.digest{senderIsSuperuser = True}}
  void $ rememberActiveThread store "test-run" Nothing (Just (messageKey 1)) testMessage "hello" (Concurrency.Handle (Concurrency.Id 1)) transcript
  outsiderHalted <- haltThreadForMessage store cancel outsider
  superuserHalted <- haltThreadForMessage store cancel superuser
  cancelledHandles <- liftIO (IORef.readIORef cancelled)
  liftIO do
    outsiderHalted @?= False
    superuserHalted @?= True
    cancelledHandles @?= [Concurrency.Id 1]

testActiveThreadWithoutPlatformReply :: IO ()
testActiveThreadWithoutPlatformReply = runEff $ runConcurrent $ runPrim $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" $ Media.runMediaPassthrough do
  store <- newThreadStore
  cancelled <- liftIO (IORef.newIORef [])
  let workerId = Concurrency.Id 1
      cancel handleId = liftIO (IORef.modifyIORef' cancelled (handleId :)) $> True
  active <- rememberActiveThread store "test-run" Nothing Nothing testMessage "run sleep 100 and say nothing" (Concurrency.Handle workerId) (startWithUser "hello")
  listed <- listActiveThreadsForMessage store testMessage
  halted <- haltActiveThreadsForMessage store cancel testMessage [workerId]
  remaining <- listActiveThreadsForMessage store testMessage
  cancelledHandles <- liftIO (IORef.readIORef cancelled)
  liftIO do
    isJust active @? "expected active thread handle"
    listed @?= [ActiveThreadInfo workerId "run sleep 100 and say nothing"]
    halted @?= [workerId]
    remaining @?= []
    cancelledHandles @?= [workerId]

testActiveThreadIdsAreStableAndChatScoped :: IO ()
testActiveThreadIdsAreStableAndChatScoped = runEff $ runConcurrent $ runPrim $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" $ Media.runMediaPassthrough do
  store <- newThreadStore
  cancelled <- liftIO (IORef.newIORef [])
  let otherChat = testMessageInChat 200
      transcript = startWithUser "hello"
      remember message responseId workerId prompt =
        void $ rememberActiveThread store [i|test-run-#{workerId}|] Nothing (Just (threadMessageKey message (integerMessageId responseId))) message prompt (Concurrency.Handle (Concurrency.Id workerId)) transcript
      cancel workerId = liftIO (IORef.modifyIORef' cancelled (<> [workerId])) $> True
  remember testMessage 1 11 "first prompt"
  remember testMessage 2 12 "second prompt"
  remember otherChat 3 13 "other chat"
  let outsider = testMessage{senderId = Just "other"}
      superuser = outsider{digest = outsider.digest{senderIsSuperuser = True}}
  outsiderListed <- listActiveThreadsForMessage store outsider
  outsiderHalted <- haltActiveThreadsForMessage store cancel outsider [Concurrency.Id 11]
  superuserListed <- listActiveThreadsForMessage store superuser
  listed <- listActiveThreadsForMessage store testMessage
  halted <- haltActiveThreadsForMessage store cancel testMessage [Concurrency.Id 12, Concurrency.Id 13]
  remaining <- listActiveThreadsForMessage store testMessage
  otherRemaining <- listActiveThreadsForMessage store otherChat
  cancelledIds <- liftIO (IORef.readIORef cancelled)
  liftIO do
    outsiderListed @?= []
    outsiderHalted @?= []
    superuserListed @?= [ActiveThreadInfo (Concurrency.Id 11) "first prompt", ActiveThreadInfo (Concurrency.Id 12) "second prompt"]
    listed @?= [ActiveThreadInfo (Concurrency.Id 11) "first prompt", ActiveThreadInfo (Concurrency.Id 12) "second prompt"]
    halted @?= [Concurrency.Id 12]
    remaining @?= [ActiveThreadInfo (Concurrency.Id 11) "first prompt"]
    otherRemaining @?= [ActiveThreadInfo (Concurrency.Id 13) "other chat"]
    cancelledIds @?= [Concurrency.Id 12]

testActiveThreadSteeringLifecycle :: IO ()
testActiveThreadSteeringLifecycle = runEff $ runConcurrent $ runPrim $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" $ Media.runMediaPassthrough do
  store <- newThreadStore
  let transcript = startWithUser "hello"
      firstSteer =
        testMessage
          { messageId = Just (integerMessageId 2)
          , replyToMessageId = Just (integerMessageId 1)
          , text = "first"
          }
      secondSteer =
        firstSteer
          { messageId = Just (integerMessageId 3)
          , replyToMessageId = Just (integerMessageId 2)
          , text = "second"
          }
  active <- fromMaybe (error "expected active thread") <$> rememberActiveThread store "test-run" Nothing (Just (messageKey 1)) testMessage "hello" (Concurrency.Handle (Concurrency.Id 1)) transcript
  firstAccepted <- enqueueActiveThreadSteer store firstSteer (inputWithImages firstSteer.text [])
  secondAccepted <- enqueueActiveThreadSteer store secondSteer (inputWithImages secondSteer.text [])
  queued <- drainActiveThreadSteers active
  closed <- completeActiveThreadSteering active
  rejectedAfterClose <- enqueueActiveThreadSteer store secondSteer (inputWithImages "too late" [])
  finishActiveThread store active transcript
  steerAlias <- lookupThreadTranscript store (messageKey 2)
  racing <- fromMaybe (error "expected racing active thread") <$> rememberActiveThread store "racing-run" Nothing (Just (messageKey 10)) testMessage "race" (Concurrency.Handle (Concurrency.Id 2)) transcript
  let racingSteer =
        testMessage
          { messageId = Just (integerMessageId 11)
          , replyToMessageId = Just (integerMessageId 10)
          , text = "race"
          }
  raceResult <- Async.concurrently
    (enqueueActiveThreadSteer store racingSteer (inputWithImages racingSteer.text []))
    (completeActiveThreadSteering racing)
  finishActiveThread store racing transcript
  liftIO do
    firstAccepted @?= True
    secondAccepted @?= True
    map (.text) queued @?= ["first", "second"]
    closed @?= Nothing
    rejectedAfterClose @?= False
    assertBool "finished steer aliases should not become continuation points" (isNothing steerAlias)
    assertBool "enqueue wins with its value, or completion closes before enqueue" $
      raceResult == (True, Just [inputWithImages "race" []])
        || raceResult == (False, Nothing)

testWebFetchMaxUsesLimitsCalls :: IO ()
testWebFetchMaxUsesLimitsCalls = do
  answers <- IORef.newIORef
    [ chatAnswer ""
        [ toolCall "call-1" "fetch_url" (Aeson.object ["url" Aeson..= ("https://example.test/1" :: Text)])
        , toolCall "call-2" "fetch_url" (Aeson.object ["url" Aeson..= ("https://example.test/2" :: Text)])
        ]
    , chatAnswer "done" []
    ]
  fetches <- IORef.newIORef (0 :: Int)
  (answer, _) <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 4 (agentContext{Agent.toolConfig = Agent.defaultToolConfig{Agent.webFetch = True, Agent.webFetchMaxUses = Just 1}}) [fakeWebFetchTool fetches] (startWithUser "fetch twice")
  answer @?= "done"
  IORef.readIORef fetches >>= (@?= 1)

fakeWebFetchTool :: IOE :> es => IORef.IORef Int -> AgentTool.Tool (Eff es)
fakeWebFetchTool fetches =
  AgentTool.withDescription "fake web fetch"
  $ AgentTool.toolWithRunState "fetch_url" AgentTool.noArguments
      (\context -> newUseLimiter context.toolConfig.webFetchMaxUses)
      \checkUseLimit -> do
        raise checkUseLimit >>= \case
          UseLimitReached currentUses ->
            pure (Agent.toolText [i|fetch_url use limit reached for this agent run: #{currentUses}.|])
          UseAllowed -> do
            liftIO $ IORef.modifyIORef' fetches (+ 1)
            pure (Agent.toolText "fetched")

testThreadRepliesKeepSnapshots :: IO ()
testThreadRepliesKeepSnapshots = runEff $ runConcurrent $ runPrim $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" $ Media.runMediaPassthrough do
  store <- newThreadStore
  let firstTranscript = startWithUser "first"
      secondTranscript = appendAssistant "second" firstTranscript
  rememberThreadTranscript store (Just (messageKey 1)) firstTranscript
  rememberThreadTranscriptFrom store (Just (messageKey 1)) (Just (messageKey 2)) secondTranscript
  firstLookup <- lookupThreadTranscript store (messageKey 1)
  secondLookup <- lookupThreadTranscript store (messageKey 2)
  liftIO do
    (show firstLookup :: String) @?= show (Just firstTranscript)
    (show secondLookup :: String) @?= show (Just secondTranscript)

testThreadBranchesDoNotOverwriteSiblings :: IO ()
testThreadBranchesDoNotOverwriteSiblings = runEff $ runConcurrent $ runPrim $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" $ Media.runMediaPassthrough do
  store <- newThreadStore
  let root = appendAssistant "root answer" (startWithUser "root")
      branchA = appendAssistant "A answer" (appendUser "A follow-up" root)
      branchB = appendAssistant "B answer" (appendUser "B follow-up" root)
      branchA2 = appendAssistant "A second answer" (appendUser "A second follow-up" branchA)
  rememberThreadTranscript store (Just (messageKey 1)) root
  rememberThreadTranscriptFrom store (Just (messageKey 1)) (Just (messageKey 2)) branchA
  rememberThreadTranscriptFrom store (Just (messageKey 1)) (Just (messageKey 3)) branchB
  rememberThreadTranscriptFrom store (Just (messageKey 2)) (Just (messageKey 4)) branchA2
  rootLookup <- lookupThreadTranscript store (messageKey 1)
  branchALookup <- lookupThreadTranscript store (messageKey 2)
  branchBLookup <- lookupThreadTranscript store (messageKey 3)
  branchA2Lookup <- lookupThreadTranscript store (messageKey 4)
  liftIO do
    (show rootLookup :: String) @?= show (Just root)
    (show branchALookup :: String) @?= show (Just branchA)
    (show branchBLookup :: String) @?= show (Just branchB)
    (show branchA2Lookup :: String) @?= show (Just branchA2)

testThreadLookupIsScopedByChat :: IO ()
testThreadLookupIsScopedByChat = runEff $ runConcurrent $ runPrim $ runTestLog $ StorageSQLite.runStorageSQLitePath ":memory:" $ Media.runMediaPassthrough do
  store <- newThreadStore
  let chatA = testMessageInChat 100
      chatB = testMessageInChat 200
      keyA = threadMessageKey chatA
      keyB = threadMessageKey chatB
      transcriptA = appendAssistant "answer A" (startWithUser "from chat A")
      transcriptB = appendAssistant "answer B" (startWithUser "from chat B")
  rememberThreadTranscript store (Just (keyA "1")) transcriptA
  rememberThreadTranscript store (Just (keyB "1")) transcriptB
  lookupA <- lookupThreadTranscript store (keyA "1")
  lookupB <- lookupThreadTranscript store (keyB "1")
  liftIO do
    (show lookupA :: String) @?= show (Just transcriptA)
    (show lookupB :: String) @?= show (Just transcriptB)

testThreadBranchesPersistThroughSQLiteReload :: IO ()
testThreadBranchesPersistThroughSQLiteReload =
  withSQLiteTempPath "thread-branches" \path -> runEff $ runConcurrent $ runPrim $ runTestLog do
    StorageSQLite.runStorageSQLitePath path $ Media.runMediaPassthrough do
      store <- newThreadStore
      let root = appendAssistant "root answer" (startWithUser "root")
          branchA = appendAssistant "A answer" (appendUser "A follow-up" root)
          branchB = appendAssistant "B answer" (appendUser "B follow-up" root)
      rememberThreadTranscript store (Just (messageKey 1)) root
      rememberThreadTranscriptFrom store (Just (messageKey 1)) (Just (messageKey 2)) branchA
      rememberThreadTranscriptFrom store (Just (messageKey 1)) (Just (messageKey 3)) branchB

      reloaded <- newThreadStore
      branchAAfterReload <- lookupThreadTranscript reloaded (messageKey 2)
      branchBAfterReload <- lookupThreadTranscript reloaded (messageKey 3)
      let branchA2 = appendAssistant "A second answer" (appendUser "A second follow-up" branchA)
      rememberThreadTranscriptFrom reloaded (Just (messageKey 2)) (Just (messageKey 4)) branchA2
      rows <- loadThreadRows
      branchA2AfterReload <- lookupThreadTranscript reloaded (messageKey 4)

      liftIO do
        (show branchAAfterReload :: String) @?= show (Just branchA)
        (show branchBAfterReload :: String) @?= show (Just branchB)
        (show branchA2AfterReload :: String) @?= show (Just branchA2)
        map rowMessageId rows @?= ["1", "2", "3", "4"]
        map rowParentMessageId rows @?= [Nothing, Just "1", Just "1", Just "2"]
        map payloadMessageCount rows @?= [2, 2, 2, 2]
        assertBool "all nodes in the reloaded tree keep the same thread storage id" (sameThreadStorageIds rows)

testConcurrentThreadStoresAllocateDistinctIds :: IO ()
testConcurrentThreadStoresAllocateDistinctIds =
  withSQLiteTempPath "concurrent-thread-ids" \path -> runEff $ runConcurrent $ runPrim $ runTestLog $
    StorageSQLite.runStorageSQLitePath path do
      firstStore <- newThreadStore
      secondStore <- newThreadStore
      void $ Async.concurrently
        (rememberThreadTranscript firstStore (Just (messageKey 1)) (startWithUser "first"))
        (rememberThreadTranscript secondStore (Just (messageKey 2)) (startWithUser "second"))
      ids <- map (.threadStorageId) <$> loadThreadRows
      liftIO $ case ids of
        [Just firstId, Just secondId] ->
          assertBool "concurrent root threads should have distinct storage ids" (firstId /= secondId)
        _ ->
          assertFailure [i|expected two persisted thread ids, got #{show ids :: String}|]

testThreadCacheMissLoadsEvictedParent :: IO ()
testThreadCacheMissLoadsEvictedParent =
  withSQLiteTempPath "thread-cache-miss" \path -> runEff $ runConcurrent $ runPrim $ runTestLog do
    StorageSQLite.runStorageSQLitePath path $ Media.runMediaPassthrough do
      store <- newThreadStore
      let root = appendAssistant "root answer" (startWithUser "root")
          child = appendAssistant "child answer" (appendUser "child follow-up" root)
      rememberThreadTranscript store (Just (messageKey 1)) root
      for_ [1000..1512] \messageId ->
        rememberThreadTranscript store (Just (messageKey messageId)) (startWithUser [i|filler #{messageId}|])
      rememberThreadTranscriptFrom store (Just (messageKey 1)) (Just (messageKey 2)) child
      rootLookup <- lookupThreadTranscript store (messageKey 1)
      childLookup <- lookupThreadTranscript store (messageKey 2)
      rows <- loadThreadRows
      let childRow = find ((== "2") . rowMessageId) rows
      liftIO do
        (show rootLookup :: String) @?= show (Just root)
        (show childLookup :: String) @?= show (Just child)
        (rowParentMessageId =<< childRow) @?= Just "1"
        (payloadMessageCount <$> childRow) @?= Just 2

testThreadStorageOmitsLargeToolResults :: IO ()
testThreadStorageOmitsLargeToolResults =
  withSQLiteTempPath "thread-large-tool-result" \dbPath ->
    withTempDir "thread-large-tool-result-media" \dir -> do
      let cfg = MediaConfig.defaultConfig{MediaConfig.cacheDir = dir </> "cache"}
          result = "<!doctype html><html><body>" <> Text.replicate 5000 "x" <> "</body></html>"
          resultBytes = TextEncoding.encodeUtf8 result
          answers = [chatAnswer "" [toolCall "call-1" "large_tool" (Aeson.object [])], chatAnswer "done" []]
      answerRef <- IORef.newIORef answers
      let runStack =
            runFileSystem
              . runProcess
              . runFail
              . runConcurrent
              . runPrim
              . runTestLog
              . StorageSQLite.runStorageSQLitePath dbPath
              . ConcurrencyManager.runConcurrencyManager
              . ResourceManager.runResourceManager
              . HTTP.runHTTP
              . runTimeout
              . MediaInterpreter.runMedia cfg
      runResult <- runEff $ runStack do
        store <- newThreadStore
        (_answer, transcript) <- LLMTest.runLLMWith
          (\_ -> S.yield "unused text stream answer" $> "unused text stream answer")
          (\_ _ -> S.yield "unused image answer" $> "unused image answer")
          (\_ _ _ _ -> S.yield "unused image edit answer" $> "unused image edit answer")
          (\_ _ -> S.yield "unused audio answer" $> "unused audio answer")
          (\_ _ -> do
              answer <- liftIO (popAnswer answerRef)
              case answer of
                LLM.ChatFinalAnswer{content} ->
                  S.yield content
                LLM.ChatToolRequest{content}
                  | Text.null content -> pure ()
                  | otherwise -> S.yield content
              pure answer)
          $
            AgentAudit.runAgentAudit $
              Chat.runChatWith NoopChatDriver do
                agentRun <- startTestRuntime 4 agentContext [largeResultTool result]
                outputs S.:> agentResult <- S.toList (Agent.agentStream (Agent.defaultRuntime AgentAudit.agentAuditObserver 1000000 agentRun) (startWithUser "fetch"))
                pure (agentOutputText outputs, agentResult.transcript)
        rememberThreadTranscript store (Just (messageKey 1)) transcript
        loaded <- lookupThreadTranscript store (messageKey 1)
        storedRows <- loadThreadRows
        mediaFiles <- Media.listMediaFiles
        pure (loaded, storedRows, mediaFiles)
      (cachedLookup, rows, files) <- either assertFailure pure runResult
      case cachedLookup of
        Just loaded ->
          assertBool "cached lookup should contain omitted marker" ("[tool result omitted;" `Text.isInfixOf` Text.unlines (toolOutputs loaded))
        Nothing ->
          assertFailure "expected cached thread"
      case rows of
        [row] -> do
          contents <- decodeStoredToolContents row
          case contents of
            [stored] -> do
              assertBool "large thread tool result is replaced by omitted marker" ("[tool result omitted;" `Text.isPrefixOf` stored)
              assertBool "thread marker keeps inferred HTML MIME" ("mime=text/html; charset=utf-8" `Text.isInfixOf` stored)
              assertBool "thread marker points to media cache" ("media_id=mf_" `Text.isInfixOf` stored)
              assertBool "thread marker keeps a preview" ("preview=\"<!doctype html>" `Text.isInfixOf` stored)
              assertBool "thread row should not retain the full result tail" (not ("</body></html>" `Text.isInfixOf` stored))
            other ->
              assertFailure [i|expected one stored tool result, got #{length other}|]
        other ->
          assertFailure [i|expected one thread row, got #{length other}|]
      case files of
        [file] -> do
          file.mimeType @?= "text/html; charset=utf-8"
          file.size @?= StrictByteString.length resultBytes
        other ->
          assertFailure [i|expected one cached result file, got #{length other}|]

largeResultTool :: Text -> AgentTool.Tool (Eff es)
largeResultTool result =
  AgentTool.withDescription "fake large result"
  $ AgentTool.tool "large_tool" AgentTool.noArguments
      (pure (Agent.toolText result))

testTranscriptOmitsBase64GeneratedImageContext :: IO ()
testTranscriptOmitsBase64GeneratedImageContext = do
  let base64Image = "data:image/png;base64,AAAA"
      transcript = appendAssistant (ReplyBody.imageDirective base64Image) (startWithUser "draw")
      encoded = TextEncoding.decodeUtf8 (LazyByteString.toStrict (Aeson.encode transcript))
  imageContextUrls transcript @?= []
  assertBool "transcript should not retain base64 image payloads" (not (base64Image `Text.isInfixOf` encoded))
  assertBool "transcript should keep a small generated-image marker" ("Generated image." `Text.isInfixOf` encoded)

testLLMRequestOmitsBase64GeneratedImageContext :: IO ()
testLLMRequestOmitsBase64GeneratedImageContext = do
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  answers <- IORef.newIORef [chatAnswer "ok" []]
  let base64Image = "data:image/png;base64," <> Text.replicate 160 "A"
      transcript =
        appendUser
          "what did you draw?"
          (appendAssistant (ReplyBody.imageDirective base64Image) (startWithUser "draw"))
  _ <- runAgentCapturingMessages captured answers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 1 agentContext AgentTools.defaultTools transcript
  requests <- IORef.readIORef captured
  let encoded = jsonText requests
  assertBool "captured LLM request should not contain generated image base64" (not (base64Image `Text.isInfixOf` encoded))
  assertBool "captured LLM request should retain generated-image marker" ("Generated image." `Text.isInfixOf` encoded)

testTranscriptJsonRemainsListCompatible :: IO ()
testTranscriptJsonRemainsListCompatible = do
  let transcript = appendAssistant "answer" (appendUser "follow-up" (startWithUser "hello"))
      encoded = Aeson.encode transcript
      decoded = Aeson.eitherDecode encoded :: Either String Transcript
      encodedValue = Aeson.eitherDecode encoded :: Either String Aeson.Value
  case decoded of
    Left err ->
      assertFailure err
    Right roundTripped ->
      (show roundTripped :: String) @?= show transcript
  encodedValue @?=
    Right (Aeson.object ["messages" Aeson..= Foldable.toList transcript.messages])

testMemoryToolManagesCurrentSenderMemory :: IO ()
testMemoryToolManagesCurrentSenderMemory = withMemoryTempDir \dir -> do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "sender_memory" (Aeson.object ["action" Aeson..= ("replace" :: Text), "memory" Aeson..= ("Prefers concise Chinese answers." :: Text)])]
    , chatAnswer "" [toolCall "call-2" "sender_memory" (Aeson.object ["action" Aeson..= ("view" :: Text)])]
    , chatAnswer "" [toolCall "call-3" "sender_memory" (Aeson.object ["action" Aeson..= ("clear" :: Text)])]
    , chatAnswer "done" []
    ]
  (answer, _) <- runAgentWithMemory (MemoryStore.MemoryConfig dir) answers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 8 agentContext AgentTools.defaultTools (startWithEnabledTools ["work"] "remember this")
  answer @?= "done"
  exists <- doesFileExist (dir </> "telegram" </> "sender" </> "200.md")
  exists @?= False
  doesDirectoryExist (dir </> ".git") >>= (@?= True)
  commitSubjects <- Process.readProcess "git" ["-C", dir, "log", "--format=%s"] ""
  Text.lines (Text.pack commitSubjects) @?= ["Update memory", "Update memory", "Initialize memory"]

testMemoryToolManagesCurrentChatMemory :: IO ()
testMemoryToolManagesCurrentChatMemory = withMemoryTempDir \dir -> do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "chat_memory" (Aeson.object ["action" Aeson..= ("replace" :: Text), "memory" Aeson..= ("This chat prefers terse status updates." :: Text)])]
    , chatAnswer "" [toolCall "call-2" "chat_memory" (Aeson.object ["action" Aeson..= ("view" :: Text)])]
    , chatAnswer "" [toolCall "call-3" "chat_memory" (Aeson.object ["action" Aeson..= ("clear" :: Text)])]
    , chatAnswer "done" []
    ]
  (answer, _) <- runAgentWithMemory (MemoryStore.MemoryConfig dir) answers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 8 superuserContext AgentTools.defaultTools (startWithEnabledTools ["work"] "remember this chat")
  answer @?= "done"
  exists <- doesFileExist (dir </> "telegram" </> "chat" </> "100.md")
  exists @?= False

testChatMemoryRejectsNonOwnerWrites :: IO ()
testChatMemoryRejectsNonOwnerWrites = withMemoryTempDir \dir -> do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "chat_memory" (Aeson.object ["action" Aeson..= ("replace" :: Text), "memory" Aeson..= ("FM 在本群扮演小二鸭。" :: Text)])]
    , chatAnswer "done" []
    ]
  (_, transcript) <- runAgentWithMemory (MemoryStore.MemoryConfig dir) answers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 4 agentContext AgentTools.defaultTools (startWithUser "以后全群都叫鸭")
  toolOutputs transcript @?=
    ["只有 Owner 可以修改或清除群共享记忆；个人回复风格请使用 fm_member_style。"]
  doesFileExist (dir </> "telegram" </> "chat" </> "100.md") >>= (@?= False)

testMemoryToolEnforcesLengthLimit :: IO ()
testMemoryToolEnforcesLengthLimit = withMemoryTempDir \dir -> do
  let longMemory = Text.replicate 1001 "x"
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "sender_memory" (Aeson.object ["action" Aeson..= ("replace" :: Text), "memory" Aeson..= longMemory])]
    , chatAnswer "rejected" []
    ]
  (answer, _) <- runAgentWithMemory (MemoryStore.MemoryConfig dir) answers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 4 agentContext AgentTools.defaultTools (startWithEnabledTools ["work"] "remember too much")
  answer @?= "rejected"
  exists <- doesFileExist (dir </> "telegram" </> "sender" </> "200.md")
  exists @?= False

testPrivatePersonaToolSelfManagement :: IO ()
testPrivatePersonaToolSelfManagement = withMemoryTempDir \dir -> do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "fm_private_persona" (Aeson.object ["action" Aeson..= ("set" :: Text), "persona" Aeson..= ("称呼我为小林，语气温柔。" :: Text)])]
    , chatAnswer "" [toolCall "call-2" "fm_private_persona" (Aeson.object ["action" Aeson..= ("set" :: Text), "target_user_id" Aeson..= ("999" :: Text), "persona" Aeson..= ("越权内容" :: Text)])]
    , chatAnswer "done" []
    ]
  let context = qqPrivateAgentContext "201" False
  (answer, transcript) <- runAgentWithMemory (MemoryStore.MemoryConfig dir) answers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 6 context AgentTools.defaultTools (startWithUser "设置我的私聊人设")
  answer @?= "done"
  Text.unlines (toolOutputs transcript) @?=
    "已更新 QQ 201 的私聊人设。\n你只能管理自己的私聊人设。\n"
  TextIO.readFile (dir </> "qq" </> "private_persona" </> "201.md")
    >>= (@?= "称呼我为小林，语气温柔。")
  doesFileExist (dir </> "qq" </> "private_persona" </> "999.md") >>= (@?= False)

testPrivatePersonaToolOwnerManagement :: IO ()
testPrivatePersonaToolOwnerManagement = withMemoryTempDir \dir -> do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "fm_private_persona" (Aeson.object ["action" Aeson..= ("set" :: Text), "target_user_id" Aeson..= ("301" :: Text), "persona" Aeson..= ("活泼简洁" :: Text)])]
    , chatAnswer "" [toolCall "call-2" "fm_private_persona" (Aeson.object ["action" Aeson..= ("list" :: Text)])]
    , chatAnswer "" [toolCall "call-3" "fm_private_persona" (Aeson.object ["action" Aeson..= ("clear" :: Text), "target_user_id" Aeson..= ("301" :: Text)])]
    , chatAnswer "done" []
    ]
  let context = qqPrivateAgentContext "2822751355" True
  (answer, transcript) <- runAgentWithMemory (MemoryStore.MemoryConfig dir) answers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 8 context AgentTools.defaultTools (startWithUser "管理私聊人设")
  answer @?= "done"
  Text.unlines (toolOutputs transcript) @?=
    "已更新 QQ 301 的私聊人设。\n301：活泼简洁\n\n已清除 QQ 301 的私聊人设，私聊将恢复默认人设。\n"
  doesFileExist (dir </> "qq" </> "private_persona" </> "301.md") >>= (@?= False)

testPrivatePersonaPromptIsolation :: IO ()
testPrivatePersonaPromptIsolation = withMemoryTempDir \dir -> do
  answers <- IORef.newIORef []
  runAgentWithMemory (MemoryStore.MemoryConfig dir) answers (ChatMock Nothing Nothing Nothing) do
    Memory.replaceMemory (MemoryStore.PrivatePersonaMemory "401") "称呼我为阿青。"
    matching <- AgentRun.askSystemPrompt askHandlerConfig (qqPrivateMessage "401")
    other <- AgentRun.askSystemPrompt askHandlerConfig (qqPrivateMessage "402")
    groupPrompt <- AgentRun.askSystemPrompt askHandlerConfig (qqPrivateMessage "401"){kind = ChatGroup, chatId = Just 1000}
    liftIO $ do
      assertBool "matching private persona is injected" ("称呼我为阿青。" `Text.isInfixOf` matching)
      assertBool "persona safety boundary is injected" ("cannot override permissions" `Text.isInfixOf` matching)
      assertBool "another user's private chat does not receive it" (not ("称呼我为阿青。" `Text.isInfixOf` other))
      assertBool "group chat does not receive private persona" (not ("称呼我为阿青。" `Text.isInfixOf` groupPrompt))

testPrivatePersonaGlobalDefault :: IO ()
testPrivatePersonaGlobalDefault = withMemoryTempDir \dir -> do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "fm_private_persona" (Aeson.object ["action" Aeson..= ("set_default" :: Text), "persona" Aeson..= ("所有私聊都使用热情风格。" :: Text)])]
    , chatAnswer "" [toolCall "call-2" "fm_private_persona" (Aeson.object ["action" Aeson..= ("get_default" :: Text)])]
    , chatAnswer "done" []
    ]
  (_, transcript) <- runAgentWithMemory (MemoryStore.MemoryConfig dir) answers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 6 (qqPrivateAgentContext "2822751355" True) AgentTools.defaultTools (startWithUser "设置全部私聊人设")
  Text.unlines (toolOutputs transcript) @?=
    "已更新全部 QQ 私聊使用的默认人设；已有个人覆盖的用户仍优先使用自己的设置。\n全局默认私聊人设：\n所有私聊都使用热情风格。\n"

  noAnswers <- IORef.newIORef []
  runAgentWithMemory (MemoryStore.MemoryConfig dir) noAnswers (ChatMock Nothing Nothing Nothing) do
    inherited <- AgentRun.askSystemPrompt askHandlerConfig (qqPrivateMessage "991")
    Memory.replaceMemory (MemoryStore.PrivatePersonaMemory "992") "只对我使用安静风格。"
    overridden <- AgentRun.askSystemPrompt askHandlerConfig (qqPrivateMessage "992")
    liftIO $ do
      assertBool "new user inherits global private persona" ("所有私聊都使用热情风格。" `Text.isInfixOf` inherited)
      assertBool "user override is loaded" ("只对我使用安静风格。" `Text.isInfixOf` overridden)
      assertBool "user override wins over global default" (not ("所有私聊都使用热情风格。" `Text.isInfixOf` overridden))

qqPrivateAgentContext :: Text -> Bool -> Agent.Context
qqPrivateAgentContext userId isOwner =
  agentContext
    { Agent.message = qqPrivateMessage userId
    , Agent.input = inputWithImages "" []
    , Agent.superuser = isOwner
    }

qqPrivateMessage :: Text -> IncomingMessage
qqPrivateMessage userId =
  testMessage
    { platform = PlatformQQ
    , kind = ChatPrivate
    , chatId = readMaybe (Text.unpack userId)
    , senderId = Just userId
    , digest = emptyMessageDigest{senderIsAllowed = True}
    }

testQqPrivateAgentReplyIsStandalone :: IO ()
testQqPrivateAgentReplyIsStandalone = do
  let original = (qqPrivateMessage "501")
        { messageId = Just "9001"
        , replyToMessageId = Just "8999"
        , raw = Aeson.object ["post_type" Aeson..= ("message" :: Text)]
        }
      standalone = FMBridge.fmStandaloneMessage original
  standalone.messageId @?= Nothing
  standalone.replyToMessageId @?= Nothing
  standalone.senderId @?= Just "501"
  standalone.kind @?= ChatPrivate

testGroupPersonaToolPermissions :: IO ()
testGroupPersonaToolPermissions = withMemoryTempDir \dir -> do
  ownerAnswers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "fm_group_persona" (Aeson.object ["action" Aeson..= ("set" :: Text), "target_group_id" Aeson..= (601 :: Integer), "persona" Aeson..= ("轻松幽默，回答简洁。" :: Text)])]
    , chatAnswer "" [toolCall "call-2" "fm_group_persona" (Aeson.object ["action" Aeson..= ("list" :: Text)])]
    , chatAnswer "done" []
    ]
  (_, ownerTranscript) <- runAgentWithMemory (MemoryStore.MemoryConfig dir) ownerAnswers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 6 (qqGroupAgentContext 906230260 True) AgentTools.defaultTools (startWithUser "设置群聊人设")
  Text.unlines (toolOutputs ownerTranscript) @?=
    "已更新 QQ 群 601 的群聊人设。\n601：轻松幽默，回答简洁。\n\n"

  memberAnswers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-3" "fm_group_persona" (Aeson.object ["action" Aeson..= ("status" :: Text)])]
    , chatAnswer "" [toolCall "call-4" "fm_group_persona" (Aeson.object ["action" Aeson..= ("set" :: Text), "persona" Aeson..= ("不应写入" :: Text)])]
    , chatAnswer "done" []
    ]
  (_, memberTranscript) <- runAgentWithMemory (MemoryStore.MemoryConfig dir) memberAnswers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 6 (qqGroupAgentContext 601 False) AgentTools.defaultTools (startWithUser "查看并修改群聊人设")
  Text.unlines (toolOutputs memberTranscript) @?=
    "QQ 群 601 的群聊人设：\n轻松幽默，回答简洁。\n只有 Owner 可以修改或跨群查看群聊人设。\n"
  TextIO.readFile (dir </> "qq" </> "group_persona" </> "601.md")
    >>= (@?= "轻松幽默，回答简洁。")

testGroupPersonaPromptIsolation :: IO ()
testGroupPersonaPromptIsolation = withMemoryTempDir \dir -> do
  answers <- IORef.newIORef []
  runAgentWithMemory (MemoryStore.MemoryConfig dir) answers (ChatMock Nothing Nothing Nothing) do
    Memory.replaceMemory (MemoryStore.GroupPersonaMemory 701) "在本群称呼大家为同学。"
    matching <- AgentRun.askSystemPrompt askHandlerConfig (qqGroupMessage 701)
    other <- AgentRun.askSystemPrompt askHandlerConfig (qqGroupMessage 702)
    privatePrompt <- AgentRun.askSystemPrompt askHandlerConfig (qqPrivateMessage "701")
    liftIO $ do
      assertBool "matching group persona is injected" ("在本群称呼大家为同学。" `Text.isInfixOf` matching)
      assertBool "another group does not receive it" (not ("在本群称呼大家为同学。" `Text.isInfixOf` other))
      assertBool "private chat does not receive group persona" (not ("在本群称呼大家为同学。" `Text.isInfixOf` privatePrompt))

testGroupPersonaGlobalDefault :: IO ()
testGroupPersonaGlobalDefault = withMemoryTempDir \dir -> do
  answers <- IORef.newIORef []
  runAgentWithMemory (MemoryStore.MemoryConfig dir) answers (ChatMock Nothing Nothing Nothing) do
    Memory.replaceMemory MemoryStore.DefaultGroupPersonaMemory "所有群默认轻松幽默。"
    Memory.replaceMemory (MemoryStore.GroupPersonaMemory 801) "本群保持安静简洁。"
    inherited <- AgentRun.askSystemPrompt askHandlerConfig (qqGroupMessage 802)
    overridden <- AgentRun.askSystemPrompt askHandlerConfig (qqGroupMessage 801)
    privatePrompt <- AgentRun.askSystemPrompt askHandlerConfig (qqPrivateMessage "801")
    liftIO $ do
      assertBool "group without override inherits default" ("所有群默认轻松幽默。" `Text.isInfixOf` inherited)
      assertBool "group override wins over default" ("本群保持安静简洁。" `Text.isInfixOf` overridden)
      assertBool "group override omits default" (not ("所有群默认轻松幽默。" `Text.isInfixOf` overridden))
      assertBool "private chat does not receive default group persona" (not ("所有群默认轻松幽默。" `Text.isInfixOf` privatePrompt))

testMemberStyleAcrossGroups :: IO ()
testMemberStyleAcrossGroups = withMemoryTempDir \dir -> do
  answers <- IORef.newIORef []
  runAgentWithMemory (MemoryStore.MemoryConfig dir) answers (ChatMock Nothing Nothing Nothing) do
    Memory.replaceMemory MemoryStore.DefaultGroupPersonaMemory "默认群风格。"
    Memory.replaceMemory (MemoryStore.MemberStyleMemory "1001") "回复我时简洁温柔。"
    matching <- AgentRun.askSystemPrompt askHandlerConfig (qqGroupMessageFrom 901 "1001")
    otherMember <- AgentRun.askSystemPrompt askHandlerConfig (qqGroupMessageFrom 901 "1002")
    otherGroup <- AgentRun.askSystemPrompt askHandlerConfig (qqGroupMessageFrom 902 "1001")
    privatePrompt <- AgentRun.askSystemPrompt askHandlerConfig (qqPrivateMessage "1001")
    liftIO $ do
      assertBool "matching member receives style" ("回复我时简洁温柔。" `Text.isInfixOf` matching)
      assertBool "matching member still receives group persona" ("默认群风格。" `Text.isInfixOf` matching)
      assertBool "another member does not receive style" (not ("回复我时简洁温柔。" `Text.isInfixOf` otherMember))
      assertBool "same member receives style in another group" ("回复我时简洁温柔。" `Text.isInfixOf` otherGroup)
      assertBool "private chat does not receive member style" (not ("回复我时简洁温柔。" `Text.isInfixOf` privatePrompt))

testMemberStyleToolPermissions :: IO ()
testMemberStyleToolPermissions = withMemoryTempDir \dir -> do
  memberAnswers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "fm_member_style" (Aeson.object ["action" Aeson..= ("set" :: Text), "style" Aeson..= ("每句话结尾加喵。" :: Text)])]
    , chatAnswer "" [toolCall "call-2" "fm_member_style" (Aeson.object ["action" Aeson..= ("status" :: Text)])]
    , chatAnswer "" [toolCall "call-3" "fm_member_style" (Aeson.object ["action" Aeson..= ("set" :: Text), "target_user_id" Aeson..= ("1002" :: Text), "style" Aeson..= ("不应写入" :: Text)])]
    , chatAnswer "done" []
    ]
  (_, transcript) <- runAgentWithMemory (MemoryStore.MemoryConfig dir) memberAnswers (ChatMock Nothing Nothing Nothing) do
    let context = (qqGroupAgentContext 901 False)
          { Agent.message = qqGroupMessageFrom 901 "1001" }
    runTestAgent 8 context AgentTools.defaultTools (startWithUser "以后和我说话加喵")
  Text.unlines (toolOutputs transcript) @?=
    "已更新 QQ 1001 的群聊个人回复风格；会在所有 QQ 群生效，但不会影响其他群友或私聊。\nQQ 1001 的群聊个人回复风格：\n每句话结尾加喵。\n你只能管理自己的群聊个人回复风格。\n"
  TextIO.readFile (dir </> "qq" </> "member_style" </> "1001.md")
    >>= (@?= "每句话结尾加喵。")

qqGroupAgentContext :: Integer -> Bool -> Agent.Context
qqGroupAgentContext groupId isOwner =
  agentContext
    { Agent.message = qqGroupMessage groupId
    , Agent.input = inputWithImages "" []
    , Agent.superuser = isOwner
    }

qqGroupMessage :: Integer -> IncomingMessage
qqGroupMessage groupId =
  qqGroupMessageFrom groupId "800"

qqGroupMessageFrom :: Integer -> Text -> IncomingMessage
qqGroupMessageFrom groupId userId =
  testMessage
    { platform = PlatformQQ
    , kind = ChatGroup
    , chatId = Just groupId
    , senderId = Just userId
    , digest = emptyMessageDigest{chatIsAllowed = True, senderIsAllowed = True}
    }

testMemoryUpdateRollsBackOnCommitFailure :: IO ()
testMemoryUpdateRollsBackOnCommitFailure = withMemoryTempDir \dir -> do
  let cfg = MemoryStore.MemoryConfig dir
      scope = MemoryStore.SenderMemory PlatformTelegram "200"
      runStore = runEff . runFileSystem . runProcess
  runStore do
    MemoryStore.initializeMemoryRepo cfg
    MemoryStore.replaceMemory cfg scope "old memory"

  let hook = dir </> ".git" </> "hooks" </> "pre-commit"
  TextIO.writeFile hook "#!/bin/sh\nexit 1\n"
  permissions <- getPermissions hook
  setPermissions hook permissions{executable = True}

  (result, current) <- runStore do
    result <- trySync (MemoryStore.replaceMemory cfg scope "new memory")
    current <- MemoryStore.loadMemory cfg scope
    pure (result, current)
  case result of
    Left _ -> pure ()
    Right () -> assertFailure "memory update unexpectedly succeeded"
  current @?= Just "old memory"
  Process.readProcess "git" ["-C", dir, "status", "--porcelain"] "" >>= (@?= "")

testRunBashCapturesStdoutAndStderr :: IO ()
testRunBashCapturesStdoutAndStderr = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "run_bash" (Aeson.object ["script" Aeson..= ("printf stdout; printf stderr >&2" :: Text), "timeout_seconds" Aeson..= (5 :: Int)])]
    , chatAnswer "done" []
    ]
  (answer, transcript) <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 4 superuserContext AgentTools.defaultTools (startWithEnabledTools ["work"] "run command")
  answer @?= "done"
  let output = Text.unlines (toolOutputs transcript)
  assertBool "stdout is included" ("stdout:\nstdout" `Text.isInfixOf` output)
  assertBool "stderr is included" ("stderr:\nstderr" `Text.isInfixOf` output)
  assertBool "exit code is included" ("exit code: ExitSuccess" `Text.isInfixOf` output)

testRunBashKillsTimedOutProcess :: IO ()
testRunBashKillsTimedOutProcess = do
  answers <- IORef.newIORef
    [ chatAnswer "" [toolCall "call-1" "run_bash" (Aeson.object ["script" Aeson..= ("sleep 2; printf late" :: Text), "timeout_seconds" Aeson..= (1 :: Int)])]
    , chatAnswer "done" []
    ]
  (answer, transcript) <- runAgentWith answers (ChatMock Nothing Nothing Nothing) do
    runTestAgent 4 superuserContext AgentTools.defaultTools (startWithEnabledTools ["work"] "run slow command")
  answer @?= "done"
  let output = Text.unlines (toolOutputs transcript)
  assertBool ("timeout is reported in: " <> Text.unpack output) ("Script timed out after 1 seconds and was killed." `Text.isInfixOf` output)
  assertBool ("post-timeout output is not included in: " <> Text.unpack output) (not ("late" `Text.isInfixOf` output))

testRunBashKillsProcessGroupWhenCancelled :: IO ()
testRunBashKillsProcessGroupWhenCancelled = withTempDir "run-bash-cancel" \dir -> do
  let pidPath = dir </> "child.pid"
      script = [i|sleep 60 & echo $! > #{pidPath}; wait|]
  childPid <- runEff $ runFailIO $ runConcurrent $ runTimeout $ runProcess $ TypedProcess.runTypedProcess do
    bashThread <- Async.async (runBashSafe 30 script)
    waitUntil (liftIO (doesFileExist pidPath))
    pidText <- liftIO (TextIO.readFile pidPath)
    pid <- maybe (liftIO (assertFailure [i|invalid child pid: #{pidText}|])) pure (readMaybe (Text.unpack pidText))
    Async.cancel bashThread
    void (Async.waitCatch bashThread)
    waitUntil (liftIO (not <$> isProcessAlive pid))
    pure pid
  alive <- isProcessAlive childPid
  assertBool [i|child process #{childPid} should be killed when run_bash is cancelled|] (not alive)

isProcessAlive :: Integer -> IO Bool
isProcessAlive pid =
  (signalProcess nullSignal (fromInteger pid) $> True)
    `catchIOError` \_ -> pure False

testLLMResponseTimeoutSummaryIsConcise :: IO ()
testLLMResponseTimeoutSummaryIsConcise = do
  request <- HTTP.parseRequest "https://api.example.test/v1/chat/completions"
  let err = toException (HTTP.HttpExceptionRequest request HTTP.ResponseTimeout)
  LLM.llmExceptionSummary err @?= "HTTP error: ResponseTimeout"

testLLMExceptionSummaryDescribesLLMErrors :: IO ()
testLLMExceptionSummaryDescribesLLMErrors =
  LLM.llmExceptionSummary (toException (LLM.LLMException "OpenAI response was empty: no text output."))
    @?= "LLM error: OpenAI response was empty: no text output."

testLLMStatusErrorSummaryIsConcise :: IO ()
testLLMStatusErrorSummaryIsConcise = do
  err <- expiredQQImageReqException
  let summary = LLM.llmExceptionSummary err
  assertBool "summary includes status" ("HTTP error: 400 Bad Request\n{" `Text.isPrefixOf` summary)
  assertBool "summary includes provider error JSON" ("\"error\"" `Text.isInfixOf` summary)
  assertBool "summary includes provider message" ("Error while downloading https://multimedia.nt.qq.com.cn/download" `Text.isInfixOf` summary)
  assertBool "summary omits the request dump" (not ("responseOriginalRequest" `Text.isInfixOf` summary))

testAgentFailureSummarizesReqHttpErrors :: IO ()
testAgentFailureSummarizesReqHttpErrors = do
  err <- expiredQQImageReqException
  let summary = LLM.llmExceptionSummary err
  let failure = AgentTypes.failureFromException err
  failure.userMessage @?= summary

expiredQQImageReqException :: IO SomeException
expiredQQImageReqException = do
  request <- HTTP.parseRequest "https://api.example.test/v1/chat/completions"
  let response = HTTPInternal.Response
        { HTTPInternal.responseStatus = HTTPStatus.status400
        , HTTPInternal.responseVersion = HTTPVersion.http11
        , HTTPInternal.responseHeaders = []
        , HTTPInternal.responseBody = ()
        , HTTPInternal.responseCookieJar = HTTP.createCookieJar []
        , HTTPInternal.responseClose' = HTTPInternal.ResponseClose (pure ())
        , HTTPInternal.responseOriginalRequest = request
        , HTTPInternal.responseEarlyHints = []
        }
      httpErr = HTTP.HttpExceptionRequest request (HTTP.StatusCodeException response expiredQQImageErrorBody)
  pure (toException (Req.VanillaHttpException httpErr))

expiredQQImageErrorBody :: ByteString
expiredQQImageErrorBody =
  LazyByteString.toStrict $
    Aeson.encode $
      Aeson.object
        [ "error" Aeson..= Text.unlines
            [ "Error: Current provider response failed: {'detail': '{"
            , "  \"error\": {"
            , "    \"message\": \"Error while downloading https://multimedia.nt.qq.com.cn/download?appid=1407&rkey=secret. Upstream status code: 400.\","
            , "    \"param\": \"url\","
            , "    \"code\": \"invalid_value\""
            , "  }"
            , "}'}"
            ]
        ]

withMemoryTempDir :: (FilePath -> IO a) -> IO a
withMemoryTempDir action = do
  withTempDir "memory-test" action

withIsolatedMemoryConfig :: MemoryStore.MemoryConfig -> (MemoryStore.MemoryConfig -> IO a) -> IO a
withIsolatedMemoryConfig cfg action
  | cfg.dir == "/tmp/cosmobot-agent-spec-unused" =
      withMemoryTempDir (action . MemoryStore.MemoryConfig)
  | otherwise = action cfg

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir label action = do
  runEff $ runFileSystem do
    root <- FS.getTemporaryDirectory
    unique <- liftIO (hashUnique <$> newUnique)
    let dir = root </> [i|cosmobot-#{label}-#{unique}|]
    bracket
      (FS.createDirectory dir $> dir)
      FS.removeDirectoryRecursive
      (liftIO . action)

withSQLiteTempPath :: String -> (FilePath -> IO a) -> IO a
withSQLiteTempPath label action =
  withTempDir label \dir ->
    action (dir </> "test.sqlite")

sameThreadStorageIds :: [ThreadRow] -> Bool
sameThreadStorageIds rows =
  case map (.threadStorageId) rows of
    [] ->
      True
    firstId : rest ->
      isJust firstId && all (== firstId) rest

payloadMessageCount :: ThreadRow -> Int
payloadMessageCount row =
  case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 row.messagesJson) :: Either String [LLM.ChatMessage] of
    Left err ->
      error (Text.pack err)
    Right messages ->
      length messages

decodeStoredToolContents :: ThreadRow -> IO [Text]
decodeStoredToolContents row =
  case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 row.messagesJson) :: Either String [LLM.ChatMessage] of
    Left err ->
      assertFailure err
    Right messages ->
      pure
        [ text
        | message <- messages
        , message.role == "tool"
        , Just (LLM.TextContent text) <- [message.content]
        ]

rowMessageId :: ThreadRow -> MessageId
rowMessageId row =
  row.messageKey.messageId

rowParentMessageId :: ThreadRow -> Maybe MessageId
rowParentMessageId row =
  (.messageId) <$> row.parentMessageKey

messageKey :: Integer -> ThreadMessageKey
messageKey =
  threadMessageKey testMessage . integerMessageId

testMessageInChat :: Integer -> IncomingMessage
testMessageInChat chatId =
  IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = testMessage.platform
    , kind = testMessage.kind
    , chatId = Just chatId
    , chatAliases = testMessage.chatAliases
    , digest = testMessage.digest
    , senderId = testMessage.senderId
    , senderUsername = testMessage.senderUsername
    , messageId = testMessage.messageId
    , replyToMessageId = testMessage.replyToMessageId
    , mentions = testMessage.mentions
    , mentionUsernames = testMessage.mentionUsernames
    , imageUrls = testMessage.imageUrls
    , files = testMessage.files
    , text = testMessage.text
    , raw = testMessage.raw
    }

testMessageWithImages :: [Text] -> IncomingMessage
testMessageWithImages imageUrls =
  IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = testMessage.platform
    , kind = testMessage.kind
    , chatId = testMessage.chatId
    , chatAliases = testMessage.chatAliases
    , digest = testMessage.digest
    , senderId = testMessage.senderId
    , senderUsername = testMessage.senderUsername
    , messageId = testMessage.messageId
    , replyToMessageId = testMessage.replyToMessageId
    , mentions = testMessage.mentions
    , mentionUsernames = testMessage.mentionUsernames
    , imageUrls = imageUrls
    , files = testMessage.files
    , text = testMessage.text
    , raw = testMessage.raw
    }

chatLogMessage :: Integer -> Text -> Integer -> Text -> IncomingMessage
chatLogMessage messageId senderId chatId text =
  testMessage
    { messageId = Just (integerMessageId messageId)
    , senderId = Just senderId
    , chatId = Just chatId
    , text = text
    }

toolOutputs :: Transcript -> [Text]
toolOutputs (Transcript messages) =
  [ text
  | message <- Foldable.toList messages
  , message.role == "tool"
  , message.toolCallId /= Just testEnableToolCallId
  , Just (LLM.TextContent text) <- [message.content]
  ]

testEnableToolCallId :: Text
testEnableToolCallId = "test-enable-tools"

startWithEnabledTools :: [Text] -> Text -> Transcript
startWithEnabledTools tags prompt =
  appendUser prompt $
    Transcript
      ( enabledRequest.messages
      <> Seq.fromList
          [ LLM.assistantAnswer (chatAnswer "" [call])
          , LLM.toolResult call "Enabled."
          ]
      )
  where
    enabledRequest = startWithUser "Enable tools for this thread."
    call = toolCall testEnableToolCallId "tool_enable" (Aeson.object ["tags" Aeson..= tags])

transcriptMessagesList :: Transcript -> [LLM.ChatMessage]
transcriptMessagesList (Transcript messages) =
  Foldable.toList messages

showSeparatedOutputs :: [Agent.Output] -> String
showSeparatedOutputs =
  show . map render
  where
    render :: Agent.Output -> (String, Text)
    render = \case
      Agent.ContentDelta text ->
        ("content", text)
      Agent.ToolCallNotification calls ->
        ("tool", Text.intercalate ", " (toList (fmap (.name) calls)))
      Agent.ReplyBoundary ->
        ("boundary", "")

decodeSingleChatLogToolOutput :: Transcript -> IO [Aeson.Value]
decodeSingleChatLogToolOutput transcript =
  case toolOutputs transcript of
    [output] ->
      case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 output) of
        Left err ->
          assertFailure err >> pure []
        Right entries ->
          pure entries
    outputs ->
      assertFailure [i|expected one tool output, got #{length outputs}|] >> pure []

streamAnswerText :: [Agent.Output] -> Text
streamAnswerText =
  Text.strip . foldMap \case
    Agent.ContentDelta text ->
      text
    Agent.ToolCallNotification{} ->
      ""
    Agent.ReplyBoundary ->
      ""

imageContextUrls :: Transcript -> [Text]
imageContextUrls (Transcript messages) =
  [ url
  | message <- Foldable.toList messages
  , message.role == "user"
  , Just (LLM.PartsContent parts) <- [message.content]
  , LLM.ImageUrlPart url <- parts
  ]

requestUserImageUrls :: [LLM.ChatMessage] -> [Text]
requestUserImageUrls messages =
  [ url
  | message <- messages
  , message.role == "user"
  , Just (LLM.PartsContent parts) <- [message.content]
  , LLM.ImageUrlPart url <- parts
  ]

chatMessageTextsByRole :: Text -> [LLM.ChatMessage] -> [Text]
chatMessageTextsByRole role messages =
  [ text
  | message <- messages
  , message.role == role
  , text <- chatMessageTextParts message
  ]

decodedToolResults :: [LLM.ChatMessage] -> [Aeson.Value]
decodedToolResults messages =
  mapMaybe (Aeson.decodeStrict' . TextEncoding.encodeUtf8) (chatMessageTextsByRole "tool" messages)

resumedContinuationValues :: [LLM.ChatMessage] -> [Aeson.Value]
resumedContinuationValues =
  mapMaybe (AesonTypes.parseMaybe (Aeson.withObject "resumed continuation" (Aeson..: AesonKey.fromText "value")))
    . decodedToolResults

chatMessageTextParts :: LLM.ChatMessage -> [Text]
chatMessageTextParts message =
  case message.content of
    Just (LLM.TextContent text) ->
      [text]
    Just (LLM.PartsContent parts) ->
      [text | LLM.TextPart text <- parts]
    Nothing ->
      []

requestRoles :: [LLM.ChatMessage] -> [Text]
requestRoles =
  map (.role)

imageGenerateCall :: LLM.ImageRequestOptions -> LLM.ChatMessage -> ImageGenerateCall
imageGenerateCall options message =
  ImageGenerateCall
    { prompt = messagePromptText message
    , imageRefs = messageImageRefs message
    , options = options
    }

audioGenerateCall :: LLM.AudioRequestOptions -> LLM.ChatMessage -> AudioGenerateCall
audioGenerateCall options message =
  AudioGenerateCall
    { prompt = messagePromptText message
    , options = options
    }

messagePromptText :: LLM.ChatMessage -> Text
messagePromptText message =
  case message.content of
    Just (LLM.TextContent text) ->
      text
    Just (LLM.PartsContent parts) ->
      Text.concat [text | LLM.TextPart text <- parts]
    Nothing ->
      ""

messageImageRefs :: LLM.ChatMessage -> [Text]
messageImageRefs message =
  case message.content of
    Just (LLM.PartsContent parts) ->
      [url | LLM.ImageUrlPart url <- parts]
    _ ->
      []

runAgentWith
  :: IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWith answers chatMock action =
  runAgentWithMemory (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused") answers chatMock action

runAgentWithImageGenerate
  :: IORef.IORef [LLM.ChatAnswer]
  -> IORef.IORef [ImageGenerateCall]
  -> Text
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithImageGenerate answers generateCalls generateAnswer chatMock action = do
  rendered <- IORef.newIORef ([] :: [Text])
  runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEditAndReferenced
    (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused")
    defaultTestSkillsConfig
    rendered
    Nothing
    answers
    chatMock
    Nothing
    (\options messages -> do
        IORef.modifyIORef' generateCalls (<> map (imageGenerateCall options) messages)
        pure generateAnswer)
    (\_ _ _ _ -> pure "unused image edit answer")
    action

runAgentWithImageEdit
  :: IORef.IORef [LLM.ChatAnswer]
  -> IORef.IORef [ImageEditCall]
  -> Text
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithImageEdit answers editCalls editAnswer chatMock action = do
  rendered <- IORef.newIORef ([] :: [Text])
  runAgentWithMemorySkillsAndTypstAndCaptureAndImageEdit
    (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused")
    defaultTestSkillsConfig
    rendered
    Nothing
    answers
    chatMock
    (\options prompt imageRefs maskRef -> do
        IORef.modifyIORef' editCalls (<> [ImageEditCall{prompt, imageRefs, maskRef, options}])
        pure editAnswer)
    action

runAgentWithImageEditAndReferencedMessage
  :: IORef.IORef [LLM.ChatAnswer]
  -> IORef.IORef [ImageEditCall]
  -> Text
  -> Maybe ReferencedMessage
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithImageEditAndReferencedMessage answers editCalls editAnswer referencedMessage chatMock action = do
  rendered <- IORef.newIORef ([] :: [Text])
  runAgentWithMemorySkillsAndTypstAndCaptureAndImageEditAndReferenced
    (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused")
    defaultTestSkillsConfig
    rendered
    Nothing
    answers
    chatMock
    referencedMessage
    (\options prompt imageRefs maskRef -> do
        IORef.modifyIORef' editCalls (<> [ImageEditCall{prompt, imageRefs, maskRef, options}])
        pure editAnswer)
    action

runAgentCapturingMessages
  :: IORef.IORef [[LLM.ChatMessage]]
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentCapturingMessages captured answers chatMock action = do
  rendered <- IORef.newIORef ([] :: [Text])
  runAgentWithMemoryAndTypstAndCapture
    (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused")
    rendered
    (Just captured)
    answers
    chatMock
    action

runAgentCapturingMessagesWithSkills
  :: SkillsStore.SkillsConfig
  -> IORef.IORef [[LLM.ChatMessage]]
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentCapturingMessagesWithSkills skillsCfg captured answers chatMock action = do
  rendered <- IORef.newIORef ([] :: [Text])
  runAgentWithMemorySkillsAndTypstAndCapture
    (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused")
    skillsCfg
    rendered
    (Just captured)
    answers
    chatMock
    action

runAgentWithTypst
  :: IORef.IORef [Text]
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithTypst rendered answers chatMock action =
  runAgentWithMemoryAndTypstAndCapture
    (MemoryStore.MemoryConfig "/tmp/cosmobot-agent-spec-unused")
    rendered
    Nothing
    answers
    chatMock
    action

runAgentWithMemory
  :: MemoryStore.MemoryConfig
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithMemory memoryCfg answers chatMock action = do
  rendered <- IORef.newIORef ([] :: [Text])
  runAgentWithMemoryAndTypst memoryCfg rendered answers chatMock action

runAgentWithMemoryAndTypst
  :: MemoryStore.MemoryConfig
  -> IORef.IORef [Text]
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithMemoryAndTypst memoryCfg rendered answers chatMock action = do
  runAgentWithMemoryAndTypstAndCapture memoryCfg rendered Nothing answers chatMock action

runAgentWithMemoryAndTypstAndCapture
  :: MemoryStore.MemoryConfig
  -> IORef.IORef [Text]
  -> Maybe (IORef.IORef [[LLM.ChatMessage]])
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithMemoryAndTypstAndCapture memoryCfg rendered captured answers chatMock action = do
  runAgentWithMemorySkillsAndTypstAndCapture memoryCfg defaultTestSkillsConfig rendered captured answers chatMock action

runAgentWithMemorySkillsAndTypstAndCapture
  :: MemoryStore.MemoryConfig
  -> SkillsStore.SkillsConfig
  -> IORef.IORef [Text]
  -> Maybe (IORef.IORef [[LLM.ChatMessage]])
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithMemorySkillsAndTypstAndCapture memoryCfg skillsCfg rendered captured answers chatMock action = do
  runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEdit
    memoryCfg
    skillsCfg
    rendered
    captured
    answers
    chatMock
    (\_ _ -> pure "unused image answer")
    (\_ _ _ _ -> pure "unused image edit answer")
    action

runAgentWithMemorySkillsAndTypstAndCaptureAndImageEdit
  :: MemoryStore.MemoryConfig
  -> SkillsStore.SkillsConfig
  -> IORef.IORef [Text]
  -> Maybe (IORef.IORef [[LLM.ChatMessage]])
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> (LLM.ImageRequestOptions -> Text -> [Text] -> Maybe Text -> IO Text)
  -> Eff AgentStack a
  -> IO a
runAgentWithMemorySkillsAndTypstAndCaptureAndImageEdit memoryCfg skillsCfg rendered captured answers chatMock imageEdit action = do
  runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEditAndReferenced
    memoryCfg
    skillsCfg
    rendered
    captured
    answers
    chatMock
    Nothing
    (\_ _ -> pure "unused image answer")
    imageEdit
    action

runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEdit
  :: MemoryStore.MemoryConfig
  -> SkillsStore.SkillsConfig
  -> IORef.IORef [Text]
  -> Maybe (IORef.IORef [[LLM.ChatMessage]])
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> (LLM.ImageRequestOptions -> [LLM.ChatMessage] -> IO Text)
  -> (LLM.ImageRequestOptions -> Text -> [Text] -> Maybe Text -> IO Text)
  -> Eff AgentStack a
  -> IO a
runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEdit memoryCfg skillsCfg rendered captured answers chatMock imageGenerate imageEdit action = do
  runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEditAndReferenced
    memoryCfg
    skillsCfg
    rendered
    captured
    answers
    chatMock
    Nothing
    imageGenerate
    imageEdit
    action

runAgentWithMemorySkillsAndTypstAndCaptureAndImageEditAndReferenced
  :: MemoryStore.MemoryConfig
  -> SkillsStore.SkillsConfig
  -> IORef.IORef [Text]
  -> Maybe (IORef.IORef [[LLM.ChatMessage]])
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Maybe ReferencedMessage
  -> (LLM.ImageRequestOptions -> Text -> [Text] -> Maybe Text -> IO Text)
  -> Eff AgentStack a
  -> IO a
runAgentWithMemorySkillsAndTypstAndCaptureAndImageEditAndReferenced memoryCfg skillsCfg rendered captured answers chatMock referencedMessage imageEdit action = do
  runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEditAndReferenced
    memoryCfg
    skillsCfg
    rendered
    captured
    answers
    chatMock
    referencedMessage
    (\_ _ -> pure "unused image answer")
    imageEdit
    action

runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEditAndReferenced
  :: MemoryStore.MemoryConfig
  -> SkillsStore.SkillsConfig
  -> IORef.IORef [Text]
  -> Maybe (IORef.IORef [[LLM.ChatMessage]])
  -> IORef.IORef [LLM.ChatAnswer]
  -> ChatMock
  -> Maybe ReferencedMessage
  -> (LLM.ImageRequestOptions -> [LLM.ChatMessage] -> IO Text)
  -> (LLM.ImageRequestOptions -> Text -> [Text] -> Maybe Text -> IO Text)
  -> Eff AgentStack a
  -> IO a
runAgentWithMemorySkillsAndTypstAndCaptureAndImageGenerateAndEditAndReferenced requestedMemoryCfg skillsCfg rendered captured answers chatMock referencedMessage imageGenerate imageEdit action =
  withIsolatedMemoryConfig requestedMemoryCfg \memoryCfg -> do
  let runStack =
        runFileSystem
          . runProcess
          . runTimeout
          . runConcurrent
          . runFail
          . runPrim
          . runTestLog
          . StorageSQLite.runStorageSQLitePath ":memory:"
          . ConcurrencyManager.runConcurrencyManager
          . ResourceManager.runResourceManager
          . HTTP.runHTTP
          . TypstTest.runTypstWith (mockTypstRender rendered)
          . Scheduler.runScheduler
          . Memory.runMemory memoryCfg
          . Skills.runSkills skillsCfg
          . Media.runMediaPassthrough
          . LLMTest.runLLMWith
              (\messages -> do
                  lift $ captureMessages captured messages
                  S.yield "unused text stream answer"
                  pure "unused text stream answer")
              (\options messages -> do
                  lift $ captureMessages captured messages
                  answer <- liftIO (imageGenerate options messages)
                  S.yield answer
                  pure answer)
              (\options prompt imageRefs maskRef -> do
                  answer <- liftIO (imageEdit options prompt imageRefs maskRef)
                  S.yield answer
                  pure answer)
              (\_ messages -> do
                  lift $ captureMessages captured messages
                  S.yield "unused audio answer"
                  pure "unused audio answer")
              (\_ messages -> do
                  lift $ captureMessages captured messages
                  answer <- liftIO (popAnswer answers)
                  case answer of
                    LLM.ChatFinalAnswer{content} ->
                      S.yield content
                    LLM.ChatToolRequest{content}
                      | Text.null content -> pure ()
                      | otherwise -> S.yield content
                  pure answer)
          . ChatLog.runChatLog
          . Agent.runAgent
          . AgentAudit.runAgentAudit
          . Chat.runChatWith defaultAgentMockChatDriver
              { agentReply = mockReply chatMock
              , agentFetchMessage = \_ _ -> pure referencedMessage
              , agentUserAvatar = mockUserAvatar chatMock
          }
          . Lifecycle.runLifecycleEffect (\_ _ -> pure ())
          . runTestMatrix
          . runTestACP
  result <-
    runEff (runStack action)
  either assertFailure pure result

runAgentWithStreamingAnswers
  :: IORef.IORef [StreamingAnswer]
  -> ChatMock
  -> Eff AgentStack a
  -> IO a
runAgentWithStreamingAnswers answers chatMock action = withMemoryTempDir \memoryDir -> do
  rendered <- IORef.newIORef ([] :: [Text])
  let runStack =
        runFileSystem
          . runProcess
          . runTimeout
          . runConcurrent
          . runFail
          . runPrim
          . runTestLog
          . StorageSQLite.runStorageSQLitePath ":memory:"
          . ConcurrencyManager.runConcurrencyManager
          . ResourceManager.runResourceManager
          . HTTP.runHTTP
          . TypstTest.runTypstWith (mockTypstRender rendered)
          . Scheduler.runScheduler
          . Memory.runMemory (MemoryStore.MemoryConfig memoryDir)
          . Skills.runSkills defaultTestSkillsConfig
          . Media.runMediaPassthrough
          . LLMTest.runLLMWith
              (\_ -> S.yield "unused text stream answer" $> "unused text stream answer")
              (\_ _ -> S.yield "unused image answer" $> "unused image answer")
              (\_ _ _ _ -> S.yield "unused image edit answer" $> "unused image edit answer")
              (\_ _ -> S.yield "unused audio answer" $> "unused audio answer")
              (\_ _ -> do
                  streamingAnswer <- liftIO (popStreamingAnswer answers)
                  traverse_ S.yield streamingAnswer.chunks
                  pure streamingAnswer.answer)
          . ChatLog.runChatLog
          . Agent.runAgent
          . AgentAudit.runAgentAudit
          . Chat.runChatWith defaultAgentMockChatDriver
              { agentReply = mockReply chatMock
              , agentUserAvatar = mockUserAvatar chatMock
              }
          . Lifecycle.runLifecycleEffect (\_ _ -> pure ())
          . runTestMatrix
          . runTestACP
  result <-
    runEff (runStack action)
  either assertFailure pure result

defaultTestSkillsConfig :: SkillsStore.SkillsConfig
defaultTestSkillsConfig =
  SkillsStore.SkillsConfig "/tmp/cosmobot-agent-spec-unused-skills"

captureMessages :: IOE :> es => Maybe (IORef.IORef [[LLM.ChatMessage]]) -> [LLM.ChatMessage] -> Eff es ()
captureMessages captured messages =
  traverse_ (\ref -> liftIO $ IORef.modifyIORef' ref (<> [messages])) captured

mockTypstRender :: IOE :> es => IORef.IORef [Text] -> TypstTypes.TypstOutputFormat -> Text -> (FilePath -> Eff es r) -> Eff es r
mockTypstRender rendered _format source action = do
  liftIO $ IORef.modifyIORef' rendered (<> [source])
  action "/tmp/cosmobot-agent-spec-typst.png"

runTestLog :: IOE :> es => Eff (KatipE : es) a -> Eff es a
runTestLog action = startKatipE "agent-spec" "test" action

runTestACP :: Eff (ACP.ACP : es) a -> Eff es a
runTestACP =
  interpret \_ -> \case
    ACP.ReadClientFile{} ->
      pure (Left "ACP client file test interpreter is not configured.")
    ACP.WriteClientFile{} ->
      pure (Left "ACP client file test interpreter is not configured.")
    ACP.CreateClientTerminal{} ->
      pure (Left "ACP client terminal test interpreter is not configured.")
    ACP.ReadClientTerminalOutput{} ->
      pure (Left "ACP client terminal test interpreter is not configured.")
    ACP.WaitForClientTerminalExit{} ->
      pure (Left "ACP client terminal test interpreter is not configured.")
    ACP.KillClientTerminal{} ->
      pure (Left "ACP client terminal test interpreter is not configured.")
    ACP.ReleaseClientTerminal{} ->
      pure (Left "ACP client terminal test interpreter is not configured.")

popAnswer :: IORef.IORef [LLM.ChatAnswer] -> IO LLM.ChatAnswer
popAnswer answers =
  IORef.atomicModifyIORef' answers \case
    [] ->
      ([], chatAnswer "unexpected extra LLM call" [])
    answer : rest ->
      (rest, answer)

popSteering :: a -> IORef.IORef [a] -> IO a
popSteering fallback values =
  IORef.atomicModifyIORef' values \case
    [] ->
      ([], fallback)
    value : rest ->
      (rest, value)

popStreamingAnswer :: IORef.IORef [StreamingAnswer] -> IO StreamingAnswer
popStreamingAnswer answers =
  IORef.atomicModifyIORef' answers \case
    [] ->
      ([], StreamingAnswer{chunks = ["unexpected extra LLM call"], answer = chatAnswer "unexpected extra LLM call" []})
    answer : rest ->
      (rest, answer)

chatAnswer :: Text -> [LLM.ToolCall] -> LLM.ChatAnswer
chatAnswer content calls =
  LLM.chatAnswer content calls

chatAnswerWithUsage :: LLM.TokenUsage -> Text -> [LLM.ToolCall] -> LLM.ChatAnswer
chatAnswerWithUsage usage content calls =
  LLM.withChatAnswerTokenUsage (Just usage) (chatAnswer content calls)

highTokenUsage :: LLM.TokenUsage
highTokenUsage =
  LLM.TokenUsage
    { promptTokens = 900
    , completionTokens = 200
    , totalTokens = 1100
    , cachedPromptTokens = Just 600
    }

hasHighTokenUsage :: AgentAudit.AgentAuditRecord -> Bool
hasHighTokenUsage record =
  case record.event of
    AgentAudit.ModelTurnFinished{tokenUsage = Just usage} ->
      usage.totalTokens == highTokenUsage.totalTokens &&
        usage.promptTokens == highTokenUsage.promptTokens &&
        usage.completionTokens == highTokenUsage.completionTokens
    _ ->
      False

toolCall :: Text -> Text -> Aeson.Value -> LLM.ToolCall
toolCall callId name arguments =
  LLM.ToolCall
    { id = callId
    , name = name
    , arguments = jsonText arguments
    }

encodedToolParameters :: AgentTool.Tool (Eff es) -> Eff es Text
encodedToolParameters definition = do
  schema <- AgentTool.resolveToolSchema definition agentContext (startWithUser "") 0
  pure . TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode $
    maybe Aeson.Null (.parameters) schema

agentContext :: Agent.Context
agentContext =
  Agent.Context
    { message = testMessage
    , input = inputWithImages testMessage.text testMessage.imageUrls
    , superuser = False
    , systemContext = ""
    , askCommand = "!ask"
    , toolConfig = Agent.defaultToolConfig
    }

superuserContext :: Agent.Context
superuserContext =
  agentContext{Agent.superuser = True}

testToolCallMetadata :: Agent.ToolCallMetadata
testToolCallMetadata =
  Agent.ToolCallMetadata{agentRunId = "agent-test", originRunId = "agent-test", resourceOwner = Nothing}

startTestRuntime
  :: (Chat.Chat :> es, Concurrent :> es, IOE :> es)
  => Int
  -> Agent.Context
  -> [AgentTool.Tool (Eff es)]
  -> Eff es (Agent.Runtime context (Eff es))
startTestRuntime =
  Agent.startRuntime

runTestAgent
  :: Int
  -> Agent.Context
  -> [AgentTool.Tool (Eff AgentStack)]
  -> Transcript
  -> Eff AgentStack (Text, Transcript)
runTestAgent maxTurns context tools transcript = do
  runtime <- startTestRuntime maxTurns context tools
  outputs S.:> result <-
    S.toList (Agent.agentStream (Agent.defaultRuntime AgentAudit.agentAuditObserver 1000000 runtime) transcript)
  pure (agentOutputText outputs, result.transcript)

runTestAgentStreaming
  :: Int
  -> Agent.Context
  -> [AgentTool.Tool (Eff AgentStack)]
  -> Transcript
  -> Stream (Of Agent.Output) (Eff AgentStack) Transcript
runTestAgentStreaming maxTurns context tools transcript = do
  runtime <- lift (startTestRuntime maxTurns context tools)
  result <- Agent.agentStream (Agent.defaultRuntime AgentAudit.agentAuditObserver 1000000 runtime) transcript
  pure result.transcript

runAgentWithToolMessageCapture
  :: Int
  -> Agent.Context
  -> [AgentTool.Tool (Eff AgentStack)]
  -> Transcript
  -> IORef.IORef [Text]
  -> IORef.IORef [Maybe MessageId]
  -> Eff AgentStack (Text, Transcript)
runAgentWithToolMessageCapture maxTurns context tools transcript recorded remembered = do
  agentRun <- startTestRuntime maxTurns context tools
  let sink = Agent.ToolEmittedMessageSink \messageId ->
        liftIO $ IORef.modifyIORef' remembered (<> [messageId])
      program =
        ( Agent.withRecordingToolSelfMessages \body ->
            liftIO $ IORef.modifyIORef' recorded (<> [body])
        )
          . Agent.withLinkingToolEmittedMessagesToThread sink
          $ Agent.defaultRuntime AgentAudit.agentAuditObserver 1000000 agentRun
  outputs S.:> result <- S.toList (Agent.agentStream program transcript)
  pure (agentOutputText outputs, result.transcript)

runTestMatrix :: Eff (Matrix.Matrix : es) a -> Eff es a
runTestMatrix =
  interpret \_ -> \case
    Matrix.MatrixClientCall _ ->
      pure Aeson.Null

agentOutputText :: [Agent.Output] -> Text
agentOutputText =
  Text.strip . foldMap \case
    Agent.ContentDelta chunk ->
      chunk
    Agent.ToolCallNotification{} ->
      ""
    Agent.ReplyBoundary ->
      ""

assertElem :: (Eq a, Show a) => a -> [a] -> Assertion
assertElem expected actual =
  assertBool [i|expected #{show expected :: String} in #{show actual :: String}|] (expected `elem` actual)

mockReply :: IOE :> es => ChatMock -> IncomingMessage -> Text -> Eff es (Either Text MessageId)
mockReply ChatMock{replies, replyId} _ body = do
  traverse_ (\ref -> liftIO $ IORef.modifyIORef' ref (<> [body])) replies
  pure (maybe (Left "mock reply did not produce a message id") Right replyId)

recordReply :: IOE :> es => IORef.IORef [(Maybe MessageId, Text)] -> IORef.IORef Integer -> IncomingMessage -> Text -> Eff es (Either Text MessageId)
recordReply replies nextReplyId message body = do
  liftIO $ IORef.modifyIORef' replies (<> [(message.messageId, body)])
  liftIO $ IORef.atomicModifyIORef' nextReplyId \replyId ->
    (replyId + 1, Right (integerMessageId replyId))

recordEdit :: IOE :> es => IORef.IORef [(MessageId, Text)] -> IncomingMessage -> MessageId -> Text -> Eff es Bool
recordEdit edits _ messageId body = do
  liftIO $ IORef.modifyIORef' edits (<> [(messageId, body)])
  pure True

runMediaNormalizingRefs :: Eff (Media.Media : es) a -> Eff es a
runMediaNormalizingRefs =
  interpret \_ -> \case
    Media.StoreMediaObject mediaObject ->
      pure (Just ("media:stored:" <> mediaObject.mimeType))
    Media.StoreMediaObjectFromSource sourceRef _ ->
      pure (Just ("media:" <> sourceRef))
    Media.MediaRefForSource sourceRef ->
      pure (Just ("media:" <> sourceRef))
    Media.GetMediaCacheEntry _ ->
      pure Nothing
    Media.DeleteMediaFile _ ->
      pure False
    Media.GetMediaFileInfo _ ->
      pure Nothing
    Media.ListMediaFiles ->
      pure []
    Media.GetMediaCacheStats ->
      pure Media.MediaCacheStats{files = 0, existingFiles = 0, missingFiles = 0, totalBytes = 0, sources = 0, platformRefs = 0}
    Media.GcMediaCache _ _ ->
      pure 0
    Media.NormalizeMediaRef ref ->
      pure ("media:" <> ref)
    Media.PublicMediaRef ref ->
      pure ref
    Media.ModelImageRef ref ->
      pure ref
    Media.LocalMediaPath _ ->
      pure Nothing
    Media.PlatformMediaRef _ _ _ ->
      pure Nothing
    Media.StorePlatformMediaRef _ _ _ _ ->
      pure ()

runMediaLeavingRefs :: Eff (Media.Media : es) a -> Eff es a
runMediaLeavingRefs =
  interpret \_ -> \case
    Media.StoreMediaObject _ ->
      pure Nothing
    Media.StoreMediaObjectFromSource _ _ ->
      pure Nothing
    Media.MediaRefForSource _ ->
      pure Nothing
    Media.GetMediaCacheEntry _ ->
      pure Nothing
    Media.DeleteMediaFile _ ->
      pure False
    Media.GetMediaFileInfo _ ->
      pure Nothing
    Media.ListMediaFiles ->
      pure []
    Media.GetMediaCacheStats ->
      pure Media.MediaCacheStats{files = 0, existingFiles = 0, missingFiles = 0, totalBytes = 0, sources = 0, platformRefs = 0}
    Media.GcMediaCache _ _ ->
      pure 0
    Media.NormalizeMediaRef ref ->
      pure ref
    Media.PublicMediaRef ref ->
      pure ref
    Media.ModelImageRef ref ->
      pure ref
    Media.LocalMediaPath _ ->
      pure Nothing
    Media.PlatformMediaRef _ _ _ ->
      pure Nothing
    Media.StorePlatformMediaRef _ _ _ _ ->
      pure ()

mockUserAvatar :: ChatMock -> IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
mockUserAvatar ChatMock{userAvatar} _ _ =
  pure userAvatar

testMessage :: IncomingMessage
testMessage =
  IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just 100
    , chatAliases = []
    , digest = emptyMessageDigest
    , senderId = Just "200"
    , senderUsername = Just "alice"
    , messageId = Just "300"
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , files = []
    , text = "!ask"
    , raw = Aeson.Null
    }

askHandlerConfig :: AskHandlerConfig
askHandlerConfig =
  AskHandlerConfig
    { name = Just "krkr"
    , command = "!ask"
    , drawCommand = "!draw"
    , systemPrompt = "base system prompt"
    , agentMaxTurns = 4
    , contextCompactionThresholdKTokens = 1000
    , botIds = [(PlatformQQ, "2044933066")]
    }

askHandlerMessage :: IncomingMessage
askHandlerMessage =
  IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = PlatformQQ
    , kind = ChatGroup
    , chatId = Just 906230260
    , chatAliases = []
    , digest = emptyMessageDigest
        { chatIsAllowed = True
        , senderIsAllowed = True
        , senderIsSuperuser = True
        }
    , senderId = Just "295947730"
    , senderUsername = Nothing
    , messageId = Just "294869878"
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , files = []
    , text = "krkr 看下我的头像"
    , raw = Aeson.Null
    }

jsonText :: Aeson.ToJSON a => a -> Text
jsonText =
  decodeUtf8 . toStrict . Aeson.encode

streamPayload :: Aeson.Value -> Aeson.Value
streamPayload delta =
  Aeson.object
    [ "choices" Aeson..=
        [ Aeson.object
            [ "delta" Aeson..= delta
            ]
        ]
    ]

waitUntil :: (Concurrent :> es, IOE :> es) => Eff es Bool -> Eff es ()
waitUntil predicate =
  go (50 :: Int)
  where
    go 0 =
      liftIO $ assertFailure "timed out waiting for condition"
    go remaining = do
      done <- predicate
      unless done do
        threadDelay 20_000
        go (remaining - 1)

runAskHandlersAndWait
  :: Agent.ToolConfig
  -> AskHandlerConfig
  -> ThreadStore
  -> IncomingMessage
  -> Eff AgentStack ()
runAskHandlersAndWait toolConfig config threads message = do
  runAskHandlersWithToolsAndWait toolConfig AgentTools.defaultTools config threads message

runAskHandlersWithToolsAndWait
  :: Agent.ToolConfig
  -> [AgentTool.Tool (Eff AgentStack)]
  -> AskHandlerConfig
  -> ThreadStore
  -> IncomingMessage
  -> Eff AgentStack ()
runAskHandlersWithToolsAndWait toolConfig tools config threads message = do
  before <- Concurrency.list
  runHandlers (askHandlers toolConfig tools config threads) message
  snapshotAfter <- Concurrency.list
  let existingIds = (.id) <$> before.entries
      started = filter ((`notElem` existingIds) . (.id)) snapshotAfter.entries
  when (null started) (liftIO $ assertFailure "ask handler did not start background work")
  for_ started (Concurrency.await . Concurrency.Handle . (.id))
