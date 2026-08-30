module Main (main) where

import qualified Bot.Agent as Agent
import qualified Bot.Agent.Core as AgentCore
import qualified Bot.Agent.Middleware.Continuation as Continuation
import qualified Bot.Agent.Middleware.Observation as Observation
import Bot.Agent.Middleware.Observation.Types
  ( EventObservation
  , ToolResultObservation
  )
import qualified Bot.Agent.Middleware.ToolResultCompaction as ToolResultCompaction
import qualified Bot.Agent.Middleware.Tools as AgentTools
import qualified Bot.Agent.Tool as Tool
import qualified Bot.Agent.ToolRegistry as ToolRegistry
import qualified Bot.Agent.Transcript as AgentTranscript
import qualified Bot.Agent.Tools.Continuation as ContinuationTools
import Bot.Agent.Tools.Common (jsonText)
import Bot.Core.Message
import Bot.Core.Transcript
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import qualified Bot.LLM.OpenAI.Retry as Retry
import qualified Bot.LLM.OpenAI.Transport as LLMTransport
import qualified Bot.LLM.Test as LLMTest
import Bot.Prelude
import qualified Bot.Util.HList as HList
import qualified Bot.Util.Stream as StreamUtil
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Effectful.Concurrent.Async as Async
import qualified Effectful.Concurrent.MVar as MVar
import Effectful.Timeout (Timeout, runTimeout)
import qualified Effectful.Timeout as Timeout
import qualified Network.HTTP.Client as HTTP
import qualified Streaming.Prelude as S
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit

type TestEffects =
  '[ LLM.LLM
   , Media.Media
   , Timeout
   , KatipE
   , Concurrent
   , IOE
   ]

type BaseEffects =
  '[ Timeout
   , KatipE
   , Concurrent
   , IOE
   ]

type ModelInterpreter =
  [LLM.FunctionTool]
  -> [LLM.ChatMessage]
  -> Stream (Of Text) (Eff (Media.Media ': BaseEffects)) LLM.ChatAnswer

newtype InjectedFailure = InjectedFailure Text
  deriving stock (Show)
  deriving anyclass (Exception)

data FaultPlan = FaultPlan
  { firstTurnTimeouts :: !Int
  , secondTurnTimeouts :: !Int
  , failingTools :: ![Bool]
  , stopAtToolLimit :: !Bool
  , startDamaged :: !Bool
  , terminalDeliveries :: !Int
  }
  deriving stock (Show)

main :: IO ()
main =
  defaultMain $
    testGroup "agent failure injection"
      [ testCase "model timeout retries without duplicating transcript" testModelTimeout
      , testCase "model timeout stops after the retry budget" testModelTimeoutExhaustion
      , testCase "second-turn retry reuses transcript without replaying tools" testSecondTurnRetry
      , testCase "model failure after output is not retried and releases its stream" testModelFailureAfterOutput
      , testCase "permanent model failure is not retried" testPermanentModelFailure
      , testCase "model cancellation releases the response stream" testModelCancellation
      , testCase "tool exception becomes one protocol-complete failure result" testToolFailure
      , testCase "post-tool middleware failure replaces success exactly once" testPostToolMiddlewareFailure
      , testCase "control-call recovery converts sync failure but preserves async cancellation" testControlCallFailureRecovery
      , testCase "unknown and malformed tool calls each receive a result" testInvalidToolCalls
      , testCase "concurrent tool failure does not discard sibling success" testMixedConcurrentTools
      , testCase "async tool failure is not swallowed and cancels its sibling" testAsyncToolFailure
      , testCase "out-of-order tool completion preserves request order" testOutOfOrderTools
      , testCase "cancellation cleans every tool and the enclosing tool turn" testCancellationCleanup
      , testCase "cancellation between tool completion and continuation prevents next turn" testCancellationBeforeContinuation
      , testCase "interrupted tool history is repaired before the model sees it" testInterruptedTranscriptRepair
      , testCase "tool limit skips execution and closes every requested call" testToolLimitTranscript
      , testCase "schema resolution failure prevents a model request" testSchemaResolutionFailure
      , testCase "duplicate resolved tool names prevent a model request" testDuplicateToolSchemas
      , testCase "two terminal deliveries resume the program only once" testDuplicateResponse
      , middlewareFailureTests
      , lifecycleCancellationTests
      , transportFailureTests
      , transcriptCorruptionTests
      , continuationFailureTests
      , compactionAndObservationTests
      , crossProductTests
      ]

middlewareFailureTests :: TestTree
middlewareFailureTests =
  testGroup "middleware failure boundaries"
    [ testCase "model input failure prevents transport" testModelInputFailure
    , testCase "model-turn acquire failure prevents transport" testModelTurnAcquireFailure
    , testCase "model-turn release failure discards the decision" testModelTurnReleaseFailure
    , testCase "tool-turn acquire failure prevents every tool" testToolTurnAcquireFailure
    , testCase "tool-turn release failure prevents continuation" testToolTurnReleaseFailure
    , testCase "program wrapper failure prevents the first event" testProgramWrapperFailure
    , testCase "agent-run release failure runs after the terminal result" testAgentRunReleaseFailure
    ]

lifecycleCancellationTests :: TestTree
lifecycleCancellationTests =
  testGroup "lifecycle cancellation boundaries"
    [ testCase "cancellation during retry backoff stops further attempts" testRetryBackoffCancellation
    , testCase "cancellation during schema resolution releases the resolver" testSchemaResolutionCancellation
    , testCase "tool initialization failure releases acquired state" testToolInitializationFailure
    ]

transportFailureTests :: TestTree
transportFailureTests =
  testGroup "streaming transport corruption"
    [ testCase "malformed chunk shape is rejected" testMalformedStreamChunk
    , testCase "premature EOF drops an incomplete tool call" testPrematureToolCallEOF
    , testCase "duplicate deltas keep stream and terminal answer consistent" testDuplicateStreamDelta
    , testCase "out-of-order tool deltas are assembled by index" testOutOfOrderToolDeltas
    ]

transcriptCorruptionTests :: TestTree
transcriptCorruptionTests =
  testGroup "hostile transcript repair"
    [ testCase "orphan, duplicate, and unknown tool results are normalized" testToolResultNormalization
    , testCase "reversed tool results are restored to request order" testReversedToolResultRepair
    , testCase "consecutive interrupted turns are independently closed" testConsecutiveInterruptedTurns
    , testCase "fresh duplicate tool-call ids abort before execution" testDuplicateToolCallIds
    , testCase "fresh empty tool-call ids abort before execution" testEmptyToolCallId
    ]

continuationFailureTests :: TestTree
continuationFailureTests =
  testGroup "continuation failures"
    [ testCase "duplicate capture and repeated resume remain one-shot" testContinuationOneShotFailures
    , testCase "malformed and concurrent control calls execute no sibling" testContinuationMalformedAndConcurrent
    , testCase "cancellation before capture commit releases the tool turn" testContinuationCaptureCancellation
    ]

compactionAndObservationTests :: TestTree
compactionAndObservationTests =
  testGroup "compaction and observation failures"
    [ testCase "media-store failure aborts after one tool execution" testCompactionMediaFailure
    , testCase "unavailable media preserves immediate result and omits durable result" testCompactionMediaUnavailable
    , testCase "huge result is compacted before the immediate model turn" testHugeResultImmediateCompaction
    , testCase "large image result keeps image context while compacting text" testLargeImageResultCompaction
    , testCase "tool-finished observer failure becomes a tool result" testToolObserverFailure
    , testCase "run-start observer failure emits interruption before transport" testRunStartObserverFailure
    , testCase "run-finished observer failure emits interruption after completion" testRunFinishedObserverFailure
    ]

crossProductTests :: TestTree
crossProductTests =
  testGroup "cross-product fault plans (768 cases)" $
    zipWith
      (\index plan -> testCase [i|#{index}: #{show plan :: String}|] (testFaultPlan plan))
      [(1 :: Int) ..]
      faultPlans

faultPlans :: [FaultPlan]
faultPlans =
  [ FaultPlan
      { firstTurnTimeouts
      , secondTurnTimeouts
      , failingTools
      , stopAtToolLimit
      , startDamaged
      , terminalDeliveries
      }
  | firstTurnTimeouts <- [0, 1, 3, 4]
  , secondTurnTimeouts <- [0, 1, 3, 4]
  , failingTools <- [[False], [True], [False, True], [True, False], [False, False], [True, True]]
  , stopAtToolLimit <- [False, True]
  , startDamaged <- [False, True]
  , terminalDeliveries <- [1, 2]
  ]

testModelTimeout :: Assertion
testModelTimeout = do
  attempts <- IORef.newIORef (0 :: Int)
  delays <- IORef.newIORef ([] :: [Int])
  let model _ _ =
        Retry.retryLLMStreamRequestWith
          (\seconds -> liftIO $ IORef.modifyIORef' delays (<> [seconds]))
          "injected model request"
          do
            attempt <- liftIO $ IORef.atomicModifyIORef' attempts \current ->
              let next = current + 1
              in (next, next)
            if attempt == 1
              then lift $ throwIO modelTimeout
              else S.yield "recovered" $> LLM.chatAnswer "recovered" []
  (outputs, result) <- runFailureModel model (runTestAgent [] (startWithUser "hello"))
  IORef.readIORef attempts >>= (@?= 2)
  IORef.readIORef delays >>= (@?= [2])
  contentDeltas outputs @?= ["recovered"]
  assistantTexts result.transcript @?= ["recovered"]

testModelTimeoutExhaustion :: Assertion
testModelTimeoutExhaustion = do
  attempts <- IORef.newIORef (0 :: Int)
  releases <- IORef.newIORef (0 :: Int)
  delays <- IORef.newIORef ([] :: [Int])
  requests <- IORef.newIORef ([] :: [[Text]])
  let model _ messages =
        Retry.retryLLMStreamRequestWith
          (\seconds -> liftIO $ IORef.modifyIORef' delays (<> [seconds]))
          "exhausted model request"
          $ StreamUtil.bracketStream
              (liftIO $ IORef.modifyIORef' attempts (+ 1))
              (\_ -> liftIO $ IORef.modifyIORef' releases (+ 1))
              \_ -> do
                liftIO $ IORef.modifyIORef' requests (<> [map (.role) messages])
                lift $ throwIO modelTimeout
  outcome <- runFailureModel model $ trySync do
    void $ S.toList =<< (Agent.agentStream <$> testRuntime [] <*> pure (startWithUser "hello"))
  assertBool "exhausted timeout should escape" (isLeft outcome)
  IORef.readIORef attempts >>= (@?= 4)
  IORef.readIORef releases >>= (@?= 4)
  IORef.readIORef delays >>= (@?= [2, 4, 8])
  IORef.readIORef requests >>= (@?= replicate 4 ["user"])

testSecondTurnRetry :: Assertion
testSecondTurnRetry = do
  modelAttempts <- IORef.newIORef (0 :: Int)
  requests <- IORef.newIORef ([] :: [Aeson.Value])
  delays <- IORef.newIORef ([] :: [Int])
  toolExecutions <- IORef.newIORef (0 :: Int)
  let model _ messages =
        Retry.retryLLMStreamRequestWith
          (\seconds -> liftIO $ IORef.modifyIORef' delays (<> [seconds]))
          "second-turn model request"
          do
            attempt <- liftIO $ IORef.atomicModifyIORef' modelAttempts \current ->
              let next = current + 1
              in (next, next)
            liftIO $ IORef.modifyIORef' requests (<> [Aeson.toJSON messages])
            case attempt of
              1 ->
                pure (LLM.chatAnswer "" [toolCall "once-1" "once"])
              2 ->
                lift $ throwIO modelTimeout
              _ ->
                S.yield "done" $> LLM.chatAnswer "done" []
      once =
        Tool.tool "once" Tool.noArguments do
          liftIO $ IORef.modifyIORef' toolExecutions (+ 1)
          pure (Agent.toolText "ran once")
  (_, result) <- runFailureModel model (runTestAgent [once] (startWithUser "retry after tool"))
  IORef.readIORef modelAttempts >>= (@?= 3)
  IORef.readIORef delays >>= (@?= [2])
  IORef.readIORef toolExecutions >>= (@?= 1)
  captured <- IORef.readIORef requests
  case captured of
    [_firstTurn, failedAttempt, retryAttempt] ->
      failedAttempt @?= retryAttempt
    other ->
      assertFailure [i|expected one tool turn and two identical final-turn attempts, got #{length other}|]
  toolResults result.transcript @?= [("once-1", "ran once")]
  assertToolProtocolComplete result.transcript

testModelFailureAfterOutput :: Assertion
testModelFailureAfterOutput = do
  attempts <- IORef.newIORef (0 :: Int)
  released <- IORef.newIORef False
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  let model _ messages =
        Retry.retryLLMStreamRequestWith
          (\_ -> pure ())
          "partially streamed model request"
          $ StreamUtil.bracketStream
              (liftIO $ IORef.modifyIORef' attempts (+ 1))
              (\_ -> liftIO $ IORef.writeIORef released True)
              \_ -> do
                liftIO $ IORef.modifyIORef' captured (<> [messages])
                S.yield "partial"
                lift $ throwIO modelTimeout
  outcome <- runFailureModel model $ trySync do
    void $ S.toList =<< (Agent.agentStream <$> testRuntime [] <*> pure (startWithUser "hello"))
  assertBool "failure after visible output should escape instead of replaying output" (isLeft outcome)
  IORef.readIORef attempts >>= (@?= 1)
  IORef.readIORef released >>= (@?= True)
  fmap (map (map (.role))) (IORef.readIORef captured) >>= (@?= [["user"]])

testPermanentModelFailure :: Assertion
testPermanentModelFailure = do
  attempts <- IORef.newIORef (0 :: Int)
  let model _ _ =
        Retry.retryLLMStreamRequestWith
          (\_ -> pure ())
          "permanently failed model request"
          do
            liftIO $ IORef.modifyIORef' attempts (+ 1)
            lift $ throwIO (InjectedFailure "permanent model failure")
  outcome <- runFailureModel model $ trySync do
    void $ S.toList =<< (Agent.agentStream <$> testRuntime [] <*> pure (startWithUser "hello"))
  assertBool "permanent model failure should escape" (isLeft outcome)
  IORef.readIORef attempts >>= (@?= 1)

testModelCancellation :: Assertion
testModelCancellation = do
  started <- IORef.newIORef False
  released <- IORef.newIORef False
  let model _ _ =
        StreamUtil.bracketStream
          (liftIO $ IORef.writeIORef started True)
          (\_ -> liftIO $ IORef.writeIORef released True)
          (\_ -> lift (threadDelay maxBound) >> pure (LLM.chatAnswer "unreachable" []))
  outcome <- runFailureModel model $
    Timeout.timeout 1_000_000 do
      worker <- Async.async do
        runtime <- testRuntime []
        S.toList (Agent.agentStream runtime (startWithUser "cancel model"))
      awaitFlag started
      Async.cancel worker
      result <- Async.waitCatch worker
      awaitFlag released
      pure (isLeft result)
  outcome @?= Just True

testToolFailure :: Assertion
testToolFailure = do
  answers <- IORef.newIORef
    [ LLM.chatAnswer "" [toolCall "failure-1" "explode"]
    , LLM.chatAnswer "recovered" []
    ]
  let explode =
        Tool.tool "explode" Tool.noArguments $
          throwIO (InjectedFailure "tool failure")
  (_, result) <- runFailureModel (scriptedModel answers) (runTestAgent [explode] (startWithUser "break"))
  case toolResults result.transcript of
    [("failure-1", failure)] ->
      assertBool "tool exception should be visible to the model" ("Tool explode failed" `Text.isInfixOf` failure)
    other ->
      assertFailure [i|expected one matching failure result, got #{show other :: String}|]
  transcriptRoles result.transcript @?= ["user", "assistant", "tool", "assistant"]

testPostToolMiddlewareFailure :: Assertion
testPostToolMiddlewareFailure = do
  answers <- IORef.newIORef
    [ LLM.chatAnswer "" [toolCall "post-1" "commits"]
    , LLM.chatAnswer "recovered" []
    ]
  executions <- IORef.newIORef (0 :: Int)
  let commits =
        Tool.tool "commits" Tool.noArguments do
          liftIO $ IORef.modifyIORef' executions (+ 1)
          pure (Agent.toolText "success that must not leak")
  (_, result) <- runFailureModel (scriptedModel answers) do
    runtime@AgentCore.Runtime{AgentCore.aroundToolCall = innerToolCall} <-
      testRuntime [commits]
    let faulting =
          runtime
            { AgentCore.aroundToolCall = \turn call context action -> do
                void (innerToolCall turn call context action)
                throwIO (InjectedFailure "post-tool middleware failure")
            }
        recovered = AgentTools.withToolFailureRecovery faulting
    S.toList (Agent.agentStream recovered (startWithUser "post failure"))
      <&> \(outputs S.:> final) -> (outputs, final)
  IORef.readIORef executions >>= (@?= 1)
  case toolResults result.transcript of
    [("post-1", failure)] ->
      assertBool "the uncommitted success must be replaced by failure" $
        "post-tool middleware failure" `Text.isInfixOf` failure
          && not ("success that must not leak" `Text.isInfixOf` failure)
    other ->
      assertFailure [i|expected one post-middleware failure, got #{show other :: String}|]

testControlCallFailureRecovery :: Assertion
testControlCallFailureRecovery = do
  answers <- IORef.newIORef []
  (syncResult, asyncResult) <- runFailureModel (scriptedModel answers) do
    runtime <- AgentTools.withToolFailureRecovery <$> testRuntime []
    let call = toolCall "control-1" "py"
    syncResult <- runtime.aroundControlCall 1 call HList.HNil (throwIO (InjectedFailure "control failed"))
    asyncResult <- try @SomeException $
      runtime.aroundControlCall 1 call HList.HNil (throwIO ThreadKilled)
    pure (syncResult, asyncResult)
  case syncResult of
    Agent.ToolFailed{failure} ->
      assertBool "sync failure should retain the call name and cause" $
        all (`Text.isInfixOf` failure.userMessage) ["Tool py failed:", "control failed"]
    Agent.ToolSucceeded{} ->
      assertFailure "sync control failure was not converted to ToolFailed"
  case asyncResult of
    Left err ->
      fromException err @?= Just ThreadKilled
    Right{} ->
      assertFailure "async control cancellation was converted to a tool result"

testInvalidToolCalls :: Assertion
testInvalidToolCalls = do
  answers <- IORef.newIORef
    [ LLM.chatAnswer ""
        [ toolCall "unknown-1" "missing"
        , toolCallWithArguments "malformed-1" "known" "not-json"
        ]
    , LLM.chatAnswer "recovered" []
    ]
  let known = Tool.tool "known" Tool.noArguments (pure (Agent.toolText "unexpected"))
  (_, result) <- runFailureModel (scriptedModel answers) (runTestAgent [known] (startWithUser "bad calls"))
  case toolResults result.transcript of
    [("unknown-1", unknown), ("malformed-1", malformed)] -> do
      assertBool "unknown call should be closed" ("Unknown tool: missing" `Text.isInfixOf` unknown)
      assertBool "malformed arguments should be closed" ("Invalid JSON arguments for known" `Text.isInfixOf` malformed)
    other ->
      assertFailure [i|expected ordered failure results for both calls, got #{show other :: String}|]
  assertToolProtocolComplete result.transcript

testMixedConcurrentTools :: Assertion
testMixedConcurrentTools = do
  answers <- IORef.newIORef
    [ LLM.chatAnswer "" [toolCall "failed-1" "fails", toolCall "ok-1" "works"]
    , LLM.chatAnswer "recovered" []
    ]
  let fails =
        Tool.tool "fails" Tool.noArguments $
          throwIO (InjectedFailure "concurrent failure")
      works =
        Tool.tool "works" Tool.noArguments $
          pure (Agent.toolText "sibling survived")
  (_, result) <- runFailureModel (scriptedModel answers) (runTestAgent [fails, works] (startWithUser "mixed"))
  case toolResults result.transcript of
    [("failed-1", failure), ("ok-1", success)] -> do
      assertBool "failed call should become a tool result" ("Tool fails failed" `Text.isInfixOf` failure)
      success @?= "sibling survived"
    other ->
      assertFailure [i|expected both concurrent outcomes, got #{show other :: String}|]
  assertToolProtocolComplete result.transcript

testAsyncToolFailure :: Assertion
testAsyncToolFailure = do
  answers <- IORef.newIORef
    [ LLM.chatAnswer "" [toolCall "killer-1" "killer", toolCall "sibling-1" "sibling"]
    ]
  outcome <- runFailureModel (scriptedModel answers) $
    Timeout.timeout 1_000_000 do
      siblingStarted <- MVar.newEmptyMVar
      siblingCleaned <- MVar.newEmptyMVar
      let killer =
            Tool.tool "killer" Tool.noArguments do
              MVar.takeMVar siblingStarted
              throwIO ThreadKilled
          sibling =
            Tool.tool "sibling" Tool.noArguments $
              ((MVar.putMVar siblingStarted () >> threadDelay maxBound)
                `finally` MVar.putMVar siblingCleaned ())
                $> Agent.toolText "unreachable"
      result <- try @SomeException (runTestAgent [killer, sibling] (startWithUser "async failure"))
      MVar.takeMVar siblingCleaned
      pure $ case result of
        Left err -> fromException err == Just ThreadKilled
        Right{} -> False
  outcome @?= Just True

testOutOfOrderTools :: Assertion
testOutOfOrderTools = do
  answers <- IORef.newIORef
    [ LLM.chatAnswer "" [toolCall "slow-1" "slow", toolCall "fast-1" "fast"]
    , LLM.chatAnswer "done" []
    ]
  completionOrder <- IORef.newIORef ([] :: [Text])
  (physicalOrder, (_, result)) <- runFailureModel (scriptedModel answers) do
    slowStarted <- MVar.newEmptyMVar
    fastFinished <- MVar.newEmptyMVar
    let slow =
          Tool.tool "slow" Tool.noArguments do
            MVar.putMVar slowStarted ()
            MVar.takeMVar fastFinished
            liftIO $ IORef.modifyIORef' completionOrder (<> ["slow"])
            pure (Agent.toolText "slow-result")
        fast =
          Tool.tool "fast" Tool.noArguments do
            MVar.takeMVar slowStarted
            liftIO $ IORef.modifyIORef' completionOrder (<> ["fast"])
            MVar.putMVar fastFinished ()
            pure (Agent.toolText "fast-result")
    agentResult <- runTestAgent [slow, fast] (startWithUser "race")
    order <- liftIO (IORef.readIORef completionOrder)
    pure (order, agentResult)
  physicalOrder @?= ["fast", "slow"]
  toolResults result.transcript
    @?= [("slow-1", "slow-result"), ("fast-1", "fast-result")]

testCancellationCleanup :: Assertion
testCancellationCleanup = do
  answers <- IORef.newIORef
    [ LLM.chatAnswer "" [toolCall "left-1" "left", toolCall "right-1" "right"]
    ]
  outcome <- runFailureModel (scriptedModel answers) $
    Timeout.timeout 1_000_000 do
      leftStarted <- MVar.newEmptyMVar
      rightStarted <- MVar.newEmptyMVar
      leftCleaned <- MVar.newEmptyMVar
      rightCleaned <- MVar.newEmptyMVar
      turnCleaned <- MVar.newEmptyMVar
      let blocker name started cleaned =
            Tool.tool name Tool.noArguments $
              ((MVar.putMVar started () >> threadDelay maxBound)
                `finally` MVar.putMVar cleaned ())
                $> Agent.toolText "unreachable"
      runtime@AgentCore.Runtime{AgentCore.aroundToolTurn = innerToolTurn} <- testRuntime
        [ blocker "left" leftStarted leftCleaned
        , blocker "right" rightStarted rightCleaned
        ]
      let runtimeWithTurnCleanup =
            runtime
              { AgentCore.aroundToolTurn = \context request action ->
                  innerToolTurn context request action
                    `finally` MVar.putMVar turnCleaned ()
              }
      worker <- Async.async $ S.toList (Agent.agentStream runtimeWithTurnCleanup (startWithUser "cancel"))
      MVar.takeMVar leftStarted
      MVar.takeMVar rightStarted
      Async.cancel worker
      result <- Async.waitCatch worker
      MVar.takeMVar leftCleaned
      MVar.takeMVar rightCleaned
      MVar.takeMVar turnCleaned
      pure (isLeft result)
  outcome @?= Just True

testCancellationBeforeContinuation :: Assertion
testCancellationBeforeContinuation = do
  modelCalls <- IORef.newIORef (0 :: Int)
  executions <- IORef.newIORef (0 :: Int)
  let model _ _ = do
        callNumber <- liftIO $ IORef.atomicModifyIORef' modelCalls \current ->
          let next = current + 1
          in (next, next)
        if callNumber == 1
          then pure (LLM.chatAnswer "" [toolCall "commit-1" "commit"])
          else pure (LLM.chatAnswer "unexpected second turn" [])
      commit =
        Tool.tool "commit" Tool.noArguments do
          liftIO $ IORef.modifyIORef' executions (+ 1)
          pure (Agent.toolText "committed")
  outcome <- runFailureModel model $
    Timeout.timeout 1_000_000 do
      phaseCompleted <- MVar.newEmptyMVar
      phaseReleased <- MVar.newEmptyMVar
      runtime@AgentCore.Runtime{AgentCore.aroundToolTurn = innerToolTurn} <-
        testRuntime [commit]
      let pausedAfterTools =
            runtime
              { AgentCore.aroundToolTurn = \context request action ->
                  (do
                    result <- innerToolTurn context request action
                    MVar.putMVar phaseCompleted ()
                    threadDelay maxBound
                    pure result)
                    `finally` MVar.putMVar phaseReleased ()
              }
      worker <- Async.async $
        S.toList (Agent.agentStream pausedAfterTools (startWithUser "cancel before continuation"))
      MVar.takeMVar phaseCompleted
      Async.cancel worker
      result <- Async.waitCatch worker
      MVar.takeMVar phaseReleased
      pure (isLeft result)
  outcome @?= Just True
  IORef.readIORef executions >>= (@?= 1)
  IORef.readIORef modelCalls >>= (@?= 1)

testInterruptedTranscriptRepair :: Assertion
testInterruptedTranscriptRepair = do
  let completed = toolCall "completed-1" "work"
      interrupted = toolCall "interrupted-1" "work"
      damaged =
        Transcript $ Seq.fromList
          [ LLM.userText "resume"
          , LLM.assistantAnswer (LLM.chatAnswer "" [completed, interrupted])
          , LLM.toolResult completed "already done"
          ]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  let model _ messages = do
        liftIO $ IORef.modifyIORef' captured (<> [messages])
        S.yield "resumed"
        pure (LLM.chatAnswer "resumed" [])
  (_, result) <- runFailureModel model (runTestAgent [] damaged)
  requests <- IORef.readIORef captured
  case requests of
    [messages] -> do
      toolMessagePairs messages
        @?= [ ("completed-1", "already done")
            , ("interrupted-1", messageText (AgentTranscript.pausedToolResult interrupted))
            ]
    other ->
      assertFailure [i|expected one repaired model request, got #{length other}|]
  assertToolProtocolComplete result.transcript

testToolLimitTranscript :: Assertion
testToolLimitTranscript = do
  answers <- IORef.newIORef
    [ LLM.chatAnswer "" [toolCall "limited-1" "counted", toolCall "limited-2" "counted"]
    ]
  executions <- IORef.newIORef (0 :: Int)
  let counted =
        Tool.tool "counted" Tool.noArguments do
          liftIO $ IORef.modifyIORef' executions (+ 1)
          pure (Agent.toolText "unexpected")
  (outputs, result) <- runFailureModel (scriptedModel answers) do
    runtime <- testRuntime [counted]
    let limited =
          AgentTools.withToolLimit (\_ _ -> False) runtime{AgentCore.maxTurns = 1}
    S.toList (Agent.agentStream limited (startWithUser "limit")) <&> \(chunks S.:> final) ->
      (chunks, final)
  IORef.readIORef executions >>= (@?= 0)
  result.status @?= "tool_limit"
  contentDeltas outputs @?= []
  result.finalText @?= ""
  map fst (toolResults result.transcript) @?= ["limited-1", "limited-2"]
  assertToolProtocolComplete result.transcript

testSchemaResolutionFailure :: Assertion
testSchemaResolutionFailure = do
  modelCalls <- IORef.newIORef (0 :: Int)
  let broken =
        Tool.mapSchemaM
          (\_ _ _ _ -> throwIO (InjectedFailure "schema failure"))
          (Tool.tool "broken" Tool.noArguments (pure (Agent.toolText "unexpected")))
      model _ _ = do
        liftIO $ IORef.modifyIORef' modelCalls (+ 1)
        pure (LLM.chatAnswer "unexpected" [])
  outcome <- runFailureModel model $ trySync do
    void $ S.toList =<< (Agent.agentStream <$> testRuntime [broken] <*> pure (startWithUser "schema"))
  assertBool "schema failure should abort the turn" (isLeft outcome)
  IORef.readIORef modelCalls >>= (@?= 0)

testDuplicateToolSchemas :: Assertion
testDuplicateToolSchemas = do
  modelCalls <- IORef.newIORef (0 :: Int)
  let duplicate =
        Tool.tool "duplicate" Tool.noArguments (pure (Agent.toolText "unexpected"))
      model _ _ = do
        liftIO $ IORef.modifyIORef' modelCalls (+ 1)
        pure (LLM.chatAnswer "unexpected" [])
  outcome <- runFailureModel model $ trySync do
    void $ S.toList =<<
      (Agent.agentStream <$> testRuntime [duplicate, duplicate] <*> pure (startWithUser "duplicates"))
  assertBool "duplicate schemas should abort before transport" (isLeft outcome)
  IORef.readIORef modelCalls >>= (@?= 0)

testDuplicateResponse :: Assertion
testDuplicateResponse = do
  modelRequests <- IORef.newIORef (0 :: Int)
  deliveries <- IORef.newIORef (0 :: Int)
  let model _ _ = do
        liftIO $ IORef.modifyIORef' modelRequests (+ 1)
        let deliver = do
              liftIO $ IORef.atomicModifyIORef' deliveries \count -> (count + 1, ())
              pure (LLM.chatAnswer "once" [])
        firstDelivery <- lift (Async.async deliver)
        secondDelivery <- lift (Async.async deliver)
        answer <- lift (Async.wait firstDelivery)
        void (lift (Async.wait secondDelivery))
        S.yield "once"
        pure answer
  (_, result) <- runFailureModel model (runTestAgent [] (startWithUser "one response"))
  IORef.readIORef deliveries >>= (@?= 2)
  IORef.readIORef modelRequests >>= (@?= 1)
  assistantTexts result.transcript @?= ["once"]

testModelInputFailure :: Assertion
testModelInputFailure = do
  modelCalls <- IORef.newIORef (0 :: Int)
  outcome <- runFailureModel (countingFinalModel modelCalls) $ trySync do
    runtime <- testRuntime []
    let faulting =
          runtime
            { AgentCore.modelInputTranscript = \_ _ ->
                throwIO (InjectedFailure "model input failure")
            }
    void $ S.toList (Agent.agentStream faulting (startWithUser "input"))
  assertBool "model-input failure should escape" (isLeft outcome)
  IORef.readIORef modelCalls >>= (@?= 0)

testModelTurnAcquireFailure :: Assertion
testModelTurnAcquireFailure = do
  modelCalls <- IORef.newIORef (0 :: Int)
  outcome <- runFailureModel (countingFinalModel modelCalls) $ trySync do
    runtime <- testRuntime []
    let faulting =
          runtime
            { AgentCore.aroundModelTurn = \_ _ _ ->
                lift $ throwIO (InjectedFailure "model-turn acquire failure")
            }
    void $ S.toList (Agent.agentStream faulting (startWithUser "model acquire"))
  assertBool "model-turn acquire failure should escape" (isLeft outcome)
  IORef.readIORef modelCalls >>= (@?= 0)

testModelTurnReleaseFailure :: Assertion
testModelTurnReleaseFailure = do
  modelCalls <- IORef.newIORef (0 :: Int)
  outcome <- runFailureModel (countingFinalModel modelCalls) $ trySync do
    runtime@AgentCore.Runtime{AgentCore.aroundModelTurn = innerModelTurn} <- testRuntime []
    let faulting =
          runtime
            { AgentCore.aroundModelTurn = \context agentState action -> do
                void $ innerModelTurn context agentState action
                lift $ throwIO (InjectedFailure "model-turn release failure")
            }
    void $ S.toList (Agent.agentStream faulting (startWithUser "model release"))
  assertBool "model-turn release failure should escape" (isLeft outcome)
  IORef.readIORef modelCalls >>= (@?= 1)

testToolTurnAcquireFailure :: Assertion
testToolTurnAcquireFailure = do
  modelCalls <- IORef.newIORef (0 :: Int)
  executions <- IORef.newIORef (0 :: Int)
  let tool = countedTool executions
  outcome <- runFailureModel (singleToolModel modelCalls (Tool.toolName tool)) $ trySync do
    runtime <- testRuntime [tool]
    let faulting =
          runtime
            { AgentCore.aroundToolTurn = \_ _ _ ->
                throwIO (InjectedFailure "tool-turn acquire failure")
            }
    void $ S.toList (Agent.agentStream faulting (startWithUser "tool acquire"))
  assertBool "tool-turn acquire failure should escape" (isLeft outcome)
  IORef.readIORef modelCalls >>= (@?= 1)
  IORef.readIORef executions >>= (@?= 0)

testToolTurnReleaseFailure :: Assertion
testToolTurnReleaseFailure = do
  modelCalls <- IORef.newIORef (0 :: Int)
  executions <- IORef.newIORef (0 :: Int)
  let tool = countedTool executions
  outcome <- runFailureModel (singleToolModel modelCalls (Tool.toolName tool)) $ trySync do
    runtime@AgentCore.Runtime{AgentCore.aroundToolTurn = innerToolTurn} <- testRuntime [tool]
    let faulting =
          runtime
            { AgentCore.aroundToolTurn = \context request action -> do
                void $ innerToolTurn context request action
                throwIO (InjectedFailure "tool-turn release failure")
            }
    void $ S.toList (Agent.agentStream faulting (startWithUser "tool release"))
  assertBool "tool-turn release failure should escape" (isLeft outcome)
  IORef.readIORef modelCalls >>= (@?= 1)
  IORef.readIORef executions >>= (@?= 1)

testProgramWrapperFailure :: Assertion
testProgramWrapperFailure = do
  modelCalls <- IORef.newIORef (0 :: Int)
  outcome <- runFailureModel (countingFinalModel modelCalls) $ trySync do
    runtime <- testRuntime []
    let faulting =
          runtime
            { AgentCore.aroundProgram = \_ _ ->
                AgentCore.Program (lift $ throwIO (InjectedFailure "program wrapper failure"))
            }
    void $ S.toList (Agent.agentStream faulting (startWithUser "program"))
  assertBool "program wrapper failure should escape" (isLeft outcome)
  IORef.readIORef modelCalls >>= (@?= 0)

testAgentRunReleaseFailure :: Assertion
testAgentRunReleaseFailure = do
  modelCalls <- IORef.newIORef (0 :: Int)
  acquired <- IORef.newIORef (0 :: Int)
  released <- IORef.newIORef (0 :: Int)
  outcome <- runFailureModel (countingFinalModel modelCalls) $ trySync do
    runtime@AgentCore.Runtime{AgentCore.aroundAgentRun = innerAgentRun} <- testRuntime []
    let faulting =
          runtime
            { AgentCore.aroundAgentRun = \context action ->
                StreamUtil.bracketStream
                  (liftIO $ IORef.modifyIORef' acquired (+ 1))
                  (\_ -> do
                    liftIO $ IORef.modifyIORef' released (+ 1)
                    throwIO (InjectedFailure "agent-run release failure"))
                  (\_ -> innerAgentRun context action)
            }
    void $ S.toList (Agent.agentStream faulting (startWithUser "run release"))
  assertBool "agent-run release failure should replace the terminal result" (isLeft outcome)
  IORef.readIORef modelCalls >>= (@?= 1)
  IORef.readIORef acquired >>= (@?= 1)
  IORef.readIORef released >>= (@?= 1)

countingFinalModel :: IORef.IORef Int -> ModelInterpreter
countingFinalModel calls _ _ = do
  liftIO $ IORef.modifyIORef' calls (+ 1)
  S.yield "done"
  pure (LLM.chatAnswer "done" [])

singleToolModel :: IORef.IORef Int -> Text -> ModelInterpreter
singleToolModel calls toolName _ _ = do
  callNumber <- liftIO $ IORef.atomicModifyIORef' calls \current ->
    let next = current + 1
    in (next, next)
  if callNumber == 1
    then pure (LLM.chatAnswer "" [toolCall "middleware-tool" toolName])
    else S.yield "done" $> LLM.chatAnswer "done" []

countedTool :: IORef.IORef Int -> Tool.Tool (Eff TestEffects)
countedTool executions =
  Tool.tool "counted" Tool.noArguments do
    liftIO $ IORef.modifyIORef' executions (+ 1)
    pure (Agent.toolText "counted")

testRetryBackoffCancellation :: Assertion
testRetryBackoffCancellation = do
  attempts <- IORef.newIORef (0 :: Int)
  sleeping <- IORef.newIORef False
  cleaned <- IORef.newIORef False
  let model _ _ =
        Retry.retryLLMStreamRequestWith
          (\_ -> do
            liftIO $ IORef.writeIORef sleeping True
            threadDelay maxBound
              `finally` liftIO (IORef.writeIORef cleaned True))
          "cancelled retry backoff"
          do
            liftIO $ IORef.modifyIORef' attempts (+ 1)
            lift $ throwIO modelTimeout
  outcome <- runFailureModel model $
    Timeout.timeout 1_000_000 do
      worker <- Async.async do
        runtime <- testRuntime []
        S.toList (Agent.agentStream runtime (startWithUser "cancel retry"))
      awaitFlag sleeping
      Async.cancel worker
      result <- Async.waitCatch worker
      awaitFlag cleaned
      pure (isLeft result)
  outcome @?= Just True
  IORef.readIORef attempts >>= (@?= 1)

testSchemaResolutionCancellation :: Assertion
testSchemaResolutionCancellation = do
  started <- IORef.newIORef False
  cleaned <- IORef.newIORef False
  modelCalls <- IORef.newIORef (0 :: Int)
  let blocking =
        Tool.mapSchemaM
          (\_ _ _ schema -> do
            liftIO $ IORef.writeIORef started True
            (threadDelay maxBound
              `finally` liftIO (IORef.writeIORef cleaned True))
            pure (Just schema))
          (Tool.tool "blocking-schema" Tool.noArguments (pure (Agent.toolText "unexpected")))
  outcome <- runFailureModel (countingFinalModel modelCalls) $
    Timeout.timeout 1_000_000 do
      worker <- Async.async do
        runtime <- testRuntime [blocking]
        S.toList (Agent.agentStream runtime (startWithUser "cancel schema"))
      awaitFlag started
      Async.cancel worker
      result <- Async.waitCatch worker
      awaitFlag cleaned
      pure (isLeft result)
  outcome @?= Just True
  IORef.readIORef modelCalls >>= (@?= 0)

testToolInitializationFailure :: Assertion
testToolInitializationFailure = do
  acquired <- IORef.newIORef (0 :: Int)
  released <- IORef.newIORef (0 :: Int)
  modelCalls <- IORef.newIORef (0 :: Int)
  let broken =
        Tool.toolWithRunState
          "broken-init"
          Tool.noArguments
          (\_ ->
            bracket
              (liftIO $ IORef.modifyIORef' acquired (+ 1))
              (\_ -> liftIO $ IORef.modifyIORef' released (+ 1))
              (\_ -> throwIO (InjectedFailure "tool initialization failure")))
          (\() -> pure (Agent.toolText "unexpected"))
  outcome <- runFailureModel (countingFinalModel modelCalls) $
    trySync (void (testRuntime [broken]))
  assertBool "tool initialization failure should escape" (isLeft outcome)
  IORef.readIORef acquired >>= (@?= 1)
  IORef.readIORef released >>= (@?= 1)
  IORef.readIORef modelCalls >>= (@?= 0)

testMalformedStreamChunk :: Assertion
testMalformedStreamChunk =
  case LLMTransport.chatStreamTextFromPayloads True
    [Aeson.object ["choices" Aeson..= ("not-an-array" :: Text)]] of
    Left{} -> pure ()
    Right result ->
      assertFailure [i|malformed chunk unexpectedly parsed as #{show result :: String}|]

testPrematureToolCallEOF :: Assertion
testPrematureToolCallEOF = do
  let payloads =
        [ streamPayload (Aeson.object ["content" Aeson..= ("partial" :: Text)])
        , streamPayload $
            Aeson.object
              [ "tool_calls" Aeson..=
                  [ Aeson.object
                      [ "index" Aeson..= (0 :: Int)
                      , "function" Aeson..=
                          Aeson.object ["arguments" Aeson..= ("{\"x\":" :: Text)]
                      ]
                  ]
              ]
        ]
  case LLMTransport.chatStreamTextFromPayloads True payloads of
    Right (outputs, LLM.ChatFinalAnswer{content}) -> do
      outputs @?= ["partial"]
      content @?= "partial"
    other ->
      assertFailure [i|premature tool call produced #{show other :: String}|]

testDuplicateStreamDelta :: Assertion
testDuplicateStreamDelta = do
  let duplicate = streamPayload (Aeson.object ["content" Aeson..= ("same" :: Text)])
  case LLMTransport.chatStreamTextFromPayloads True [duplicate, duplicate] of
    Right (outputs, LLM.ChatFinalAnswer{content}) -> do
      outputs @?= ["same", "same"]
      content @?= Text.concat outputs
    other ->
      assertFailure [i|duplicate content deltas produced #{show other :: String}|]

testOutOfOrderToolDeltas :: Assertion
testOutOfOrderToolDeltas = do
  let toolDelta index callId name arguments =
        Aeson.object
          [ "index" Aeson..= (index :: Int)
          , "id" Aeson..= (callId :: Text)
          , "function" Aeson..=
              Aeson.object
                [ "name" Aeson..= (name :: Text)
                , "arguments" Aeson..= (arguments :: Text)
                ]
          ]
      argumentDelta index arguments =
        Aeson.object
          [ "index" Aeson..= (index :: Int)
          , "function" Aeson..=
              Aeson.object ["arguments" Aeson..= (arguments :: Text)]
          ]
      payloads =
        [ streamPayload $ Aeson.object
            ["tool_calls" Aeson..= [toolDelta 1 "call-b" "second" "{\"b\":"]]
        , streamPayload $ Aeson.object
            ["tool_calls" Aeson..= [toolDelta 0 "call-a" "first" "{\"a\":"]]
        , streamPayload $ Aeson.object
            [ "tool_calls" Aeson..=
                [ argumentDelta 1 "2}"
                , argumentDelta 0 "1}"
                ]
            ]
        ]
  case LLMTransport.chatStreamTextFromPayloads False payloads of
    Right ([], LLM.ChatToolRequest{toolCalls}) ->
      map (\call -> (call.id, call.name, call.arguments)) (toList toolCalls)
        @?= [ ("call-a", "first", "{\"a\":1}")
            , ("call-b", "second", "{\"b\":2}")
            ]
    other ->
      assertFailure [i|out-of-order tool deltas produced #{show other :: String}|]

streamPayload :: Aeson.Value -> Aeson.Value
streamPayload delta =
  Aeson.object
    [ "choices" Aeson..=
        [Aeson.object ["delta" Aeson..= delta]]
    ]

testToolResultNormalization :: Assertion
testToolResultNormalization = do
  let firstCall = toolCall "repair-a" "repair"
      secondCall = toolCall "repair-b" "repair"
      unknownCall = toolCall "unknown-result" "repair"
      damaged =
        Transcript $ Seq.fromList
          [ LLM.toolResult unknownCall "orphan before any assistant"
          , LLM.userText "repair this"
          , LLM.assistantAnswer (LLM.chatAnswer "" [firstCall, secondCall])
          , LLM.toolResult secondCall "kept"
          , LLM.toolResult secondCall "duplicate"
          , LLM.toolResult unknownCall "unknown"
          ]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  (_, result) <- runFailureModel (capturingFinalModel captured) (runTestAgent [] damaged)
  IORef.readIORef captured >>= \case
    [messages] ->
      toolMessagePairs messages
        @?= [ ("repair-a", messageText (AgentTranscript.pausedToolResult firstCall))
            , ("repair-b", "kept")
            ]
    other ->
      assertFailure [i|expected one normalized request, got #{length other}|]
  assertToolProtocolComplete result.transcript

testReversedToolResultRepair :: Assertion
testReversedToolResultRepair = do
  let firstCall = toolCall "ordered-a" "repair"
      secondCall = toolCall "ordered-b" "repair"
      damaged =
        Transcript $ Seq.fromList
          [ LLM.userText "repair order"
          , LLM.assistantAnswer (LLM.chatAnswer "" [firstCall, secondCall])
          , LLM.toolResult secondCall "second"
          , LLM.toolResult firstCall "first"
          ]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  void $ runFailureModel (capturingFinalModel captured) (runTestAgent [] damaged)
  IORef.readIORef captured >>= \case
    [messages] ->
      toolMessagePairs messages @?= [("ordered-a", "first"), ("ordered-b", "second")]
    other ->
      assertFailure [i|expected one reordered request, got #{length other}|]

testConsecutiveInterruptedTurns :: Assertion
testConsecutiveInterruptedTurns = do
  let firstCall = toolCall "old-a" "repair"
      secondCall = toolCall "old-b" "repair"
      damaged =
        Transcript $ Seq.fromList
          [ LLM.userText "first"
          , LLM.assistantAnswer (LLM.chatAnswer "" [firstCall])
          , LLM.assistantAnswer (LLM.chatAnswer "" [secondCall])
          ]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  (_, result) <- runFailureModel (capturingFinalModel captured) (runTestAgent [] damaged)
  IORef.readIORef captured >>= \case
    [messages] -> assertToolMessagesComplete "consecutive interrupted turns" messages
    other -> assertFailure [i|expected one repaired request, got #{length other}|]
  assertToolProtocolComplete result.transcript

testDuplicateToolCallIds :: Assertion
testDuplicateToolCallIds =
  testInvalidFreshToolCallIds
    [toolCall "duplicate-id" "counted", toolCall "duplicate-id" "counted"]

testEmptyToolCallId :: Assertion
testEmptyToolCallId =
  testInvalidFreshToolCallIds [toolCall "" "counted"]

testInvalidFreshToolCallIds :: [LLM.ToolCall] -> Assertion
testInvalidFreshToolCallIds calls = do
  executions <- IORef.newIORef (0 :: Int)
  modelCalls <- IORef.newIORef (0 :: Int)
  let model _ _ = do
        callNumber <- liftIO $ IORef.atomicModifyIORef' modelCalls \current ->
          let next = current + 1
          in (next, next)
        if callNumber == 1
          then pure (LLM.chatAnswer "" calls)
          else pure (LLM.chatAnswer "unexpected continuation" [])
  outcome <- runFailureModel model $ trySync do
    runtime <- testRuntime [countedTool executions]
    S.toList (Agent.agentStream runtime (startWithUser "invalid ids"))
  case outcome of
    Right (_outputs S.:> result) ->
      result.status @?= "model_protocol_error"
    Left err ->
      assertFailure [i|invalid tool-call ids failed exceptionally: #{displayException err}|]
  IORef.readIORef executions >>= (@?= 0)
  IORef.readIORef modelCalls >>= (@?= 1)

capturingFinalModel :: IORef.IORef [[LLM.ChatMessage]] -> ModelInterpreter
capturingFinalModel captured _ messages = do
  liftIO $ IORef.modifyIORef' captured (<> [messages])
  S.yield "done"
  pure (LLM.chatAnswer "done" [])

testContinuationOneShotFailures :: Assertion
testContinuationOneShotFailures = do
  answers <- IORef.newIORef
    [ LLM.chatAnswer "" [toolCallWithArguments "same" "capture_continuation" "{}"]
    , LLM.chatAnswer "" [toolCallWithArguments "same" "capture_continuation" "{}"]
    , LLM.chatAnswer "" [resumeCall "resume-once" "same" (Aeson.String "value")]
    , LLM.chatAnswer "" [resumeCall "resume-twice" "same" Aeson.Null]
    , LLM.chatAnswer "done" []
    ]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  (_, result) <- runFailureModel (scriptedCapturingModel answers captured) do
    runtime <- testRuntime
      [ ContinuationTools.captureContinuationTool
      , ContinuationTools.resumeContinuationTool
      ]
    S.toList (Agent.agentStream (Continuation.withContinuations runtime) (startWithUser "continuations"))
      <&> \(outputs S.:> final) -> (outputs, final)
  requests <- IORef.readIORef captured
  let toolTexts = concatMap (map snd . toolMessagePairs) requests
  assertBool "duplicate capture should be rejected" $
    any ("Continuation id already exists" `Text.isInfixOf`) toolTexts
  assertBool "consumed continuation should reject its second resume" $
    any ("Continuation not found" `Text.isInfixOf`) toolTexts
  assertToolProtocolComplete result.transcript

testContinuationMalformedAndConcurrent :: Assertion
testContinuationMalformedAndConcurrent = do
  answers <- IORef.newIORef
    [ LLM.chatAnswer ""
        [toolCallWithArguments "malformed" "capture_continuation" "[]"]
    , LLM.chatAnswer ""
        [ toolCall "capture-with-sibling" "capture_continuation"
        , toolCall "side-effect" "side-effect"
        ]
    , LLM.chatAnswer "done" []
    ]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  executions <- IORef.newIORef (0 :: Int)
  let sideEffect =
        Tool.tool "side-effect" Tool.noArguments do
          liftIO $ IORef.modifyIORef' executions (+ 1)
          pure (Agent.toolText "executed")
  (_, result) <- runFailureModel (scriptedCapturingModel answers captured) do
    runtime <- testRuntime
      [ ContinuationTools.captureContinuationTool
      , ContinuationTools.resumeContinuationTool
      , sideEffect
      ]
    S.toList (Agent.agentStream (Continuation.withContinuations runtime) (startWithUser "bad controls"))
      <&> \(outputs S.:> final) -> (outputs, final)
  IORef.readIORef executions >>= (@?= 0)
  requests <- IORef.readIORef captured
  let toolTexts = concatMap (map snd . toolMessagePairs) requests
  assertBool "malformed arguments should be visible" $
    any ("Invalid continuation arguments" `Text.isInfixOf`) toolTexts
  length (filter ("must be called alone" `Text.isInfixOf`) toolTexts) @?= 2
  assertToolProtocolComplete result.transcript

testContinuationCaptureCancellation :: Assertion
testContinuationCaptureCancellation = do
  answers <- IORef.newIORef
    [LLM.chatAnswer "" [toolCall "capture-cancelled" "capture_continuation"]]
  outcome <- runFailureModel (scriptedModel answers) $
    Timeout.timeout 1_000_000 do
      phaseCompleted <- MVar.newEmptyMVar
      phaseReleased <- MVar.newEmptyMVar
      runtime@AgentCore.Runtime{AgentCore.aroundToolTurn = innerToolTurn} <-
        testRuntime
          [ ContinuationTools.captureContinuationTool
          , ContinuationTools.resumeContinuationTool
          ]
      let paused =
            runtime
              { AgentCore.aroundToolTurn = \context request action ->
                  (do
                    result <- innerToolTurn context request action
                    MVar.putMVar phaseCompleted ()
                    threadDelay maxBound
                    pure result)
                    `finally` MVar.putMVar phaseReleased ()
              }
      worker <- Async.async $
        S.toList $
          Agent.agentStream
            (Continuation.withContinuations paused)
            (startWithUser "cancel capture")
      MVar.takeMVar phaseCompleted
      Async.cancel worker
      result <- Async.waitCatch worker
      MVar.takeMVar phaseReleased
      pure (isLeft result)
  outcome @?= Just True

resumeCall :: Text -> Text -> Aeson.Value -> LLM.ToolCall
resumeCall callId continuationId value =
  toolCallWithArguments callId "resume_continuation" . jsonText $
    Aeson.object
      [ "continuation_id" Aeson..= continuationId
      , "value" Aeson..= value
      ]

scriptedCapturingModel
  :: IORef.IORef [LLM.ChatAnswer]
  -> IORef.IORef [[LLM.ChatMessage]]
  -> ModelInterpreter
scriptedCapturingModel answers captured schemas messages = do
  liftIO $ IORef.modifyIORef' captured (<> [messages])
  scriptedModel answers schemas messages

testCompactionMediaFailure :: Assertion
testCompactionMediaFailure = do
  let largeResult = Text.replicate 5000 "x"
  answers <- IORef.newIORef
    [ LLM.chatAnswer "" [toolCall "large-failure" "large"]
    , LLM.chatAnswer "unexpected" []
    ]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  executions <- IORef.newIORef (0 :: Int)
  stores <- IORef.newIORef (0 :: Int)
  outcome <-
    runFailureModelWithMedia
      (runMediaStoreWith \_ -> do
        liftIO $ IORef.modifyIORef' stores (+ 1)
        throwIO (InjectedFailure "media store failure"))
      (scriptedCapturingModel answers captured)
      $ trySync do
          runtime <- compactingRuntime [largeResultTool executions largeResult []]
          void $ S.toList (Agent.agentStream runtime (startWithUser "large failure"))
  assertBool "media-store failure should abort the tool turn" (isLeft outcome)
  IORef.readIORef executions >>= (@?= 1)
  IORef.readIORef stores >>= (@?= 1)
  fmap length (IORef.readIORef captured) >>= (@?= 1)

testCompactionMediaUnavailable :: Assertion
testCompactionMediaUnavailable = do
  let largeResult = Text.replicate 5000 "x"
  answers <- IORef.newIORef
    [ LLM.chatAnswer "" [toolCall "large-unavailable" "large"]
    , LLM.chatAnswer "done" []
    ]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  executions <- IORef.newIORef (0 :: Int)
  stores <- IORef.newIORef (0 :: Int)
  (_, result) <-
    runFailureModelWithMedia
      (runMediaStoreWith \_ -> do
        liftIO $ IORef.modifyIORef' stores (+ 1)
        pure Nothing)
      (scriptedCapturingModel answers captured)
      do
        runtime <- compactingRuntime [largeResultTool executions largeResult []]
        S.toList (Agent.agentStream runtime (startWithUser "large unavailable"))
          <&> \(outputs S.:> final) -> (outputs, final)
  IORef.readIORef executions >>= (@?= 1)
  IORef.readIORef stores >>= (@?= 1)
  IORef.readIORef captured >>= \case
    [_firstRequest, secondRequest] ->
      List.lookup "large-unavailable" (toolMessagePairs secondRequest) @?= Just largeResult
    other ->
      assertFailure [i|expected two model requests, got #{length other}|]
  case List.lookup "large-unavailable" (toolResults result.transcript) of
    Just durable ->
      assertBool "durable result should use the unavailable omission marker" $
        "[tool result omitted; media_id=unavailable" `Text.isPrefixOf` durable
    Nothing ->
      assertFailure "durable compacted result is missing"

testHugeResultImmediateCompaction :: Assertion
testHugeResultImmediateCompaction = do
  let hugeResult = Text.replicate 12000 "h"
  answers <- IORef.newIORef
    [ LLM.chatAnswer "" [toolCall "huge-1" "large"]
    , LLM.chatAnswer "done" []
    ]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  executions <- IORef.newIORef (0 :: Int)
  (_, result) <-
    runFailureModelWithMedia
      (runMediaStoreWith \_ -> pure (Just "media:huge-result"))
      (scriptedCapturingModel answers captured)
      do
        runtime <- compactingRuntime [largeResultTool executions hugeResult []]
        S.toList (Agent.agentStream runtime (startWithUser "huge"))
          <&> \(outputs S.:> final) -> (outputs, final)
  IORef.readIORef captured >>= \case
    [_firstRequest, secondRequest] ->
      case List.lookup "huge-1" (toolMessagePairs secondRequest) of
        Just immediate -> do
          assertBool "immediate huge result should already be omitted" $
            "[tool result omitted; media_id=huge-result" `Text.isPrefixOf` immediate
          assertBool "immediate request must not retain the huge body" $
            not (hugeResult `Text.isInfixOf` immediate)
        Nothing ->
          assertFailure "immediate huge result is missing"
    other ->
      assertFailure [i|expected two model requests, got #{length other}|]
  assertToolProtocolComplete result.transcript

testLargeImageResultCompaction :: Assertion
testLargeImageResultCompaction = do
  let largeResult = Text.replicate 5000 "i"
      imageUrl = "https://example.test/result.png"
  answers <- IORef.newIORef
    [ LLM.chatAnswer "" [toolCall "image-large-1" "large"]
    , LLM.chatAnswer "done" []
    ]
  captured <- IORef.newIORef ([] :: [[LLM.ChatMessage]])
  executions <- IORef.newIORef (0 :: Int)
  (_, result) <-
    runFailureModelWithMedia
      (runMediaStoreWith \_ -> pure (Just "media:image-large"))
      (scriptedCapturingModel answers captured)
      do
        runtime <- compactingRuntime [largeResultTool executions largeResult [imageUrl]]
        S.toList (Agent.agentStream runtime (startWithUser "large image"))
          <&> \(outputs S.:> final) -> (outputs, final)
  IORef.readIORef captured >>= \case
    [_firstRequest, secondRequest] -> do
      List.lookup "image-large-1" (toolMessagePairs secondRequest) @?= Just largeResult
      imageUrlsFromMessages secondRequest @?= [imageUrl]
    other ->
      assertFailure [i|expected two model requests, got #{length other}|]
  case List.lookup "image-large-1" (toolResults result.transcript) of
    Just durable ->
      assertBool "durable image tool text should be omitted" $
        "[tool result omitted; media_id=image-large" `Text.isPrefixOf` durable
    Nothing ->
      assertFailure "durable image result is missing"
  imageUrlsFromMessages (transcriptMessages result.transcript) @?= [imageUrl]

testToolObserverFailure :: Assertion
testToolObserverFailure = do
  answers <- IORef.newIORef
    [ LLM.chatAnswer "" [toolCall "observed-1" "counted"]
    , LLM.chatAnswer "done" []
    ]
  events <- IORef.newIORef ([] :: [Agent.Event])
  executions <- IORef.newIORef (0 :: Int)
  let observer event = do
        liftIO $ IORef.modifyIORef' events (<> [event])
        case event of
          Agent.ToolCallFinished{} ->
            throwIO (InjectedFailure "tool observer failure")
          _ ->
            pure Observation.emptyObservationContext
  (_, result) <- runFailureModel (scriptedModel answers) do
    runtime <- observedRuntime observer [countedTool executions]
    S.toList (Agent.agentStream runtime (startWithUser "observer tool"))
      <&> \(outputs S.:> final) -> (outputs, final)
  IORef.readIORef executions >>= (@?= 1)
  case List.lookup "observed-1" (toolResults result.transcript) of
    Just failure ->
      assertBool "observer failure should become the tool result" $
        "tool observer failure" `Text.isInfixOf` failure
    Nothing ->
      assertFailure "observer failure result is missing"

testRunStartObserverFailure :: Assertion
testRunStartObserverFailure = do
  events <- IORef.newIORef ([] :: [Agent.Event])
  modelCalls <- IORef.newIORef (0 :: Int)
  let observer event = do
        liftIO $ IORef.modifyIORef' events (<> [event])
        case event of
          Agent.AgentRunStarted{} ->
            throwIO (InjectedFailure "run-start observer failure")
          _ ->
            pure Observation.emptyObservationContext
  outcome <- runFailureModel (countingFinalModel modelCalls) $ trySync do
    runtime <- observedRuntime observer []
    void $ S.toList (Agent.agentStream runtime (startWithUser "observer start"))
  assertBool "run-start observer failure should escape" (isLeft outcome)
  IORef.readIORef modelCalls >>= (@?= 0)
  IORef.readIORef events >>= \observed -> do
    assertBool "run-start event should be attempted" (any isRunStarted observed)
    assertBool "run interruption should be emitted" (any isRunInterrupted observed)

testRunFinishedObserverFailure :: Assertion
testRunFinishedObserverFailure = do
  events <- IORef.newIORef ([] :: [Agent.Event])
  modelCalls <- IORef.newIORef (0 :: Int)
  let observer event = do
        liftIO $ IORef.modifyIORef' events (<> [event])
        case event of
          Agent.AgentRunFinished{} ->
            throwIO (InjectedFailure "run-finished observer failure")
          _ ->
            pure Observation.emptyObservationContext
  outcome <- runFailureModel (countingFinalModel modelCalls) $ trySync do
    runtime <- observedRuntime observer []
    void $ S.toList (Agent.agentStream runtime (startWithUser "observer finish"))
  assertBool "run-finished observer failure should escape" (isLeft outcome)
  IORef.readIORef modelCalls >>= (@?= 1)
  IORef.readIORef events >>= \observed -> do
    assertBool "run-finished event should be attempted" (any isRunFinished observed)
    assertBool "run interruption should follow" (any isRunInterrupted observed)

compactingRuntime
  :: [Tool.Tool (Eff TestEffects)]
  -> Eff TestEffects (AgentCore.Runtime '[] (Eff TestEffects))
compactingRuntime tools =
  ToolResultCompaction.withToolResultCompaction
    <$> testRuntimeFor @'[ToolResultObservation TestEffects] tools

observedRuntime
  :: Agent.Observer Observation.ObservationContext (Eff TestEffects)
  -> [Tool.Tool (Eff TestEffects)]
  -> Eff TestEffects (AgentCore.Runtime '[] (Eff TestEffects))
observedRuntime observer tools = do
  runtime <-
    testRuntimeFor
      @'[ Observation.ObservationContext
        , EventObservation TestEffects
        , ToolResultObservation TestEffects
        ]
      tools
  pure $
    AgentTools.withToolFailureRecovery
      . ToolResultCompaction.withToolResultCompaction
      . Observation.withObservation observer
      $ runtime

largeResultTool
  :: IORef.IORef Int
  -> Text
  -> [Text]
  -> Tool.Tool (Eff TestEffects)
largeResultTool executions result imageUrls =
  Tool.tool "large" Tool.noArguments do
    liftIO $ IORef.modifyIORef' executions (+ 1)
    pure (Agent.toolTextWithImages result imageUrls)

imageUrlsFromMessages :: [LLM.ChatMessage] -> [Text]
imageUrlsFromMessages messages =
  [ url
  | message <- messages
  , Just (LLM.PartsContent parts) <- [message.content]
  , LLM.ImageUrlPart url <- parts
  ]

transcriptMessages :: Transcript -> [LLM.ChatMessage]
transcriptMessages (Transcript messages) =
  Foldable.toList messages

isRunStarted :: Agent.Event -> Bool
isRunStarted Agent.AgentRunStarted{} = True
isRunStarted _ = False

isRunFinished :: Agent.Event -> Bool
isRunFinished Agent.AgentRunFinished{} = True
isRunFinished _ = False

isRunInterrupted :: Agent.Event -> Bool
isRunInterrupted Agent.AgentRunInterrupted{} = True
isRunInterrupted _ = False

runMediaStoreWith
  :: (Media.MediaObject -> Eff BaseEffects (Maybe Text))
  -> Eff (Media.Media ': BaseEffects) a
  -> Eff BaseEffects a
runMediaStoreWith store =
  interpret \_ -> \case
    Media.StoreMediaObject mediaObject ->
      store mediaObject
    Media.StoreMediaObjectFromSource _ mediaObject ->
      store mediaObject
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
      pure Media.MediaCacheStats
        { files = 0
        , existingFiles = 0
        , missingFiles = 0
        , totalBytes = 0
        , sources = 0
        , platformRefs = 0
        }
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

testFaultPlan :: FaultPlan -> Assertion
testFaultPlan plan = do
  logicalTurns <- IORef.newIORef (0 :: Int)
  firstAttempts <- IORef.newIORef (0 :: Int)
  secondAttempts <- IORef.newIORef (0 :: Int)
  deliveries <- IORef.newIORef (0 :: Int)
  executions <- IORef.newIORef ([] :: [Text])
  captured <- IORef.newIORef ([] :: [(Int, [LLM.ChatMessage])])
  let calls =
        zipWith
          (\index _ -> toolCall [i|plan-#{index}|] [i|tool-#{index}|])
          [(1 :: Int) ..]
          plan.failingTools
      definitions =
        List.zipWith3
          (planTool executions)
          [(1 :: Int) ..]
          plan.failingTools
          calls
      model _ messages = do
        logicalTurn <- liftIO $ IORef.atomicModifyIORef' logicalTurns \current ->
          let next = current + 1
          in (next, next)
        let (timeouts, attempts, answer) =
              case logicalTurn of
                1 -> (plan.firstTurnTimeouts, firstAttempts, LLM.chatAnswer "" calls)
                _ -> (plan.secondTurnTimeouts, secondAttempts, LLM.chatAnswer "done" [])
        Retry.retryLLMStreamRequestWith
          (\_ -> pure ())
          "cross-product fault plan"
          do
            attempt <- liftIO $ IORef.atomicModifyIORef' attempts \current ->
              let next = current + 1
              in (next, next)
            liftIO $ IORef.modifyIORef' captured (<> [(logicalTurn, messages)])
            if attempt <= timeouts
              then lift $ throwIO modelTimeout
              else do
                delivered <- replicateM plan.terminalDeliveries do
                  delivery <- lift $ Async.async do
                    liftIO $ IORef.atomicModifyIORef' deliveries \count -> (count + 1, ())
                    pure answer
                  lift (Async.wait delivery)
                let accepted = fromMaybe answer (viaNonEmpty head delivered)
                case accepted of
                  LLM.ChatFinalAnswer{content} -> S.yield content
                  LLM.ChatToolRequest{} -> pure ()
                pure accepted
      initialTranscript
        | plan.startDamaged =
            let interrupted = toolCall "old-interrupted" "old-tool"
            in Transcript $ Seq.fromList
                [ LLM.userText "old request"
                , LLM.assistantAnswer (LLM.chatAnswer "" [interrupted])
                ]
        | otherwise =
            startWithUser "fault plan"
  outcome <- runFailureModel model $ try @SomeException do
    runtime <- testRuntime definitions
    let installed
          | plan.stopAtToolLimit =
              AgentTools.withToolLimit (\_ _ -> False) runtime{AgentCore.maxTurns = 1}
          | otherwise =
              runtime
    S.toList (Agent.agentStream installed initialTranscript)
  observedFirstAttempts <- IORef.readIORef firstAttempts
  observedSecondAttempts <- IORef.readIORef secondAttempts
  observedDeliveries <- IORef.readIORef deliveries
  observedExecutions <- IORef.readIORef executions
  observedTurns <- IORef.readIORef logicalTurns
  observedRequests <- IORef.readIORef captured
  let firstExhausted = plan.firstTurnTimeouts >= 4
      secondReached = not firstExhausted && not plan.stopAtToolLimit
      secondExhausted = secondReached && plan.secondTurnTimeouts >= 4
      expectedFirstAttempts = min 4 (plan.firstTurnTimeouts + 1)
      expectedSecondAttempts
        | secondReached = min 4 (plan.secondTurnTimeouts + 1)
        | otherwise = 0
      expectedTurns
        | secondReached = 2
        | otherwise = 1
      successfulTurns =
        fromEnum (not firstExhausted)
          + fromEnum (secondReached && not secondExhausted)
      expectedExecutions
        | firstExhausted || plan.stopAtToolLimit = []
        | otherwise = map (.name) calls
      label = show plan
  assertEqual (label <> ": first-turn attempts") expectedFirstAttempts observedFirstAttempts
  assertEqual (label <> ": second-turn attempts") expectedSecondAttempts observedSecondAttempts
  assertEqual (label <> ": logical turns") expectedTurns observedTurns
  assertEqual
    (label <> ": duplicate terminal deliveries")
    (successfulTurns * plan.terminalDeliveries)
    observedDeliveries
  assertEqual (label <> ": tool executions") (sort expectedExecutions) (sort observedExecutions)
  assertRetryRequestsStable label observedRequests
  traverse_ (assertToolMessagesComplete label . snd) observedRequests
  case (firstExhausted || secondExhausted, outcome) of
    (True, Left{}) ->
      pure ()
    (True, Right{}) ->
      assertFailure (label <> ": exhausted retry plan unexpectedly completed")
    (False, Left err) ->
      assertFailure (label <> ": recoverable plan failed: " <> displayException err)
    (False, Right (_outputs S.:> result)) -> do
      let expectedStatus
            | plan.stopAtToolLimit = "tool_limit"
            | otherwise = "answered"
          currentResults =
            filter (Text.isPrefixOf "plan-" . fst) (toolResults result.transcript)
      assertEqual (label <> ": result status") expectedStatus result.status
      assertEqual (label <> ": current tool-result ids") (map (.id) calls) (map fst currentResults)
      assertToolProtocolCompleteWith label result.transcript

planTool
  :: IORef.IORef [Text]
  -> Int
  -> Bool
  -> LLM.ToolCall
  -> Tool.Tool (Eff TestEffects)
planTool executions index fails call =
  Tool.tool call.name Tool.noArguments do
    liftIO $ IORef.atomicModifyIORef' executions \calls -> (call.name : calls, ())
    if fails
      then throwIO (InjectedFailure [i|planned tool failure #{index}|])
      else pure (Agent.toolText [i|tool #{index} succeeded|])

assertRetryRequestsStable :: String -> [(Int, [LLM.ChatMessage])] -> Assertion
assertRetryRequestsStable label requests =
  for_ (List.groupBy ((==) `on` fst) requests) \attempts ->
    case map (Aeson.toJSON . snd) attempts of
      [] ->
        pure ()
      initial : rest ->
        assertBool (label <> ": retries changed their transcript") (all (== initial) rest)

assertToolMessagesComplete :: String -> [LLM.ChatMessage] -> Assertion
assertToolMessagesComplete label =
  go
  where
    go [] =
      pure ()
    go (message : rest)
      | message.role == "assistant"
      , not (null message.toolCalls) = do
          let (results, remaining) = span ((== "tool") . (.role)) rest
          assertEqual
            (label <> ": model request has an incomplete tool exchange")
            (map (.id) message.toolCalls)
            (mapMaybe (.toolCallId) results)
          go remaining
      | otherwise =
          go rest

runTestAgent
  :: [Tool.Tool (Eff TestEffects)]
  -> Transcript
  -> Eff TestEffects ([Agent.Output], Agent.Result)
runTestAgent tools transcript = do
  runtime <- testRuntime tools
  S.toList (Agent.agentStream runtime transcript) <&> \(outputs S.:> result) ->
    (outputs, result)

testRuntime :: [Tool.Tool (Eff TestEffects)] -> Eff TestEffects (AgentCore.Runtime '[] (Eff TestEffects))
testRuntime =
  testRuntimeFor

testRuntimeFor
  :: forall context.
     [Tool.Tool (Eff TestEffects)]
  -> Eff TestEffects (AgentCore.Runtime context (Eff TestEffects))
testRuntimeFor tools = do
  runningTools <- traverse (ToolRegistry.startToolRun testContext) tools
  pure $
    AgentTools.withToolFailureRecovery
      AgentCore.Runtime
        { runId = "failure-spec"
        , toolCallMetadata =
            Agent.ToolCallMetadata
              { agentRunId = "failure-spec"
              , originRunId = "failure-spec"
              , resourceOwner = Nothing
              }
        , context = testContext
        , tools
        , exposedTools = tools
        , runningTools
        , dispatchToolCall = \metadata ->
            ToolRegistry.runToolCall testContext metadata tools runningTools
        , maxTurns = 4
        , modelInputTranscript = \_ agentState -> pure agentState.transcript
        , aroundProgram = \_ program -> program
        , aroundAgentRun = \_ stream -> stream
        , aroundModelTurn = \_ agentState action -> action agentState
        , aroundToolTurn = \_ _ action -> action
        , aroundControlCall = \_ _ _ action -> action
        , aroundToolCall = \_ _ _ action -> action
        }

runFailureModel :: ModelInterpreter -> Eff TestEffects a -> IO a
runFailureModel =
  runFailureModelWithMedia Media.runMediaPassthrough

runFailureModelWithMedia
  :: (forall result. Eff (Media.Media ': BaseEffects) result -> Eff BaseEffects result)
  -> ModelInterpreter
  -> Eff TestEffects a
  -> IO a
runFailureModelWithMedia runMedia model =
  runEff
    . runConcurrent
    . startKatipE "failure-spec" "test"
    . runTimeout
    . runMedia
    . LLMTest.runLLMWith
        (\_ -> pure "unused")
        (\_ _ -> pure "unused")
        (\_ _ _ _ -> pure "unused")
        (\_ _ -> pure "unused")
        model

scriptedModel
  :: IORef.IORef [LLM.ChatAnswer]
  -> ModelInterpreter
scriptedModel answers _ _ = do
  answer <- liftIO (popAnswer answers)
  case answer of
    LLM.ChatFinalAnswer{content}
      | not (Text.null content) -> S.yield content
    LLM.ChatToolRequest{content}
      | not (Text.null content) -> S.yield content
    _ ->
      pure ()
  pure answer

popAnswer :: IORef.IORef [LLM.ChatAnswer] -> IO LLM.ChatAnswer
popAnswer answers =
  IORef.atomicModifyIORef' answers \case
    [] ->
      error "fault model received an unexpected request"
    answer : rest ->
      (rest, answer)

toolCall :: Text -> Text -> LLM.ToolCall
toolCall callId name =
  toolCallWithArguments callId name "{}"

toolCallWithArguments :: Text -> Text -> Text -> LLM.ToolCall
toolCallWithArguments callId name arguments =
  LLM.ToolCall
    { id = callId
    , name
    , arguments
    }

contentDeltas :: [Agent.Output] -> [Text]
contentDeltas =
  mapMaybe \case
    Agent.ContentDelta content -> Just content
    _ -> Nothing

assistantTexts :: Transcript -> [Text]
assistantTexts (Transcript messages) =
  [ text
  | message <- Foldable.toList messages
  , message.role == "assistant"
  , Just (LLM.TextContent text) <- [message.content]
  ]

toolResults :: Transcript -> [(Text, Text)]
toolResults (Transcript messages) =
  toolMessagePairs (Foldable.toList messages)

toolMessagePairs :: [LLM.ChatMessage] -> [(Text, Text)]
toolMessagePairs messages =
  [ (callId, text)
  | message <- messages
  , message.role == "tool"
  , Just callId <- [message.toolCallId]
  , Just (LLM.TextContent text) <- [message.content]
  ]

messageText :: LLM.ChatMessage -> Text
messageText message =
  case message.content of
    Just (LLM.TextContent text) -> text
    _ -> ""

transcriptRoles :: Transcript -> [Text]
transcriptRoles (Transcript messages) =
  map (.role) (Foldable.toList messages)

assertToolProtocolComplete :: Transcript -> Assertion
assertToolProtocolComplete =
  assertToolProtocolCompleteWith "tool protocol"

assertToolProtocolCompleteWith :: String -> Transcript -> Assertion
assertToolProtocolCompleteWith label (Transcript messages) =
  go (Foldable.toList messages)
  where
    go [] =
      pure ()
    go (message : rest)
      | message.role == "assistant"
      , not (null message.toolCalls) = do
          let (results, remaining) = span ((== "tool") . (.role)) rest
          assertEqual
            (label <> ": incomplete tool exchange")
            (map (.id) message.toolCalls)
            (mapMaybe (.toolCallId) results)
          go remaining
      | otherwise =
          go rest

awaitFlag :: (Concurrent :> es, IOE :> es) => IORef.IORef Bool -> Eff es ()
awaitFlag flag =
  liftIO (IORef.readIORef flag) >>= \case
    True -> pure ()
    False -> threadDelay 1_000 >> awaitFlag flag

testContext :: Agent.Context
testContext =
  Agent.Context
    { message =
        IncomingMessage
          { eventKind = IncomingMessageCreated
          , platform = PlatformRPC
          , kind = ChatPrivate
          , chatId = Just 1
          , chatAliases = []
          , digest = emptyMessageDigest
          , senderId = Just "failure-spec"
          , senderUsername = Nothing
          , messageId = Just "failure-spec"
          , replyToMessageId = Nothing
          , mentions = []
          , mentionUsernames = []
          , imageUrls = []
          , files = []
          , text = "fault injection"
          , raw = Aeson.Null
          }
    , input = inputWithImages "fault injection" []
    , superuser = False
    , systemContext = ""
    , askCommand = "!ask"
    , toolConfig = Agent.defaultToolConfig
    }

modelTimeout :: HTTP.HttpException
modelTimeout =
  HTTP.HttpExceptionRequest HTTP.defaultRequest HTTP.ResponseTimeout
