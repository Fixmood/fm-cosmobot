{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Bot.Agent.Middleware.Python
  ( interpretPython
  , withPythonMiddleware
  , withPythonRunner
  )
where

import Bot.Agent.Control (finishToolTurn)
import Bot.Agent.Core
import Bot.Agent.Failure (budgetExhaustedFailure)
import Bot.Agent.Program.Python
import Bot.Agent.Tool (toolName)
import Bot.Agent.Tools.Python
import Bot.Agent.Types
import Bot.Core.Message (IncomingMessage)
import qualified Bot.Effect.Agent as Agent
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Bot.Util.HList as HList
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Effectful.Concurrent.Async as Async
import qualified Effectful.Concurrent.MVar as MVar

withPythonMiddleware
  :: (Agent.Agent :> es, Concurrent :> es)
  => ( (Int -> NonEmpty PythonToolCall -> Eff es (NonEmpty ToolResult))
       -> IncomingMessage
       -> Maybe Concurrency.Handle
       -> PythonRequest
       -> Eff es PythonExit
     )
  -> Eff es a
  -> Eff es a
withPythonMiddleware runPython =
  interpose \localEnv -> \case
    Agent.RunAgent runtime use ->
      localLiftUnlift localEnv (ConcUnlift Persistent Unlimited) \liftLocal unliftLocal ->
        let adapted maxCalls outerCallId runOne message resourceOwner pythonRequest =
              liftLocal do
                nestedState <- MVar.newMVar (NestedToolState maxCalls 1)
                runPython
                  (runPythonTools outerCallId nestedState (unliftLocal . runOne))
                  message
                  resourceOwner
                  pythonRequest
        in passthrough localEnv (Agent.RunAgent (withPythonProgram adapted runtime) use)

withPythonRunner
  :: Concurrent :> es
  => ( (Int -> NonEmpty PythonToolCall -> Eff es (NonEmpty ToolResult))
       -> IncomingMessage
       -> Maybe Concurrency.Handle
       -> PythonRequest
       -> Eff es PythonExit
     )
  -> Runtime context (Eff es)
  -> Runtime context (Eff es)
withPythonRunner runPython =
  withPythonProgram \maxCalls outerCallId runOne message resourceOwner pythonRequest -> do
    nestedState <- MVar.newMVar (NestedToolState maxCalls 1)
    runPython
      (runPythonTools outerCallId nestedState runOne)
      message
      resourceOwner
      pythonRequest

withPythonProgram
  :: Monad m
  => PythonProgramRunner m
  -> Runtime context m
  -> Runtime context m
withPythonProgram runPython runtime =
  runtime
    { aroundProgram = \finalRuntime@Runtime{aroundToolTurn = toolTurn} ->
        interpretPython
          (pythonProgram finalRuntime)
          (map toolName finalRuntime.exposedTools)
          toolTurn
          . runtime.aroundProgram finalRuntime
    }
  where
    pythonProgram finalRuntime request outerCall parsedRequest =
      lift $
        finalRuntime.aroundControlCall request.agentState.turn outerCall HList.HNil do
          case parsedRequest of
            Left err ->
              pure (argumentFailure [i|Invalid py arguments: #{err}|])
            Right pythonRequest ->
              pythonExitResult
                <$> runPython
                  finalRuntime.context.toolConfig.python.maxToolCalls
                  outerCall.id
                  (\call ->
                    finalRuntime.aroundToolCall request.agentState.turn call HList.HNil $
                      finalRuntime.dispatchToolCall finalRuntime.toolCallMetadata call)
                  finalRuntime.context.message
                  finalRuntime.toolCallMetadata.resourceOwner
                  pythonRequest

interpretPython
  :: Monad m
  => PythonInterpreter m
  -> [Text]
  -> (forall a. HList.HList '[] -> ToolRequest -> m (TurnState, a) -> m (TurnState, a))
  -> Program m result
  -> Program m result
interpretPython interpretCall exposedToolNames toolTurn =
  go
  where
    go (Program action) =
      Program do
        action >>= \case
          Finished result ->
            pure (Finished result)
          Continues next ->
            pure (Continues (go next))
          Visible (RunTools request) continue ->
            case exposedPythonCalls exposedToolNames request.toolCalls of
              [] ->
                pure (Visible (RunTools request) (go . continue))
              [call]
                | [_] <- toList request.toolCalls ->
                    handleCall request continue call
              _ ->
                rejectBatch request continue
          Visible event continue ->
            pure (Visible event (go . continue))

    handleCall request continue call =
      (do
        result <- case runPythonRequest call of
          Just parsedRequest ->
            interpretCall request call parsedRequest
          Nothing ->
            pure (argumentFailure "Unknown Python control operation.")
        resume request continue ((call, result) :| [])
      ).observe

    rejectBatch request continue =
      (resume request continue $ request.toolCalls <&> \call ->
        (call, argumentFailure "py must be called alone; no tool call in this turn was executed.")
      ).observe

    resume request continue executions = do
      (nextState, _) <- lift $ finishToolTurn toolTurn request (pure (snd <$> executions))
      go (continue nextState)

type PythonInterpreter m =
  ToolRequest
  -> LLM.ToolCall
  -> Either Text PythonRequest
  -> Program m ToolResult

type PythonProgramRunner m =
  Int
  -> Text
  -> (LLM.ToolCall -> m ToolResult)
  -> IncomingMessage
  -> Maybe Concurrency.Handle
  -> PythonRequest
  -> m PythonExit

exposedPythonCalls :: [Text] -> NonEmpty LLM.ToolCall -> [LLM.ToolCall]
exposedPythonCalls exposedToolNames =
  filter isExposedPython . toList
  where
    isExposedPython call =
      call.name == runPythonToolName
        && call.name `elem` exposedToolNames

argumentFailure :: Text -> ToolResult
argumentFailure message =
  toolFailure (permanentArgumentFailure message message)

maxNestedBatchCalls :: Int
maxNestedBatchCalls =
  16

maxNestedToolCallIdChars :: Int
maxNestedToolCallIdChars =
  256

data NestedToolState = NestedToolState
  { remainingCalls :: !Int
  , nextRpcId :: !Int
  }

runPythonTools
  :: Concurrent :> es
  => Text
  -> MVar.MVar NestedToolState
  -> (LLM.ToolCall -> Eff es ToolResult)
  -> Int
  -> NonEmpty PythonToolCall
  -> Eff es (NonEmpty ToolResult)
runPythonTools outerCallId nestedState runOne rpcId calls = do
  let validated = validateNestedBatch outerCallId rpcId calls
  claimNestedBatch nestedState rpcId (length calls) validated >>= \case
    Left failure ->
      pure (calls $> toolFailure failure)
    Right nestedCalls ->
      Async.mapConcurrently runOne nestedCalls

validateNestedBatch
  :: Text
  -> Int
  -> NonEmpty PythonToolCall
  -> Either Failure (NonEmpty LLM.ToolCall)
validateNestedBatch outerCallId rpcId calls
  | length calls > maxNestedBatchCalls =
      Left (invalidBatch [i|Python tools.run accepts at most #{maxNestedBatchCalls} calls per batch.|])
  | Just control <- find (isPythonProgramControl . (.name)) calls =
      let controlName = control.name
      in Left (invalidBatch [i|Python cannot call program-control tool: #{controlName}|])
  | otherwise =
      traverse toToolCall (NonEmpty.zip calls ((0 :: Int) :| [1 ..]))
  where
    toToolCall (call, index)
      | Text.length callId > maxNestedToolCallIdChars =
          Left (invalidBatch "Synthetic Python tool-call id exceeds the audit limit.")
      | otherwise =
          Right LLM.ToolCall
            { id = callId
            , name = call.name
            , arguments = call.arguments
            }
      where
        callId = [i|#{outerCallId}/python/#{rpcId}/#{index}|]

invalidBatch :: Text -> Failure
invalidBatch message =
  permanentArgumentFailure message message

claimNestedBatch
  :: Concurrent :> es
  => MVar.MVar NestedToolState
  -> Int
  -> Int
  -> Either Failure (NonEmpty LLM.ToolCall)
  -> Eff es (Either Failure (NonEmpty LLM.ToolCall))
claimNestedBatch stateVar rpcId requested validated =
  MVar.modifyMVarMasked stateVar \current@NestedToolState{remainingCalls, nextRpcId} ->
    if rpcId /= nextRpcId
      then pure
        ( current
        , Left (invalidBatch [i|Expected Python tools.run request id #{nextRpcId}, got #{rpcId}.|])
        )
      else
        let advanced = current{nextRpcId = nextRpcId + 1}
        in if requested <= remainingCalls
          then pure
            ( advanced{remainingCalls = remainingCalls - requested}
            , validated
            )
          else pure
            ( advanced
            , Left . budgetExhaustedFailure
                "Python nested tool-call budget exhausted."
                $ [i|Requested #{requested} calls with #{remainingCalls} remaining.|]
            )
