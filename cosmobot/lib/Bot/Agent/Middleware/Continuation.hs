{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-|
Module      : Bot.Agent.Middleware.Continuation
Description : One-shot delimited agent continuations
Stability   : experimental
-}
module Bot.Agent.Middleware.Continuation
  ( withContinuations
  )
where

import Bot.Agent.Core
import Bot.Agent.Control (finishToolTurn, runControlTurn)
import Bot.Agent.Tools.Common (jsonText)
import Bot.Agent.Tools.Continuation
import Bot.Agent.Transcript (appendMessage)
import Bot.Agent.Tool (toolName)
import Bot.Agent.Types
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Bot.Util.HList as HList
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map

data Resumption es = Resumption
  { ordinal :: !Int
  , resume :: TurnState -> Aeson.Value -> Program (Eff es) Result
  }

withContinuations
  :: Runtime context (Eff es)
  -> Runtime context (Eff es)
withContinuations runtime =
  runtime
    { aroundProgram = \finalRuntime ->
        handleContinuations finalRuntime
          . runtime.aroundProgram finalRuntime
    }

handleContinuations
  :: Runtime '[] (Eff es)
  -> Program (Eff es) Result
  -> Program (Eff es) Result
handleContinuations runtime@Runtime{aroundToolTurn = toolTurn} =
  go 0 Map.empty
  where
    go nextOrdinal saved (Program action) =
      Program do
        action >>= \case
          Finished result ->
            pure (Finished result)
          Continues next ->
            pure (Continues (go 0 Map.empty next))
          Visible (RunTools request) continue ->
            case exposedContinuationCalls runtime request.toolCalls of
              [] ->
                pure (Visible (RunTools request) (go nextOrdinal saved . continue))
              [call]
                | [_] <- toList request.toolCalls ->
                    handleCall nextOrdinal saved request continue call
              _ ->
                handleConcurrent nextOrdinal saved request continue
          Visible event continue ->
            pure (Visible event (go nextOrdinal saved . continue))

    handleCall nextOrdinal saved request continue call =
      case continuationRequest call of
        Nothing ->
          reject nextOrdinal saved request continue call "Unknown continuation operation."
        Just (Left err) ->
          reject nextOrdinal saved request continue call [i|Invalid continuation arguments: #{err}|]
        Just (Right (CaptureContinuation label)) ->
          case Map.lookup call.id saved of
            Just _ ->
              reject nextOrdinal saved request continue call "Continuation id already exists."
            Nothing -> do
              (nextState, result) <-
                lift $ runControlTurn runtime request call (pure (captureResult call label))
              let resumption =
                    Resumption
                      { ordinal = nextOrdinal
                      , resume = \currentState value ->
                          continue (restoreAtCapture request call currentState value)
                      }
                  (continuedOrdinal, continuedSaved)
                    | isNothing (toolResultFailure result) =
                        (nextOrdinal + 1, Map.insert call.id resumption saved)
                    | otherwise =
                        (nextOrdinal, saved)
              (go continuedOrdinal continuedSaved (continue nextState)).observe
        Just (Right (ResumeContinuation continuationId value)) ->
          case Map.lookup continuationId saved of
            Nothing ->
              reject nextOrdinal saved request continue call [i|Continuation not found in this agent run: #{continuationId}|]
            Just resumption -> do
              (nextState, result) <-
                lift $ runControlTurn runtime request call (pure (toolText (jsonText (continuationValue continuationId value))))
              let succeeded = isNothing (toolResultFailure result)
                  continuedSaved
                    | succeeded = Map.filter ((< resumption.ordinal) . (.ordinal)) saved
                    | otherwise = saved
                  next
                    | succeeded = resumption.resume nextState value
                    | otherwise = continue nextState
              (go nextOrdinal continuedSaved next).observe

    reject nextOrdinal saved request continue call message = do
      (nextState, _) <-
        lift $ runControlTurn runtime request call (pure (argumentFailure message))
      (go nextOrdinal saved (continue nextState)).observe

    handleConcurrent nextOrdinal saved request continue = do
      (nextState, _) <- lift $ finishToolTurn toolTurn request $
        forM request.toolCalls \call -> do
          runtime.aroundToolCall request.agentState.turn call HList.HNil $
            pure (argumentFailure "Continuation control tools must be called alone; no tool call in this turn was executed.")
      (go nextOrdinal saved (continue nextState)).observe

exposedContinuationCalls :: Runtime context (Eff es) -> NonEmpty LLM.ToolCall -> [LLM.ToolCall]
exposedContinuationCalls runtime =
  filter isExposedContinuation . toList
  where
    isExposedContinuation call =
      isContinuationToolName call.name
        && any ((== call.name) . toolName) runtime.exposedTools

captureResult :: LLM.ToolCall -> Maybe Text -> ToolResult
captureResult call label =
  toolText . jsonText $
    Aeson.object
      [ "state" Aeson..= ("captured" :: Text)
      , "continuation_id" Aeson..= call.id
      , "label" Aeson..= label
      , "scope" Aeson..= ("current_agent_run" :: Text)
      , "one_shot" Aeson..= True
      ]

restoreAtCapture
  :: ToolRequest
  -> LLM.ToolCall
  -> TurnState
  -> Aeson.Value
  -> TurnState
restoreAtCapture request call currentState value =
  currentState
    { transcript = resumedTranscript
    , nextModelTranscript = Just resumedTranscript
    }
  where
    resumedTranscript =
      appendMessage (LLM.toolResult call (jsonText (continuationValue call.id value))) request.answered

continuationValue :: Text -> Aeson.Value -> Aeson.Value
continuationValue continuationId value =
  Aeson.object
    [ "state" Aeson..= ("resumed" :: Text)
    , "continuation_id" Aeson..= continuationId
    , "value" Aeson..= value
    ]

argumentFailure :: Text -> ToolResult
argumentFailure message =
  toolFailure (permanentArgumentFailure message message)
