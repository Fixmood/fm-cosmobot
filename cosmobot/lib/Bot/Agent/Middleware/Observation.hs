{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module      : Bot.Agent.Middleware.Observation
Description : Agent lifecycle observation wrappers
Stability   : experimental
-}

module Bot.Agent.Middleware.Observation
  ( ObservedThreadLink (..)
  , ObservedModelTurn (..)
  , ObservedToolCall (..)
  , ObservationContext (..)
  , emptyObservationContext
  , observeThreadLinked
  , withObservation
  , withObservedModelTurn
  , withObservedAgentRun
  , withObservedToolCall
  )
where

import Bot.Agent.Core
import Bot.Agent.Middleware.Observation.Types
import Bot.Agent.Tool (toolName)
import qualified Bot.Agent.ToolRegistry as ToolRegistry
import Bot.Agent.Types
import Bot.Core.Transcript
import Bot.Core.Thread (ThreadMessageKey (..))
import Bot.Core.Message (IncomingMessage (..), MessageId)
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Bot.Util.HList as HList
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Streaming
import qualified Streaming.Prelude as S

data ObservedModelTurn result = ObservedModelTurn
  { runId :: !Text
  , turn :: !Int
  , messageCount :: !Int
  , exposedTools :: ![Text]
  , toolGroups :: ![(Text, Int)]
  , finished :: result -> Event
  }

data ObservedToolCall = ObservedToolCall
  { runId :: !Text
  , turn :: !Int
  , toolCall :: !LLM.ToolCall
  }

data ObservedThreadLink = ObservedThreadLink
  { runId :: !Text
  , parentMessageId :: !(Maybe MessageId)
  , linkedMessageKey :: !ThreadMessageKey
  }

withObservation
  :: forall context es.
     HList.Has (ToolResultObservation es) context
  => Observer ObservationContext (Eff es)
  -> Runtime (ObservationContext ': EventObservation es ': context) (Eff es)
  -> Runtime context (Eff es)
withObservation observer program@Runtime{aroundToolTurn = toolTurn} =
  program
    { aroundAgentRun = \context action ->
        withObservedAgentRun observer program (map toolName program.exposedTools) do
          program.aroundAgentRun (observedContext emptyObservationContext context) action
    , modelInputTranscript = \context agentState ->
        program.modelInputTranscript (observedContext emptyObservationContext context) agentState
    , aroundModelTurn = \context agentState action ->
        let turnInfo = ObservedModelTurn
              { runId = program.runId
              , turn = agentState.turn
              , messageCount = transcriptMessageCount agentState
              , exposedTools = map toolName program.exposedTools
              , toolGroups = ToolRegistry.enabledToolGroups agentState.transcript program.runningTools
              , finished = modelTurnFinished program.runId
              }
        in withObservedModelTurn observer turnInfo (program.aroundModelTurn (observedContext emptyObservationContext context) agentState action)
    , aroundToolTurn = \context toolState action ->
        toolTurn (observedContext emptyObservationContext context) toolState action
    , aroundControlCall = \turn toolCall context action ->
        let observedCall = ObservedToolCall
              { runId = program.runId
              , turn = turn
              , toolCall = toolCall
              }
            toolResultObservation =
              HList.get @(ToolResultObservation es) context
        in withObservedToolCall toolResultObservation.observeToolResult observer observedCall \observation ->
             program.aroundControlCall turn toolCall (observedContext observation context) action
    , aroundToolCall = \turn toolCall context action ->
        let observedCall = ObservedToolCall
              { runId = program.runId
              , turn = turn
              , toolCall = toolCall
              }
            toolResultObservation =
              HList.get @(ToolResultObservation es) context
        in withObservedToolCall toolResultObservation.observeToolResult observer observedCall \observation ->
             program.aroundToolCall turn toolCall (observedContext observation context) action
    }
  where
    observedContext observation context =
      observation HList.:& EventObservation observer HList.:& context

    modelTurnFinished runId (agentState, answer) =
      case answer of
        LLM.ChatFinalAnswer{content} ->
          ModelTurnFinished
            { runId = runId
            , turn = agentState.turn
            , answerKind = "final"
            , contentLength = Text.length content
            , toolCalls = []
            , tokenUsage = LLM.chatAnswerTokenUsage answer
            }
        LLM.ChatToolRequest{content, toolCalls} ->
          ModelTurnFinished
            { runId = runId
            , turn = agentState.turn
            , answerKind = "tool_request"
            , contentLength = Text.length content
            , toolCalls = toList toolCalls
            , tokenUsage = LLM.chatAnswerTokenUsage answer
            }

transcriptMessageCount :: TurnState -> Int
transcriptMessageCount TurnState{transcript = Transcript{messages}} =
  Foldable.length messages

withObservedAgentRun
  :: Observer ObservationContext (Eff es)
  -> Runtime context (Eff es)
  -> [Text]
  -> Stream (Of Output) (Eff es) Result
  -> Stream (Of Output) (Eff es) Result
withObservedAgentRun observer runtime exposedTools action =
  catchStream
    ( do
        lift $ void $ observer AgentRunStarted
          { runId = runtime.runId
          , messageId = runtime.context.message.messageId
          , maxTurns = runtime.maxTurns
          , exposedTools
          }
        result <- action
        let Result{status, finalText, turnsUsed} = result
        lift $ void $ observer AgentRunFinished
          { runId = runtime.runId
          , status
          , finalLength = Text.length finalText
          , turnsUsed
          }
        pure result
    )
    \err -> do
      lift $ void $ observer AgentRunInterrupted{runId = runtime.runId, reason = interruptedReason err}
      lift $ throwIO err

withObservedModelTurn
  :: Observer ObservationContext (Eff es)
  -> ObservedModelTurn result
  -> Stream (Of Output) (Eff es) result
  -> Stream (Of Output) (Eff es) result
withObservedModelTurn observer turnInfo action = do
  lift $ void $ observer ModelTurnStarted
    { runId = turnInfo.runId
    , turn = turnInfo.turn
    , messageCount = turnInfo.messageCount
    , exposedTools = turnInfo.exposedTools
    , toolGroups = turnInfo.toolGroups
    }
  result <- action
  lift $ void $ observer (turnInfo.finished result)
  pure result

withObservedToolCall
  :: (ToolResult -> Eff es Text)
  -> Observer ObservationContext (Eff es)
  -> ObservedToolCall
  -> (ObservationContext -> Eff es ToolResult)
  -> Eff es ToolResult
withObservedToolCall toolResultForObservation observer callInfo action = do
  observation <- observer ToolCallStarted
    { runId = callInfo.runId
    , turn = callInfo.turn
    , toolCall = callInfo.toolCall
    }
  (status, result) <-
    statusFromResult <$> action observation
  finishToolCall toolResultForObservation observer callInfo status result
  pure result

statusFromResult :: ToolResult -> (Text, ToolResult)
statusFromResult result
  | Just failure <- toolResultFailure result =
      (failureStatus failure, result)
  | otherwise =
      ("ok", result)

observeThreadLinked :: Observer ObservationContext (Eff es) -> ObservedThreadLink -> Eff es ()
observeThreadLinked observer ObservedThreadLink{runId, parentMessageId, linkedMessageKey} =
  void $ observer AgentThreadLinked
    { runId
    , linkedMessageId = linkedMessageKey.messageId
    , linkedMessageKey
    , parentMessageId
    }

finishToolCall :: (ToolResult -> Eff es Text) -> Observer ObservationContext (Eff es) -> ObservedToolCall -> Text -> ToolResult -> Eff es ()
finishToolCall toolResultForObservation observer callInfo status result = do
  observedResult <- toolResultForObservation result
  void $ observer ToolCallFinished
    { runId = callInfo.runId
    , turn = callInfo.turn
    , toolCallId = callInfo.toolCall.id
    , toolName = callInfo.toolCall.name
    , status = status
    , result = observedResult
    , resultLength = Text.length (toolResultContent result)
    , messageIds = []
    }

catchStream
  :: Stream (Of a) (Eff es) r
  -> (SomeException -> Stream (Of a) (Eff es) r)
  -> Stream (Of a) (Eff es) r
catchStream stream handler = do
  inspected <- lift (try (Streaming.inspect stream))
  case inspected of
    Left err ->
      handler err
    Right (Left result) ->
      pure result
    Right (Right (value S.:> rest)) -> do
      S.yield value
      catchStream rest handler

interruptedReason :: SomeException -> Text
interruptedReason _ =
  "failed"
