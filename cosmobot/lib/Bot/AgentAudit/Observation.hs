{-|
Module      : Bot.AgentAudit.Observation
Description : Agent observation adapter for audit events
Stability   : experimental
-}

module Bot.AgentAudit.Observation
  ( agentAuditObserverWith
  )
where

import qualified Bot.Agent.Middleware.Observation.Types as Observation
import qualified Bot.Agent.Types as Agent
import Bot.AgentAudit.Types
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude

agentAuditObserverWith :: (AgentAuditEvent -> Eff es (Maybe Integer)) -> Agent.Observer Observation.ObservationContext (Eff es)
agentAuditObserverWith recordEvent =
  recordAgentEvent recordEvent

recordAgentEvent :: (AgentAuditEvent -> Eff es (Maybe Integer)) -> Agent.Event -> Eff es Observation.ObservationContext
recordAgentEvent recordEvent event =
  observationContext <$> maybe (pure Nothing) recordEvent (agentAuditEvent event)

agentAuditEvent :: Agent.Event -> Maybe AgentAuditEvent
agentAuditEvent = \case
  Agent.AgentRunStarted{runId, messageId, maxTurns, exposedTools} ->
    Just AgentRunStarted{runId, messageId, maxTurns, exposedTools}
  Agent.ModelTurnStarted{runId, turn, messageCount, exposedTools, toolGroups} ->
    Just ModelTurnStarted{runId, turn, messageCount, exposedTools, toolGroups = Just toolGroups}
  Agent.ModelTurnFinished{runId, turn, answerKind, contentLength, toolCalls, tokenUsage} ->
    Just ModelTurnFinished
      { runId
      , turn
      , answerKind
      , contentLength
      , toolCalls = map toolCallTrace toolCalls
      , tokenUsage
      }
  Agent.ContextCompacted{runId, turn, messageCount, tokenUsage} ->
    Just ContextCompacted{runId, turn, messageCount, tokenUsage}
  Agent.SubAgentRunStarted{runId, childRunId, subagentId} ->
    Just SubAgentRunStarted{runId, childRunId, subagentId}
  Agent.ToolCallStarted{runId, turn, toolCall} ->
    Just ToolCallStarted
      { runId
      , turn
      , toolCall = toolCallTrace toolCall
      }
  Agent.ToolCallFinished{runId, turn, toolCallId, toolName, status, result, resultLength, messageIds} ->
    Just ToolCallFinished{runId, turn, toolCallId, toolName, status, result, resultLength, messageIds}
  Agent.AgentRunFinished{runId, status, finalLength, turnsUsed} ->
    Just AgentRunFinished{runId, status, finalLength, turnsUsed}
  Agent.AgentRunInterrupted{runId, reason} ->
    Just AgentRunInterrupted{runId, reason}
  Agent.AgentThreadLinked{runId, linkedMessageId, linkedMessageKey, parentMessageId} ->
    Just AgentThreadLinked{runId, linkedMessageId, linkedMessageKey = Just linkedMessageKey, parentMessageId}

toolCallTrace :: LLM.ToolCall -> ToolCallTrace
toolCallTrace call =
  ToolCallTrace
    { id = call.id
    , name = call.name
    , arguments = call.arguments
    }

observationContext :: Maybe Integer -> Observation.ObservationContext
observationContext auditId =
  Observation.ObservationContext{Observation.auditToolUseId = auditId}
