{-|
Module      : Bot.AgentAudit.Types
Description : Agent audit domain types
Stability   : experimental
-}

module Bot.AgentAudit.Types
  ( AgentAuditEvent (..)
  , AgentAuditRecord (..)
  , ToolCallTrace (..)
  , ToolUseDetail (..)
  , ToolUseStatus (..)
  , eventRunId
  , toolUsesFromAuditRecords
  , toolUsesFromRecords
  )
where

import Bot.Core.Message
import Bot.Core.Thread (ThreadMessageKey)
import qualified Bot.LLM.Types as LLM
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime, diffUTCTime)

data ToolCallTrace = ToolCallTrace
  { id :: !Text
  , name :: !Text
  , arguments :: !Text
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data AgentAuditEvent
  = AgentRunStarted
      { runId :: !Text
      , messageId :: !(Maybe MessageId)
      , maxTurns :: !Int
      , exposedTools :: ![Text]
      }
  | ModelTurnStarted
      { runId :: !Text
      , turn :: !Int
      , messageCount :: !Int
      , exposedTools :: ![Text]
      , toolGroups :: !(Maybe [(Text, Int)])
      }
  | ModelTurnFinished
      { runId :: !Text
      , turn :: !Int
      , answerKind :: !Text
      , contentLength :: !Int
      , toolCalls :: ![ToolCallTrace]
      , tokenUsage :: !(Maybe LLM.TokenUsage)
      }
  | ContextCompacted
      { runId :: !Text
      , turn :: !Int
      , messageCount :: !Int
      , tokenUsage :: !(Maybe LLM.TokenUsage)
      }
  | SubAgentRunStarted
      { runId :: !Text
      , childRunId :: !Text
      , subagentId :: !Text
      }
  | ToolCallStarted
      { runId :: !Text
      , turn :: !Int
      , toolCall :: !ToolCallTrace
      }
  | ToolCallFinished
      { runId :: !Text
      , turn :: !Int
      , toolCallId :: !Text
      , toolName :: !Text
      , status :: !Text
      , result :: !Text
      , resultLength :: !Int
      , messageIds :: ![Maybe MessageId]
      }
  | AgentRunFinished
      { runId :: !Text
      , status :: !Text
      , finalLength :: !Int
      , turnsUsed :: !Int
      }
  | AgentRunInterrupted
      { runId :: !Text
      , reason :: !Text
      }
  | AgentThreadLinked
      { runId :: !Text
      , linkedMessageId :: !MessageId
      , linkedMessageKey :: !(Maybe ThreadMessageKey)
      , parentMessageId :: !(Maybe MessageId)
      }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data AgentAuditRecord = AgentAuditRecord
  { id :: !Integer
  , occurredAt :: !UTCTime
  , event :: !AgentAuditEvent
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data ToolUseStatus
  = ToolUseInProgress
  | ToolUseFinished
      { status :: !Text
      , durationMilliseconds :: !Integer
      }
  | ToolUseInterrupted
      { reason :: !Text
      , durationMilliseconds :: !Integer
      }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data ToolUseDetail = ToolUseDetail
  { auditId :: !Integer
  , occurredAt :: !UTCTime
  , finishedAt :: !(Maybe UTCTime)
  , runId :: !Text
  , turn :: !Int
  , toolName :: !Text
  , toolCallId :: !Text
  , arguments :: !Text
  , status :: !ToolUseStatus
  , result :: !(Maybe Text)
  , messageIds :: ![Maybe MessageId]
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

eventRunId :: AgentAuditEvent -> Text
eventRunId = \case
  AgentRunStarted{runId} -> runId
  ModelTurnStarted{runId} -> runId
  ModelTurnFinished{runId} -> runId
  ContextCompacted{runId} -> runId
  SubAgentRunStarted{runId} -> runId
  ToolCallStarted{runId} -> runId
  ToolCallFinished{runId} -> runId
  AgentRunFinished{runId} -> runId
  AgentRunInterrupted{runId} -> runId
  AgentThreadLinked{runId} -> runId

toolUsesFromRecords :: UTCTime -> Int -> [AgentAuditRecord] -> [ToolUseDetail]
toolUsesFromRecords processStartedAt limit records =
  takeLast (max 0 limit) (map markStale (toolUsesFromAuditRecords records))
  where
    markStale toolUse
      | toolUse.status == ToolUseInProgress
      , toolUse.occurredAt < processStartedAt =
          toolUse
            { finishedAt = Just processStartedAt
            , status = ToolUseInterrupted
                { reason = "restarted"
                , durationMilliseconds = floor (diffUTCTime processStartedAt toolUse.occurredAt * 1000)
                }
            }
      | otherwise =
          toolUse

toolUsesFromAuditRecords :: [AgentAuditRecord] -> [ToolUseDetail]
toolUsesFromAuditRecords records =
  mapMaybe (toolUseDetail finishes interruptions) (filter isToolStart records)
  where
    finishes =
      Map.fromList
        [ ((event.runId, event.toolCallId), record)
        | record@AgentAuditRecord{event = event@ToolCallFinished{}} <- records
        ]
    interruptions =
      Map.fromList
        [ (event.runId, record)
        | record@AgentAuditRecord{event = event@AgentRunInterrupted{}} <- records
        ]

    toolUseDetail finishesByCall interruptionsByRun AgentAuditRecord{id = auditId, occurredAt, event = ToolCallStarted{runId, turn, toolCall}} =
      let finished = Map.lookup (runId, toolCall.id) finishesByCall
          interrupted = Map.lookup runId interruptionsByRun
      in Just ToolUseDetail
        { auditId
        , occurredAt
        , finishedAt = (.occurredAt) <$> (finished <|> interrupted)
        , runId
        , turn
        , toolName = toolCall.name
        , toolCallId = toolCall.id
        , arguments = toolCall.arguments
        , status = toolUseStatus occurredAt finished interrupted
        , result = finished >>= finishedResult
        , messageIds = maybe [] finishedMessageIds finished
        }
    toolUseDetail _ _ _ =
      Nothing

    toolUseStatus startedAt (Just finished) _ =
      finishedStatus startedAt finished
    toolUseStatus startedAt Nothing (Just interrupted) =
      interruptedStatus startedAt interrupted
    toolUseStatus _ Nothing Nothing =
      ToolUseInProgress

    finishedStatus startedAt AgentAuditRecord{occurredAt = finishedAt, event = ToolCallFinished{status}} =
      ToolUseFinished
        { status
        , durationMilliseconds = floor (diffUTCTime finishedAt startedAt * 1000)
        }
    finishedStatus _ _ =
      ToolUseInProgress

    interruptedStatus startedAt AgentAuditRecord{occurredAt = interruptedAt, event = AgentRunInterrupted{reason}} =
      ToolUseInterrupted
        { reason
        , durationMilliseconds = floor (diffUTCTime interruptedAt startedAt * 1000)
        }
    interruptedStatus _ _ =
      ToolUseInProgress

    finishedResult AgentAuditRecord{event = ToolCallFinished{result}} = Just result
    finishedResult _ = Nothing

    finishedMessageIds AgentAuditRecord{event = ToolCallFinished{messageIds}} = messageIds
    finishedMessageIds _ = []

    isToolStart AgentAuditRecord{event = ToolCallStarted{}} = True
    isToolStart _ = False

takeLast :: Int -> [a] -> [a]
takeLast n values =
  drop (max 0 (length values - n)) values
