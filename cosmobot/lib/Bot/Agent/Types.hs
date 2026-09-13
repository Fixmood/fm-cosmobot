{-|
Module      : Bot.Agent.Types
Description : Agent context, events, and tool results
Stability   : experimental
-}
module Bot.Agent.Types
  ( ToolCallMetadata (..)
  , Context (..)
  , Event (..)
  , Observer
  , FailureCategory (..)
  , Failure (..)
  , failureFromException
  , failureStatus
  , permanentArgumentFailure
  , permissionDeniedFailure
  , ToolConfig (..)
  , PythonConfig (..)
  , defaultPythonConfig
  , maxPythonWallTimeoutSeconds
  , WebSearchApi (..)
  , defaultToolConfig
  , ignoreObserver
  , ToolResult (..)
  , toolText
  , toolTextWithImages
  , toolFailure
  , toolResultContent
  , toolResultImageUrls
  , toolResultFailure
  )
where

import Bot.Agent.Failure
import qualified Data.Text as Text
import Bot.Core.Message
import Bot.Core.Thread (ThreadMessageKey)
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude

-- | Runtime configuration for agent tools.
data ToolConfig = ToolConfig
  { webSearchEnable :: !Bool
  , webSearchApi :: !WebSearchApi
  , webSearchMaxResults :: !(Maybe Int)
  , braveApiKey :: !(Maybe Text)
  , tavilyApiKey :: !(Maybe Text)
  , exaApiKey :: !(Maybe Text)
  , webFetch :: !Bool
  , webFetchMaxUses :: !(Maybe Int)
  , webFetchMaxContentTokens :: !(Maybe Int)
  , datetime :: !Bool
  , python :: !PythonConfig
  , sandboxImage :: !Text
  }
  deriving (Show)

-- | Host-controlled limits for the Python composition tool.
data PythonConfig = PythonConfig
  { enabled :: !Bool
  , wallTimeoutSeconds :: !Int
  , cpuSeconds :: !Int
  , memoryMiB :: !Int
  , maxToolCalls :: !Int
  }
  deriving (Eq, Show)

data WebSearchApi
  = WebSearchTavily
  | WebSearchBrave
  | WebSearchExa
  deriving (Eq, Show)

defaultToolConfig :: ToolConfig
defaultToolConfig = ToolConfig
  { webSearchEnable = False
  , webSearchApi = WebSearchTavily
  , webSearchMaxResults = Nothing
  , braveApiKey = Nothing
  , tavilyApiKey = Nothing
  , exaApiKey = Nothing
  , webFetch = False
  , webFetchMaxUses = Nothing
  , webFetchMaxContentTokens = Nothing
  , datetime = False
  , python = defaultPythonConfig
  , sandboxImage = "localhost/cosmobox:latest"
  }

defaultPythonConfig :: PythonConfig
defaultPythonConfig = PythonConfig
  { enabled = True
  , wallTimeoutSeconds = 30
  , cpuSeconds = 20
  , memoryMiB = 512
  , maxToolCalls = 64
  }

maxPythonWallTimeoutSeconds :: Int
maxPythonWallTimeoutSeconds = 60 * 60

data ToolCallMetadata = ToolCallMetadata
  { agentRunId :: !Text
  , originRunId :: !Text
  , resourceOwner :: !(Maybe Concurrency.Handle)
  }

-- | Per-message capabilities and permissions made available to tools.
data Context = Context
  { message :: IncomingMessage
  , input :: !MessageInput
  , superuser :: !Bool
  , systemContext :: !Text
  , askCommand :: !Text
  , toolConfig :: !ToolConfig
  }

-- | Semantic lifecycle events emitted by the agent engine.
--
-- Observers translate these into concrete side effects such as persistent
-- audit rows. The loop itself should only emit these domain events.
data Event
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
      , toolGroups :: ![(Text, Int)]
      }
  | ModelTurnFinished
      { runId :: !Text
      , turn :: !Int
      , answerKind :: !Text
      , contentLength :: !Int
      , toolCalls :: ![LLM.ToolCall]
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
      , toolCall :: !LLM.ToolCall
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
      , linkedMessageKey :: !ThreadMessageKey
      , parentMessageId :: !(Maybe MessageId)
      }
  deriving (Eq, Show)

type Observer ctx m =
  Event -> m ctx

ignoreObserver :: Applicative m => ctx -> Observer ctx m
ignoreObserver ctx =
  const (pure ctx)

-- | One tool call outcome. Failures are still returned as tool results because
-- OpenAI-compatible history requires every requested tool call to have a
-- corresponding tool-result message.
data ToolResult
  = ToolSucceeded
      { content :: !Text
      , imageUrls :: ![Text]
      }
  | ToolFailed
      { failure :: !Failure
      }

toolText :: Text -> ToolResult
toolText content =
  ToolSucceeded content []

toolTextWithImages :: Text -> [Text] -> ToolResult
toolTextWithImages content imageUrls =
  ToolSucceeded content imageUrls

toolFailure :: Failure -> ToolResult
toolFailure failure =
  ToolFailed failure

toolResultContent :: ToolResult -> Text
toolResultContent = \case
  ToolSucceeded{content} ->
    content
  ToolFailed{failure} ->
    failureContent failure

-- | Text shown to the model for a failed tool call.
--
-- The model only ever sees this string, so dropping 'detail' entirely hides the
-- actual cause (usually the raw exception) and leaves the model a summary it
-- cannot act on -- or worse, one it glosses over as success.
--
-- Many failures are built as @makeFailure category message message@, so the
-- detail is frequently identical to (or a prefix of) the summary. In that case
-- appending it would only repeat text the model already has, so we stay quiet.
failureContent :: Failure -> Text
failureContent failure =
  case Text.strip failure.detail of
    "" ->
      failure.userMessage
    detail
      | detail == summary -> failure.userMessage
      | summary `Text.isPrefixOf` detail -> failure.userMessage
      | otherwise ->
          failure.userMessage
            <> "\nRaw detail: "
            <> previewFailureDetail detail
            <> "\nReport the failure honestly; do not claim the action succeeded."
      where
        summary = Text.strip failure.userMessage

-- | Cap on how much raw failure detail is handed to the model.
failureDetailPreviewChars :: Int
failureDetailPreviewChars = 800

-- | Collapse whitespace and cap the length so a raw exception cannot flood the
-- transcript. Local to this module on purpose: the equivalent helpers elsewhere
-- live in LLM modules that this one must not depend on.
previewFailureDetail :: Text -> Text
previewFailureDetail text =
  let oneLine = Text.unwords (Text.words text)
  in if Text.length oneLine > failureDetailPreviewChars
       then Text.take failureDetailPreviewChars oneLine <> "..."
       else oneLine

toolResultImageUrls :: ToolResult -> [Text]
toolResultImageUrls = \case
  ToolSucceeded{imageUrls} ->
    imageUrls
  ToolFailed{} ->
    []

toolResultFailure :: ToolResult -> Maybe Failure
toolResultFailure = \case
  ToolSucceeded{} ->
    Nothing
  ToolFailed{failure} ->
    Just failure
