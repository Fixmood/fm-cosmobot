{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-|
Module      : Bot.Agent.Core
Description : Agent program calculus and runtime types
Stability   : experimental
-}

module Bot.Agent.Core
  ( AgentEvent (..)
  , Result (..)
  , Output (..)
  , TurnState (..)
  , Program (..)
  , Runtime (..)
  , Step (..)
  , ToolRequest (..)
  , runModel
  , runTools
  , trigger
  )
where

import Bot.Agent.Tool (Tool)
import Bot.Agent.ToolRegistry (RunningTool)
import Bot.Agent.Types
import Bot.Core.Transcript
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Bot.Util.HList as HList

data Result = Result
  { runId :: !Text
  , transcript :: !Transcript
  , status :: !Text
  , finalText :: !Text
  , turnsUsed :: !Int
  , tokenUsage :: !(Maybe LLM.TokenUsage)
  }

data Output
  = ContentDelta !Text
  | ToolCallNotification !(NonEmpty LLM.ToolCall)
  | ReplyBoundary

-- | Current state of the agent program.
data TurnState = TurnState
  { transcript    :: !Transcript
  , nextModelTranscript :: !(Maybe Transcript)
  , turn         :: !Int
  , modelTokenUsage :: !(Maybe LLM.TokenUsage)
  }

newtype Program m result = Program
  { observe :: Stream (Of Output) m (Step m result)
  }

data Step m result where
  Finished :: !result -> Step m result
  Continues :: !(Program m result) -> Step m result
  Visible
    :: !(AgentEvent response)
    -> !(response -> Program m result)
    -> Step m result

-- | Visible operations interpreted by the agent runtime.
data AgentEvent response where
  -- The response carries the effective state because model middleware may
  -- rewrite it before issuing the request.
  RunModel :: !TurnState -> AgentEvent (TurnState, LLM.ChatAnswer)
  RunTools :: !ToolRequest -> AgentEvent TurnState

trigger :: Monad m => AgentEvent response -> Program m response
trigger event =
  Program (pure (Visible event pure))

runModel :: Monad m => TurnState -> Program m (TurnState, LLM.ChatAnswer)
runModel =
  trigger . RunModel

runTools :: Monad m => ToolRequest -> Program m TurnState
runTools =
  trigger . RunTools

instance Monad m => Functor (Program m) where
  fmap f (Program action) =
    Program (fmap (mapStep f) action)

instance Monad m => Applicative (Program m) where
  pure result =
    Program (pure (Finished result))

  function <*> argument =
    function >>= \f -> fmap f argument

instance Monad m => Monad (Program m) where
  Program action >>= next =
    Program do
      action >>= \case
        Finished result ->
          let Program nextAction = next result
          in nextAction
        Continues program ->
          pure (Continues (program >>= next))
        Visible event continue ->
          pure (Visible event ((>>= next) . continue))

instance MonadTrans Program where
  lift action =
    Program (Finished <$> lift action)

mapStep
  :: Monad m
  => (a -> b)
  -> Step m a
  -> Step m b
mapStep f = \case
  Finished result ->
    Finished (f result)
  Continues program ->
    Continues (fmap f program)
  Visible event continue ->
    Visible event (fmap f . continue)

-- | Runtime wiring for the agent algorithm.
--
-- The core program stays as direct event interpretation, while cross-cutting
-- behavior gets named middleware boundaries. For example, transcript
-- compaction belongs in 'aroundModelTurn': it can rewrite state before the
-- next LLM request without changing tool execution or completion handling.
data Runtime (context :: [Type]) m = Runtime
  { runId :: !Text
  , toolCallMetadata :: !ToolCallMetadata
  , context :: Context
    -- | All configured tools, including tools unavailable to this request.
  , tools :: [Tool m]
    -- | Tools whose schemas are visible to the model.
  , exposedTools :: [Tool m]
    -- | Per-run tool implementations.
  , runningTools :: [RunningTool m]
    -- | Dispatch one call through the frozen per-run registry.
  , dispatchToolCall :: ToolCallMetadata -> LLM.ToolCall -> m ToolResult
    -- | Maximum number of model-requested tool turns.
  , maxTurns :: !Int
    -- | Select the transcript sent to the next model request. Most programs
    -- use the canonical transcript; middleware may expose a one-shot view.
  , modelInputTranscript :: HList.HList context -> TurnState -> m Transcript
    -- | Wrap the whole coinductive program.
    --
    -- The final runtime is supplied so middleware uses every installed
    -- bracket, including wrappers added later in the composition chain.
  , aroundProgram :: Runtime '[] m -> Program m Result -> Program m Result
    -- | Wrap one complete agent run.
  , aroundAgentRun :: HList.HList context -> Stream (Of Output) m Result -> Stream (Of Output) m Result
    -- | Wrap one complete model phase.
    --
    -- Use this for model-side middleware such as transcript compaction,
    -- timing, auditing, or exception-aware behavior around one streamed model
    -- request and response.
  , aroundModelTurn
      :: HList.HList context
      -> TurnState
      -> (TurnState -> Stream (Of Output) m (TurnState, LLM.ChatAnswer))
      -> Stream (Of Output) m (TurnState, LLM.ChatAnswer)
    -- | Wrap the whole tool phase.
    --
    -- Use this for cleanup, timing, timeout, auditing, or exception-aware
    -- behavior that must cover all tool calls in the phase.
  , aroundToolTurn
      :: forall a.
         HList.HList context
      -> ToolRequest
      -> m (TurnState, a)
      -> m (TurnState, a)
    -- | Wrap one model-issued control call for its complete carrier lifetime.
    --
    -- Control forms such as py are not registry tools, but retain the
    -- same failure, observation, progress, and result-observation lifecycle.
  , aroundControlCall :: Int -> LLM.ToolCall -> HList.HList context -> m ToolResult -> m ToolResult
    -- | Wrap one model-requested tool call.
    --
    -- Use this for per-call observation, failure recovery, policy, or timing
    -- without replacing the default tool registry dispatch.
  , aroundToolCall :: Int -> LLM.ToolCall -> HList.HList context -> m ToolResult -> m ToolResult
  }

data ToolRequest = ToolRequest
  { agentState :: !TurnState
  , answered   :: !Transcript
  , toolContent :: !Text
  , toolCalls  :: !(NonEmpty LLM.ToolCall)
  }
