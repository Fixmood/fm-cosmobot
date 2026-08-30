{-# LANGUAGE DataKinds #-}
{-|
Module      : Bot.Agent
Description : Agent program and extensible tool framework
Stability   : experimental
-}

module Bot.Agent
  ( ToolCallMetadata (..)
  , Context (..)
  , Event (..)
  , Observer
  , Program
  , Runtime
  , Result (..)
  , Output (..)
  , ToolEmittedMessageSink (..)
  , SteeringControl (..)
  , FailureCategory (..)
  , Failure (..)
  , ToolConfig (..)
  , WebSearchApi (..)
  , defaultToolConfig
  , withRun
  , runAgent
  , withAgentMetadata
  , startRuntime
  , runIdOf
  , defaultRuntime
  , agentStream
  , ToolResult (..)
  , toolText
  , toolTextWithImages
  , toolFailure
  , withLinkingToolEmittedMessagesToThread
  , withNormalizingToolReplies
  , withRecordingToolSelfMessages
  , withSteering
  , withTypingNotification
  )
where

import Bot.Core.Transcript
import Bot.Agent.Transcript
  ( appendMessage
  , closeInterruptedToolCalls
  )
import Bot.Agent.Core
import Bot.Agent.Control (finishToolTurn)
import Bot.Agent.Middleware.ContextCompaction
  ( withContextCompactionNotice
  )
import Bot.Agent.Middleware.Continuation
  ( withContinuations
  )
import Bot.Agent.Middleware.Observation
  ( ObservationContext
  , withObservation
  )
import Bot.Agent.Middleware.Observation.Types
  ( EventObservation
  , ToolResultObservation
  )
import Bot.Agent.Middleware.Steering
  ( SteeringControl (..)
  , withSteering
  )
import Bot.Agent.Middleware.Tools
  ( withToolFailureRecovery
  , withToolLimit
  , withToolMessage
  )
import Bot.Agent.Middleware.ToolResultCompaction
  ( withToolResultCompaction
  )
import Bot.Agent.Middleware.ToolEmittedMessage
  ( ToolEmittedMessageSink (..)
  , withLinkingToolEmittedMessagesToThread
  , withRecordingToolSelfMessages
  )
import Bot.Agent.Middleware.ToolReplyNormalization
  ( withNormalizingToolReplies
  )
import Bot.Agent.Middleware.Typing
  ( withTypingNotification
  )
import Bot.Agent.ToolRegistry
  ( resolveToolSchemas
  , startToolRun
  )
import qualified Bot.Agent.ToolRegistry as ToolRegistry
import Bot.Agent.Tool (Tool, toolAllowed, toolName)
import Bot.Agent.Tools.Continuation (resumeContinuationTool)
import Bot.Agent.Types
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Agent as AgentEffect
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import Bot.Prelude
import qualified Bot.Util.HList as HList
import qualified Crypto.Random as CryptoRandom
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.Async as Async
import qualified Streaming.Prelude as S

-----------------------------------------------------------------------------------------
-- * Public runners
-----------------------------------------------------------------------------------------

runAgent :: Eff (AgentEffect.Agent : es) a -> Eff es a
runAgent =
  interpret (runAgentOperation \runId -> ToolCallMetadata
    { agentRunId = runId
    , originRunId = runId
    , resourceOwner = Nothing
    })

withAgentMetadata
  :: AgentEffect.Agent :> es
  => (Text -> ToolCallMetadata)
  -> Eff es a
  -> Eff es a
withAgentMetadata metadataFor action =
  interpose (passthroughAgentMetadata metadataFor) action

passthroughAgentMetadata
  :: AgentEffect.Agent :> es
  => (Text -> ToolCallMetadata)
  -> EffectHandler AgentEffect.Agent es
passthroughAgentMetadata metadataFor localEnv = \case
  AgentEffect.RunAgent runtime use ->
    passthrough localEnv $ AgentEffect.RunAgent runtime \configured ->
      use configured{toolCallMetadata = metadataFor configured.runId}

runAgentOperation
  :: (Text -> ToolCallMetadata)
  -> EffectHandler AgentEffect.Agent es
runAgentOperation metadataFor localEnv = \case
    AgentEffect.RunAgent runtime use ->
      localUnlift localEnv (ConcUnlift Persistent Unlimited) \unlift ->
        unlift (use runtime
          { toolCallMetadata = metadataFor runtime.runId
          })

withRun
  :: ( AgentEffect.Agent :> es
     , Chat.Chat :> es
     , AgentAudit.AgentAudit :> es
     , Concurrency.Concurrency :> es
     , LLM.LLM :> es
     , Media.Media :> es
     , KatipE :> es
     , Prim :> es
     , Concurrent :> es
     , IOE :> es
     )
  => Int
  -> Int
  -> Context
  -> [Tool (Eff es)]
  -> (Runtime '[] (Eff es) -> Eff es a)
  -> Eff es a
withRun maxTurns compactionTokenThreshold context tools use = do
  baseRuntime <- startRuntime maxTurns context tools
  AgentEffect.withRun
    (defaultRuntime AgentAudit.agentAuditObserver compactionTokenThreshold baseRuntime)
    use

agentStream
  :: (LLM.LLM :> es, Concurrent :> es, KatipE :> es)
  => Runtime '[] (Eff es)
  -> Transcript
  -> Stream (Of Output) (Eff es) Result
agentStream runtime transcript =
  runtime.aroundAgentRun HList.HNil
    . runProgram runtime
    . runtime.aroundProgram runtime
    $ agentProgram runtime (initialState transcript)

runIdOf :: Runtime context (Eff es) -> Text
runIdOf =
  (.runId)

-----------------------------------------------------------------------------------------
-- * Run setup
-----------------------------------------------------------------------------------------

startRuntime :: (Chat.Chat :> es, Concurrent :> es, IOE :> es) => Int -> Context -> [Tool (Eff es)] -> Eff es (Runtime context (Eff es))
startRuntime maxTurns context tools = do
  runId <- newAgentRunId
  let exposedTools = filter (`toolAllowed` context) tools
      toolCallMetadata = ToolCallMetadata{agentRunId = runId, originRunId = runId, resourceOwner = Nothing}
  runningTools <- traverse (startToolRun context) exposedTools
  let dispatchToolCall metadata =
        ToolRegistry.runToolCall context metadata tools runningTools
  pure Runtime
    { runId
    , toolCallMetadata
    , context
    , tools
    , exposedTools
    , runningTools
    , dispatchToolCall
    , maxTurns = max 1 maxTurns
    , modelInputTranscript = \_ agentState -> pure agentState.transcript
    , aroundProgram = \_ program -> program
    , aroundAgentRun = \_ action -> action
    , aroundModelTurn = \_ agentState action -> action agentState
    , aroundToolTurn = \_ _ action -> action
    , aroundControlCall = \_ _ _ action -> action
    , aroundToolCall = \_ _ _ action -> action
    }

newAgentRunId :: IOE :> es => Eff es Text
newAgentRunId = do
  bytes <- liftIO (CryptoRandom.getRandomBytes 16 :: IO StrictByteString.ByteString)
  pure ("agent-" <> TextEncoding.decodeUtf8 (Base64URL.encodeUnpadded bytes))

initialState :: Transcript -> TurnState
initialState transcript =
  TurnState
    { transcript = closeInterruptedToolCalls transcript
    , nextModelTranscript = Nothing
    , turn = 1
    , modelTokenUsage = Nothing
    }

defaultRuntime
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, LLM.LLM :> es, Media.Media :> es, KatipE :> es, Prim :> es, Concurrent :> es)
  => Observer ObservationContext (Eff es)
  -> Int
  -> Runtime '[ObservationContext, EventObservation es, ToolResultObservation es] (Eff es)
  -> Runtime '[] (Eff es)
defaultRuntime observer compactionTokenThreshold =
  ( withContinuations
  . withToolLimit isResumeTransfer
  . withTypingNotification
  . withToolResultCompaction
  . withObservation observer
  . withToolMessage
  . withContextCompactionNotice compactionTokenThreshold
  . withToolFailureRecovery
  )

isResumeTransfer :: Runtime context (Eff es) -> NonEmpty LLM.ToolCall -> Bool
isResumeTransfer runtime calls =
  case toList calls of
    [call] ->
      call.name == toolName resumeContinuationTool
        && any ((== call.name) . toolName) runtime.exposedTools
    _ ->
      False

agentProgram
  :: Runtime context (Eff es)
  -> TurnState
  -> Program (Eff es) Result
agentProgram runtime agentState = do
  (modelState, answer) <- runModel agentState
  interpretModelAnswer runtime modelState answer

runProgram
  :: (LLM.LLM :> es, Concurrent :> es, KatipE :> es)
  => Runtime '[] (Eff es)
  -> Program (Eff es) Result
  -> Stream (Of Output) (Eff es) Result
runProgram runtime program =
  program.observe >>= \case
    Finished result@Result{status, turnsUsed} -> do
      lift $ logAgentState runtime.runId [i|step=finished status=#{status} turns=#{turnsUsed}|]
      pure result
    Continues next -> do
      lift $ logAgentState runtime.runId "step=continues"
      runProgram runtime next
    Visible event continue ->
      interpretAgentEvent runtime event >>= runProgram runtime . continue

interpretAgentEvent
  :: (LLM.LLM :> es, Concurrent :> es, KatipE :> es)
  => Runtime '[] (Eff es)
  -> AgentEvent response
  -> Stream (Of Output) (Eff es) response
interpretAgentEvent runtime = \case
  RunModel agentState@TurnState{turn, transcript} -> do
    lift $ logAgentState runtime.runId
      [i|step=visible event=RunModel turn=#{turn} messages=#{transcriptMessageCount transcript}|]
    runtime.aroundModelTurn HList.HNil agentState \modelState@TurnState{turn = modelTurn} -> do
      inputTranscript <- lift (runtime.modelInputTranscript HList.HNil modelState)
      answer <- askNext runtime modelState inputTranscript
      lift $ logAgentState runtime.runId
        [i|event=RunModel completed turn=#{modelTurn} answer=#{answerKind answer}|]
      pure (modelState, answer)
  RunTools request@ToolRequest{agentState = TurnState{turn}, toolCalls} ->
    lift do
      logAgentState runtime.runId
        [i|step=visible event=RunTools turn=#{turn} calls=#{toolCallSummary toolCalls}|]
      continuedState@TurnState{turn = nextTurn, transcript} <-
        fst <$> toolPhase runtime request
      logAgentState runtime.runId
        [i|event=RunTools completed next_turn=#{nextTurn} messages=#{transcriptMessageCount transcript}|]
      pure continuedState

logAgentState :: KatipE :> es => Text -> Text -> Eff es ()
logAgentState runId message =
  logDebug [i|Agent state: run=#{runId} #{message}|]

answerKind :: LLM.ChatAnswer -> Text
answerKind = \case
  LLM.ChatFinalAnswer{} ->
    "final"
  LLM.ChatToolRequest{toolCalls} ->
    "tools:" <> toolCallSummary toolCalls

toolCallSummary :: NonEmpty LLM.ToolCall -> Text
toolCallSummary =
  Text.intercalate "," . map (\call -> call.id <> ":" <> call.name) . toList

transcriptMessageCount :: Transcript -> Int
transcriptMessageCount =
  Foldable.length . (.messages)

interpretModelAnswer
  :: Runtime context (Eff es)
  -> TurnState
  -> LLM.ChatAnswer
  -> Program (Eff es) Result
interpretModelAnswer runtime agentState answer =
  case answer of
    LLM.ChatFinalAnswer{content} ->
      pure (agentCompletion runtime "answered" content agentState.turn (LLM.chatAnswerTokenUsage answer) answered)
    LLM.ChatToolRequest{content, toolCalls}
      | Just message <- toolCallIdError toolCalls ->
          modelProtocolError runtime agentState answer message
      | otherwise -> do
          Program do
            S.yield (ToolCallNotification toolCalls)
            pure (Finished ())
          nextState <- runTools ToolRequest{agentState = observedState, answered, toolContent = content, toolCalls}
          agentProgram runtime nextState
  where
    observedState =
      agentState{modelTokenUsage = LLM.chatAnswerTokenUsage answer}
    answered =
      appendMessage (LLM.assistantAnswer answer) agentState.transcript

modelProtocolError
  :: Runtime context (Eff es)
  -> TurnState
  -> LLM.ChatAnswer
  -> Text
  -> Program (Eff es) Result
modelProtocolError runtime agentState answer message =
  Program do
    S.yield (ContentDelta message)
    pure . Finished $
      agentCompletion
        runtime
        "model_protocol_error"
        message
        agentState.turn
        (LLM.chatAnswerTokenUsage answer)
        agentState.transcript

toolCallIdError :: NonEmpty LLM.ToolCall -> Maybe Text
toolCallIdError calls
  | any (Text.null . Text.strip . (.id)) callList =
      Just "Model returned a tool call with an empty id."
  | duplicateIds@(_ : _) <-
      mapMaybe listToMaybe
        . filter ((> 1) . length)
        . group
        . sort
        $ map (.id) callList =
      Just [i|Model returned duplicate tool-call ids: #{Text.intercalate ", " duplicateIds}|]
  | otherwise =
      Nothing
  where
    callList = toList calls

-- | Interpret one tool phase and advance to the next model phase.
toolPhase
  :: (Concurrent :> es, KatipE :> es)
  => Runtime '[] (Eff es)
  -> ToolRequest
  -> Eff es (TurnState, NonEmpty ToolResult)
toolPhase runtime@Runtime{aroundToolTurn = toolTurn} request =
  finishToolTurn toolTurn request $
    Async.mapConcurrently (executeToolCall runtime request.agentState.turn) request.toolCalls

-----------------------------------------------------------------------------------------
-- * Model helpers
-----------------------------------------------------------------------------------------

-- | Ask the LLM for the next assistant message.
askNext
  :: (LLM.LLM :> es, Concurrent :> es)
  => Runtime context (Eff es)
  -> TurnState
  -> Transcript
  -> Stream (Of Output) (Eff es) LLM.ChatAnswer
askNext runtime agentState transcript = do
  schemas <- lift (resolveToolSchemas agentState.transcript agentState.turn runtime.runningTools)
  S.map ContentDelta $
    LLM.askWithToolsStreaming
      schemas
      (agentRequestMessages runtime.context transcript)

agentRequestMessages :: Context -> Transcript -> [LLM.ChatMessage]
agentRequestMessages context (Transcript messages) =
  mergeSystemContext context.systemContext (Foldable.toList messages)

mergeSystemContext :: Text -> [LLM.ChatMessage] -> [LLM.ChatMessage]
mergeSystemContext context messages
  | Text.null strippedContext = messages
  | otherwise =
      case messages of
        firstMessage : rest
          | firstMessage.role == "system"
          , Just (LLM.TextContent systemPrompt) <- firstMessage.content ->
              replaceMessageContent (Just (LLM.TextContent (joinSystemPrompts systemPrompt strippedContext))) firstMessage : rest
        _ ->
          LLM.systemText strippedContext : messages
  where
    strippedContext = Text.strip context

joinSystemPrompts :: Text -> Text -> Text
joinSystemPrompts systemPrompt context =
  Text.strip $ Text.intercalate "\n\n" [systemPrompt, context]

replaceMessageContent :: Maybe LLM.MessageContent -> LLM.ChatMessage -> LLM.ChatMessage
replaceMessageContent content LLM.ChatMessage{role, toolCalls, toolCallId} =
  LLM.ChatMessage role content toolCalls toolCallId

-----------------------------------------------------------------------------------------
-- * Tool execution
-----------------------------------------------------------------------------------------

-- | Run one tool call and convert failures into tool-visible text.
--
-- Tool failures must still produce a tool result message; otherwise the next
-- LLM request would contain an assistant tool call without its required result.
executeToolCall :: (Concurrent :> es, KatipE :> es) => Runtime '[] (Eff es) -> Int -> LLM.ToolCall -> Eff es ToolResult
executeToolCall runtime@Runtime{runId} turn call@LLM.ToolCall{id = callId, name, arguments} = do
  logDebug
    [i|Agent tool: run=#{runId} turn=#{turn} id=#{callId} name=#{name} state=started argument_chars=#{Text.length arguments}|]
  result <-
    runtime.aroundToolCall turn call HList.HNil
      (runtime.dispatchToolCall runtime.toolCallMetadata call)
      `onException` logDebug
        [i|Agent tool: run=#{runId} turn=#{turn} id=#{callId} name=#{name} state=interrupted|]
  logDebug
    [i|Agent tool: run=#{runId} turn=#{turn} id=#{callId} name=#{name} state=finished status=#{toolResultStatus result} result_chars=#{Text.length (toolResultContent result)} images=#{length (toolResultImageUrls result)}|]
  pure result

toolResultStatus :: ToolResult -> Text
toolResultStatus =
  maybe "ok" failureStatus . toolResultFailure

-----------------------------------------------------------------------------------------
-- * Completion
-----------------------------------------------------------------------------------------

agentCompletion :: Runtime context (Eff es) -> Text -> Text -> Int -> Maybe LLM.TokenUsage -> Transcript -> Result
agentCompletion runtime status answer turnsUsed tokenUsage transcript =
  Result
    { runId = runtime.runId
    , transcript
    , status
    , finalText = answer
    , turnsUsed
    , tokenUsage
    }
