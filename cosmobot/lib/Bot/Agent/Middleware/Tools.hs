{-# LANGUAGE DataKinds #-}
{-|
Module      : Bot.Agent.Middleware.Tools
Description : Tool-related agent program middleware
Stability   : experimental
-}

module Bot.Agent.Middleware.Tools
  ( withToolFailureRecovery
  , withToolLimit
  , withToolMessage
  , toolProgressText
  )
where

import Bot.Agent.Transcript
  ( appendMessages
  , pausedToolResult
  )
import Bot.Agent.Core
import Bot.Agent.Middleware.Observation.Types
import Bot.Agent.Tool
import qualified Bot.Agent.ToolRegistry as ToolRegistry
import Bot.Agent.Types
import Bot.Core.Transcript
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Chat.Bridge.FM as FMBridge
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Bot.Util.HList as HList
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes

withToolFailureRecovery :: Runtime context (Eff es) -> Runtime context (Eff es)
withToolFailureRecovery program =
  program
    { aroundControlCall = \turn call context action ->
        safeToolCall call (program.aroundControlCall turn call context action)
    , aroundToolCall = \turn call context action ->
        safeToolCall call (program.aroundToolCall turn call context action)
    }

withToolMessage :: (Chat.Chat :> es, Concurrent :> es, HList.Has ObservationContext context) => Runtime context (Eff es) -> Runtime context (Eff es)
withToolMessage program =
  program
    { aroundControlCall = \turn call context action -> do
        announceNoisyTool program call context
        program.aroundControlCall turn call context action
    , aroundToolCall = \turn call context action -> do
        announceNoisyTool program call context
        program.aroundToolCall turn call context action
    }

announceNoisyTool :: (Chat.Chat :> es, Concurrent :> es, HList.Has ObservationContext context) => Runtime context (Eff es) -> LLM.ToolCall -> HList.HList context -> Eff es ()
announceNoisyTool program call context =
  case find ((== call.name) . toolName) program.tools of
    Just definition
      | toolIsNoisy definition || importantDynamicTool call -> do
          shouldAnnounce <-
            maybe (pure True) ToolRegistry.claimToolAnnouncement
              (find ((== call.name) . (.name)) program.runningTools)
          when shouldAnnounce $
            void $ Chat.replyTo program.context.message (toolMessageText call context)
    _ ->
      pure ()

importantDynamicTool :: LLM.ToolCall -> Bool
importantDynamicTool call =
  call.name == "sandbox"
    && fromMaybe False (AesonTypes.parseMaybe parseRun =<< Aeson.decodeStrict (encodeUtf8 call.arguments))
  where
    parseRun = Aeson.withObject "sandbox call" $ \o -> (== ("run" :: Text)) <$> o Aeson..: "op"

toolMessageText :: HList.Has ObservationContext context => LLM.ToolCall -> HList.HList context -> Text
toolMessageText call _context =
  toolProgressText call.name

toolProgressText :: Text -> Text
toolProgressText calledToolName =
  FMBridge.fmReplyBody [i|正在调用 #{calledToolName} 工具…|]

-- | Pause before executing another tool turn.
--
-- The assistant message already contains tool calls, and OpenAI-compatible
-- chat history requires every tool call to be followed by a tool result. We
-- therefore append synthetic "paused" tool results so the saved transcript is
-- valid when the user later continues.
handleToolLimit
  :: Text
  -> Int
  -> Text
  -> NonEmpty LLM.ToolCall
  -> Transcript
  -> Stream (Of Output) (Eff es) Result
handleToolLimit runId turn _content calls answered = do
  let paused = appendMessages (toList (fmap pausedToolResult calls)) answered
  pure Result
    { runId
    , transcript = paused
    , status = "tool_limit"
    , finalText = ""
    , turnsUsed = turn
    , tokenUsage = Nothing
    }

withToolLimit
  :: KatipE :> es
  => (Runtime '[] (Eff es) -> NonEmpty LLM.ToolCall -> Bool)
  -> Runtime context (Eff es)
  -> Runtime context (Eff es)
withToolLimit mayTransfer runtime =
  runtime
    { aroundProgram = \finalRuntime ->
        limitProgram finalRuntime (mayTransfer finalRuntime)
          . runtime.aroundProgram finalRuntime
    }

limitProgram
  :: KatipE :> es
  => Runtime '[] (Eff es)
  -> (NonEmpty LLM.ToolCall -> Bool)
  -> Program (Eff es) Result
  -> Program (Eff es) Result
limitProgram runtime mayTransfer (Program action) =
  Program $ action >>= \case
    Finished result ->
      pure (Finished result)
    Continues next ->
      pure (Continues (limitProgram runtime mayTransfer next))
    Visible (RunTools request) continue
      | request.agentState.turn >= runtime.maxTurns
      , not (mayTransfer request.toolCalls) ->
          finishAtLimit runtime request
      | otherwise ->
          pure (Visible (RunTools request) (limitProgram runtime mayTransfer . continue))
    Visible event continue ->
      pure (Visible event (limitProgram runtime mayTransfer . continue))

finishAtLimit
  :: KatipE :> es
  => Runtime '[] (Eff es)
  -> ToolRequest
  -> Stream (Of Output) (Eff es) (Step (Eff es) Result)
finishAtLimit runtime request = do
  let calls = request.toolCalls
  lift $ logInfo [i|Agent tool turn limit reached: #{show calls :: String}|]
  Finished
    <$> handleToolLimit
          runtime.runId
          request.agentState.turn
          request.toolContent
          calls
          request.answered

safeToolCall :: LLM.ToolCall -> Eff es ToolResult -> Eff es ToolResult
safeToolCall call action =
  action `catchSync` \err -> do
    let failure = failureFromException err
        message = failure.userMessage
    pure (toolFailure failure{userMessage = [i|Tool #{callName} failed: #{message}|]})
  where
    callName = call.name
