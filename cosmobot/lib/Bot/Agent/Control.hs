{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Bot.Agent.Control
  ( finishToolTurn
  , runControlTurn
  )
where

import Bot.Agent.Core
import Bot.Agent.Transcript (appendMessages)
import Bot.Agent.Types
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Bot.Util.HList as HList
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

finishToolTurn
  :: Monad m
  => (forall a. HList.HList '[] -> ToolRequest -> m (TurnState, a) -> m (TurnState, a))
  -> ToolRequest
  -> m (NonEmpty ToolResult)
  -> m (TurnState, NonEmpty ToolResult)
finishToolTurn aroundToolTurn request action =
  aroundToolTurn HList.HNil request do
    results <- action
    let executions = NonEmpty.zip request.toolCalls results
        resultMessages = map (uncurry toolResultMessage) (toList executions)
        imageMessages = concatMap (uncurry toolImageMessages) executions
    pure
      ( request.agentState
          { transcript = appendMessages (resultMessages <> imageMessages) request.answered
          , turn = request.agentState.turn + 1
          }
      , results
      )

runControlTurn
  :: Monad m
  => Runtime '[] m
  -> ToolRequest
  -> LLM.ToolCall
  -> m ToolResult
  -> m (TurnState, ToolResult)
runControlTurn Runtime{aroundToolTurn, aroundControlCall} request call action =
  finishToolTurn aroundToolTurn request do
    (:| []) <$> aroundControlCall request.agentState.turn call HList.HNil action
  <&> \(nextState, results) -> (nextState, head results)

toolResultMessage :: LLM.ToolCall -> ToolResult -> LLM.ChatMessage
toolResultMessage call =
  LLM.toolResult call . toolResultContent

toolImageMessages :: LLM.ToolCall -> ToolResult -> [LLM.ChatMessage]
toolImageMessages call result =
  imageMessages
  where
    calledToolName = call.name
    imageMessages =
      [ LLM.userWithImages
          (Text.strip [i|Image context returned by tool #{calledToolName}:
#{toolResultContent result}|])
          imageUrls
      | let imageUrls = toolResultImageUrls result
      , not (null imageUrls)
      ]
