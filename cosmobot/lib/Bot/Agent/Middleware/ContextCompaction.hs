{-|
Module      : Bot.Agent.Middleware.ContextCompaction
Description : Agent transcript compaction middleware
Stability   : experimental
-}
module Bot.Agent.Middleware.ContextCompaction
  ( withContextCompaction
  , withContextCompactionNotice
  )
where

import Bot.Agent.Core
import Bot.Agent.Middleware.Observation.Types (EventObservation (..))
import Bot.Agent.Tool (toolEnableName)
import Bot.Agent.Types (Context (..), Event (..))
import Bot.Core.Transcript
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Bot.Util.HList as HList
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

recentMessageWindow :: Int
recentMessageWindow =
  20

compactionNoticeMessage :: Text
compactionNoticeMessage =
  "正在整理较早的对话上下文..."

withContextCompaction
  :: LLM.LLM :> es
  => Int
  -> Runtime context (Eff es)
  -> Runtime context (Eff es)
withContextCompaction tokenThreshold program =
  withContextCompactionUsing
    (\_ agentState -> fst <$> compactAgentState tokenThreshold (pure ()) agentState)
    program

withContextCompactionNotice
  :: forall es context.
     ( Chat.Chat :> es
     , LLM.LLM :> es
     , HList.Has (EventObservation es) context
     )
  => Int
  -> Runtime context (Eff es)
  -> Runtime context (Eff es)
withContextCompactionNotice tokenThreshold program =
  withContextCompactionUsing
    (\context agentState -> do
        (compactedState, compacted) <-
          compactAgentState
            tokenThreshold
            (void $ Chat.replyTo program.context.message compactionNoticeMessage)
            agentState
        for_ compacted \CompactionDetails{messageCount, tokenUsage} ->
          void $ (HList.get @(EventObservation es) context).observeAgentEvent ContextCompacted
            { runId = program.runId
            , turn = agentState.turn
            , messageCount
            , tokenUsage
            }
        pure compactedState
    )
    program

withContextCompactionUsing
  :: (HList.HList context -> TurnState -> Eff es TurnState)
  -> Runtime context (Eff es)
  -> Runtime context (Eff es)
withContextCompactionUsing compact program =
  program
    { aroundModelTurn = \context agentState action -> do
        compactedState <- lift (compact context agentState)
        program.aroundModelTurn context compactedState action
    }

data CompactionDetails = CompactionDetails
  { messageCount :: !Int
  , tokenUsage :: !(Maybe LLM.TokenUsage)
  }

compactAgentState
  :: LLM.LLM :> es
  => Int
  -> Eff es ()
  -> TurnState
  -> Eff es (TurnState, Maybe CompactionDetails)
compactAgentState tokenThreshold notify agentState
  | not (shouldCompact tokenThreshold agentState.modelTokenUsage) =
      pure (agentState, Nothing)
  | otherwise = do
      let modelTranscript = selectedTranscript agentState
          (older, _) = compactableTranscriptParts modelTranscript
      if Seq.null older
        then pure (agentState{modelTokenUsage = Nothing}, Nothing)
        else do
          notify
          (summary, tokenUsage) <- summarizeMessages (Foldable.toList older)
          let modelCompactedTranscript = compactTranscriptWithSummary summary modelTranscript
              canonicalCompactedTranscript = compactTranscriptWithSummary summary agentState.transcript
          pure
            ( TurnState
                { transcript = canonicalCompactedTranscript
                , nextModelTranscript = Just modelCompactedTranscript
                , turn = agentState.turn
                , modelTokenUsage = Nothing
                }
            , Just CompactionDetails
                { messageCount = Foldable.length modelTranscript.messages
                , tokenUsage
                }
            )

selectedTranscript :: TurnState -> Transcript
selectedTranscript agentState =
  fromMaybe agentState.transcript agentState.nextModelTranscript

compactableTranscriptParts :: Transcript -> (Seq.Seq LLM.ChatMessage, Seq.Seq LLM.ChatMessage)
compactableTranscriptParts (Transcript messages) =
  splitCompactablePrefix messages

compactTranscriptWithSummary :: Text -> Transcript -> Transcript
compactTranscriptWithSummary summary transcript =
  let (older, newer) = compactableTranscriptParts transcript
      preserved = preserveToolEnableCalls older
  in Transcript (LLM.systemText (summaryMessage summary) Seq.<| (preserved <> newer))

preserveToolEnableCalls :: Seq.Seq LLM.ChatMessage -> Seq.Seq LLM.ChatMessage
preserveToolEnableCalls messages =
  Seq.fromList
    [ preserved
    | message <- Foldable.toList messages
    , call <- message.toolCalls
    , call.name == toolEnableName
    , preserved <-
        [ LLM.ChatMessage "assistant" Nothing [call] Nothing
        , fromMaybe (LLM.toolResult call "Enabled.") (find ((== Just call.id) . (.toolCallId)) toolResults)
        ]
    ]
  where
    toolResults =
      filter ((== "tool") . (.role)) (Foldable.toList messages)

shouldCompact :: Int -> Maybe LLM.TokenUsage -> Bool
shouldCompact tokenThreshold usage =
  maybe False ((>= tokenThreshold) . (.totalTokens)) usage

splitCompactablePrefix :: Seq.Seq LLM.ChatMessage -> (Seq.Seq LLM.ChatMessage, Seq.Seq LLM.ChatMessage)
splitCompactablePrefix messages =
  let cutoff = max 0 (Seq.length messages - recentMessageWindow)
      (older, newer) = Seq.splitAt cutoff messages
      (leadingToolResults, rest) = Seq.spanl ((== "tool") . (.role)) newer
  in (older <> leadingToolResults, rest)

summarizeMessages :: LLM.LLM :> es => [LLM.ChatMessage] -> Eff es (Text, Maybe LLM.TokenUsage)
summarizeMessages messages = do
  answer <- LLM.askWithTools []
    [ LLM.systemText summarySystemPrompt
    , LLM.userText [i|Summarize this chat transcript for future continuation. Preserve user goals, decisions, constraints, tool results, generated artifacts, unresolved tasks, and any facts needed to answer later follow-up messages.

Transcript JSON:
#{messagesJson messages}|]
    ]
  pure (Text.strip answer.content, answer.tokenUsage)

summarySystemPrompt :: Text
summarySystemPrompt =
  "You compact chatbot transcript into a durable continuation summary. Return a concise but complete summary. Do not invent facts. Keep identifiers, file paths, URLs, commands, and tool results precise."

summaryMessage :: Text -> Text
summaryMessage summary =
  Text.strip [i|The earlier transcript was compacted. Use this summary as context for the continuation:

#{summary}|]

messagesJson :: [LLM.ChatMessage] -> Text
messagesJson =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode
