{-|
Module      : Bot.Agent.Middleware.ToolResultCompaction
Description : Agent middleware for replacing consumed large tool results
Stability   : experimental
-}

module Bot.Agent.Middleware.ToolResultCompaction
  ( maxToolResultPreviewChars
  , maxImmediateToolResultChars
  , compactLargeToolResultText
  , compactLargeToolResultsInTranscript
  , compactLargeToolResultsInMessages
  , withToolResultCompaction
  )
where

import Bot.Agent.Core
import Bot.Agent.Middleware.Observation.Types (ToolResultObservation (..))
import Bot.Agent.Types (toolResultContent)
import Bot.Core.Transcript
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import qualified Bot.Media.Mime as Mime
import Bot.Prelude
import qualified Bot.Util.HList as HList
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Streaming.ByteString as Q

maxToolResultPreviewChars :: Int
maxToolResultPreviewChars =
  4096

maxImmediateToolResultChars :: Int
maxImmediateToolResultChars =
  10000

withToolResultCompaction
  :: Media.Media :> es
  => Runtime (ToolResultObservation es ': context) (Eff es)
  -> Runtime context (Eff es)
withToolResultCompaction program@Runtime{aroundToolTurn = toolTurn} =
  program
    { aroundAgentRun = \context action ->
        program.aroundAgentRun (toolResultObservation HList.:& context) action
    , modelInputTranscript = \context agentState ->
        case agentState.nextModelTranscript of
          Just transcript ->
            pure transcript
          Nothing ->
            program.modelInputTranscript (toolResultObservation HList.:& context) agentState
    , aroundModelTurn = \context agentState action ->
        clearConsumedModelInput
          <$> program.aroundModelTurn (toolResultObservation HList.:& context) agentState action
    , aroundToolTurn = \context toolState action -> do
        (fullState, result) <- toolTurn (toolResultObservation HList.:& context) toolState action
        immediateTranscript <- compactToolResultsInTranscript maxImmediateToolResultChars fullState.transcript
        compactedTranscript <- compactLargeToolResultsInTranscript immediateTranscript
        pure
          ( fullState
              { transcript = compactedTranscript
              , nextModelTranscript = Just immediateTranscript
              }
          , result
          )
    , aroundControlCall = \turn call context action ->
        program.aroundControlCall turn call (toolResultObservation HList.:& context) action
    , aroundToolCall = \turn call context action ->
        program.aroundToolCall turn call (toolResultObservation HList.:& context) action
    }
  where
    toolResultObservation =
      ToolResultObservation (compactLargeToolResultText . toolResultContent)

clearConsumedModelInput
  :: (TurnState, answer)
  -> (TurnState, answer)
clearConsumedModelInput (agentState, answer) =
  (agentState{nextModelTranscript = Nothing}, answer)

compactLargeToolResultsInMessages :: Media.Media :> es => [LLM.ChatMessage] -> Eff es [LLM.ChatMessage]
compactLargeToolResultsInMessages =
  traverse compactLargeToolResultMessage

compactLargeToolResultsInTranscript :: Media.Media :> es => Transcript -> Eff es Transcript
compactLargeToolResultsInTranscript =
  compactToolResultsInTranscript maxToolResultPreviewChars

compactLargeToolResultText :: Media.Media :> es => Text -> Eff es Text
compactLargeToolResultText =
  compactToolResultText maxToolResultPreviewChars

compactToolResultsInTranscript :: Media.Media :> es => Int -> Transcript -> Eff es Transcript
compactToolResultsInTranscript maxChars (Transcript messages) =
  Transcript . Seq.fromList <$> traverse (compactToolResultMessage maxChars) (Foldable.toList messages)

compactToolResultText :: Media.Media :> es => Int -> Text -> Eff es Text
compactToolResultText maxChars text
  | Text.length text <= maxChars || isOmittedToolResult text =
      pure text
  | otherwise = do
      let bytes = TextEncoding.encodeUtf8 text
          mime = Mime.sniffTextMime bytes text
      mediaRef <- Media.storeMediaObject Media.MediaObject
        { bytes = Q.fromStrict bytes
        , mimeType = mime
        , sourceName = Just (sourceNameForMime mime)
        }
      pure (maybe (omittedWithoutMedia mime bytes text) (\ref -> omittedWithMedia ref mime bytes text) mediaRef)

compactLargeToolResultMessage :: Media.Media :> es => LLM.ChatMessage -> Eff es LLM.ChatMessage
compactLargeToolResultMessage =
  compactToolResultMessage maxToolResultPreviewChars

compactToolResultMessage :: Media.Media :> es => Int -> LLM.ChatMessage -> Eff es LLM.ChatMessage
compactToolResultMessage maxChars message@LLM.ChatMessage{role = "tool", content = Just (LLM.TextContent text)} = do
  content <- LLM.TextContent <$> compactToolResultText maxChars text
  pure LLM.ChatMessage
    { role = message.role
    , content = Just content
    , toolCalls = message.toolCalls
    , toolCallId = message.toolCallId
    }
compactToolResultMessage _ message =
  pure message

omittedWithMedia :: Text -> Text -> StrictByteString.ByteString -> Text -> Text
omittedWithMedia mediaRef mime bytes text =
  [i|[tool result omitted; media_id=#{displayMediaId mediaRef}, mime=#{mime}, size=#{StrictByteString.length bytes}, preview=#{previewJson text}]|]

omittedWithoutMedia :: Text -> StrictByteString.ByteString -> Text -> Text
omittedWithoutMedia mime bytes text =
  [i|[tool result omitted; media_id=unavailable, mime=#{mime}, size=#{StrictByteString.length bytes}, preview=#{previewJson text}]|]

displayMediaId :: Text -> Text
displayMediaId ref =
  fromMaybe ref (Text.stripPrefix "media:" ref)

previewJson :: Text -> Text
previewJson =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode . Text.take maxToolResultPreviewChars

sourceNameForMime :: Text -> Text
sourceNameForMime mime =
  case Text.toLower (Text.takeWhile (/= ';') mime) of
    "application/json" -> "tool-result.json"
    "text/html" -> "tool-result.html"
    "application/xml" -> "tool-result.xml"
    "text/xml" -> "tool-result.xml"
    _ -> "tool-result.txt"

isOmittedToolResult :: Text -> Bool
isOmittedToolResult =
  ("[tool result omitted;" `Text.isPrefixOf`)
