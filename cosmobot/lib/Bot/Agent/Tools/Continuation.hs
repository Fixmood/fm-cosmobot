{-|
Module      : Bot.Agent.Tools.Continuation
Description : Agent continuation control tools and state
Stability   : experimental
-}
module Bot.Agent.Tools.Continuation
  ( ContinuationRequest (..)
  , captureContinuationTool
  , continuationRequest
  , isContinuationToolName
  , resumeContinuationTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Agent.Types
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

data ContinuationRequest
  = CaptureContinuation !(Maybe Text)
  | ResumeContinuation !Text !Aeson.Value

captureContinuationTool :: Tool (Eff es)
captureContinuationTool =
  controlTool
    "capture_continuation"
    "Agent-context setjmp. Call alone before speculative exploration. On state=captured, save continuation_id and explore; on state=resumed, use value and do not repeat the abandoned branch. One-shot, nested, and run-local. Resuming discards later model context, not tool side effects or visible output."
    (parsedArguments
      (objectSchema [fieldText "label" "Optional description of the exploration this return point precedes."] [])
      captureParser)

resumeContinuationTool :: Tool (Eff es)
resumeContinuationTool =
  controlTool
    "resume_continuation"
    "Agent-context longjmp. Call alone with a continuation_id and a self-contained JSON result. Control resumes at the matching capture_continuation with state=resumed and value; this call does not return here. Consumes that continuation and newer nested ones; older captures remain. Discards model context, not tool side effects or visible output."
    (parsedArguments
      (objectSchema
        [ fieldText "continuation_id" "Exact id returned by capture_continuation."
        , ("value", Aeson.object ["description" Aeson..= ("Self-contained arbitrary JSON result returned to the capture point." :: Text)])
        ]
        ["continuation_id", "value"]
      )
      resumeParser)

controlTool :: Text -> Text -> ParsedArguments a -> Tool (Eff es)
controlTool name description arguments =
  withDescription description
  $ tool name arguments \_ ->
      pure (toolFailure (permanentArgumentFailure failure failure))
  where
    failure = [i|Tool #{name} requires an agent program with continuation middleware.|]

isContinuationToolName :: Text -> Bool
isContinuationToolName name =
  name == toolName captureContinuationTool || name == toolName resumeContinuationTool

continuationRequest :: LLM.ToolCall -> Maybe (Either Text ContinuationRequest)
continuationRequest call
  | call.name == toolName captureContinuationTool =
      Just (decodeArguments call captureParser)
  | call.name == toolName resumeContinuationTool =
      Just (decodeArguments call resumeParser)
  | otherwise =
      Nothing

decodeArguments :: LLM.ToolCall -> (Aeson.Value -> AesonTypes.Parser a) -> Either Text a
decodeArguments call parser = do
  value <- first Text.pack (Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 call.arguments))
  first Text.pack (AesonTypes.parseEither parser value)

captureParser :: Aeson.Value -> AesonTypes.Parser ContinuationRequest
captureParser =
  Aeson.withObject "capture_continuation arguments" \object ->
    CaptureContinuation <$> object Aeson..:? Key.fromText "label"

resumeParser :: Aeson.Value -> AesonTypes.Parser ContinuationRequest
resumeParser =
  Aeson.withObject "resume_continuation arguments" \object ->
    ResumeContinuation
      <$> object Aeson..: Key.fromText "continuation_id"
      <*> object Aeson..: Key.fromText "value"
