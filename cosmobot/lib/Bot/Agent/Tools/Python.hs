module Bot.Agent.Tools.Python
  ( PythonRequest (..)
  , runPythonDescription
  , runPythonRequest
  , runPythonTool
  , runPythonToolName
  , isPythonProgramControl
  )
where

import Bot.Agent.Tool
import Bot.Agent.Tools.Common (requiredText, specialTag, workTag)
import Bot.Agent.Tools.Continuation (isContinuationToolName)
import Bot.Agent.Types
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

newtype PythonRequest = PythonRequest
  { code :: Text
  }
  deriving stock (Eq, Show)

runPythonToolName :: Text
runPythonToolName =
  "py"

isPythonProgramControl :: Text -> Bool
isPythonProgramControl name =
  name == runPythonToolName
    || name == toolEnableName
    || isContinuationToolName name

runPythonTool :: Tool (Eff es)
runPythonTool =
  tagged [specialTag, workTag]
    . allowWhen (.toolConfig.python.enabled)
    . withDescriptionBy (runPythonDescriptionFor . (.toolConfig.python))
    $ tool runPythonToolName
        (requiredText "code" "Complete Python source to execute with compile(..., '<py>', 'exec').")
        \_ -> pure (toolFailure (permanentArgumentFailure unavailable unavailable))
  where
    unavailable = "py requires the structural Python interpreter."

runPythonRequest :: LLM.ToolCall -> Maybe (Either Text PythonRequest)
runPythonRequest call
  | call.name /= runPythonToolName = Nothing
  | otherwise = Just do
      value <- first Text.pack (Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 call.arguments))
      first Text.pack (AesonTypes.parseEither parser value)
  where
    parser =
      Aeson.withObject "py arguments" \object ->
        PythonRequest <$> object Aeson..: Key.fromText "code"

runPythonDescription :: Text
runPythonDescription = runPythonDescriptionFor defaultPythonConfig

runPythonDescriptionFor :: PythonConfig -> Text
runPythonDescriptionFor PythonConfig{wallTimeoutSeconds, cpuSeconds, memoryMiB, maxToolCalls} = Text.unlines
  [ "Execute a fresh, sandboxed Python 3 program. Use `py` for one-shot scripting or multiple tool calls with sequencing, branching, loops, aggregation, or recovery. Call one tool directly for a simple single operation. `py` must be the only tool call in its model-issued batch."
  , ""
  , "API (`import cosmobot`; `Json` is any JSON value):"
  , "```python"
  , "class Failure(TypedDict):"
  , "    category: str"
  , "    message: str"
  , "    detail: str"
  , "class ToolCall(TypedDict):"
  , "    name: str"
  , "    args: Json"
  , "class ToolSuccess(TypedDict):"
  , "    ok: Literal[True]"
  , "    content: str"
  , "class ToolFailure(TypedDict):"
  , "    ok: Literal[False]"
  , "    failure: Failure"
  , "class RunToolException(Exception):"
  , "    name: str | None"
  , "    index: int | None"
  , "    failure: Failure"
  , "    results: list[ToolSuccess | ToolFailure]"
  , "def run_tool(name: str, args: Json) -> ToolSuccess: ..."
  , "def run_tools(calls: list[ToolCall]) -> list[ToolSuccess]: ..."
  , "def complete(content: str = '') -> NoReturn: ..."
  , "def fail(message: str) -> NoReturn: ..."
  , "```"
  , ""
  , "`run_tools` executes a non-empty batch concurrently and preserves input order. After all calls finish, the first failure raises `RunToolException`; `results` contains every outcome."
  , ""
  , "`complete` returns its text as the outer tool result; `fail` fails it. Both stop execution after `finally` blocks and bypass `except Exception`. Normal fall-through returns stdout, or `Python completed successfully.` when empty. Nested tool side effects persist."
  , ""
  , "Each call gets a new interpreter and a fresh writable `/work`; files disappear afterward. The Python standard library and networking, including host loopback, are available. Host files, child processes, threads, and `file://` URLs are unavailable."
  , ""
  , "Tool names and argument schemas are the tools visible in this model request. Enable needed tool tags before calling `py`. Program-control tools are host-only."
  , ""
  , [i|Configured limits: #{wallTimeoutSeconds} s wall time, #{cpuSeconds} s CPU, #{memoryMiB} MiB address space, and #{maxToolCalls} total nested tool calls. The batch limit is 16 calls.|]
  ]
