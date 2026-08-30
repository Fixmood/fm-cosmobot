module Bot.Agent.Program.Python
  ( PythonToolCall (..)
  , PythonExit (..)
  , pythonExitResult
  )
where

import Bot.Agent.Types
import Bot.Prelude

-- | One worker-originated call. The host, not Python, supplies the audit id.
data PythonToolCall = PythonToolCall
  { name :: !Text
  , arguments :: !Text
  }
  deriving stock (Eq, Show)

data PythonExit
  = PythonCompleted !Text
  | PythonFailed !Failure
  deriving stock (Eq, Show)

pythonExitResult :: PythonExit -> ToolResult
pythonExitResult = \case
  PythonCompleted content -> toolText content
  PythonFailed failure -> toolFailure failure
