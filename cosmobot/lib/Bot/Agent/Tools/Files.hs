{-|
Module      : Bot.Agent.Tools.Files
Description : Agent filesystem tools
Stability   : experimental
-}

module Bot.Agent.Tools.Files
  ( acpReadClientFileTool
  , acpWriteClientFileTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Agent.Types
import Bot.Core.Message
import qualified Bot.Effect.ACP as ACP
import Bot.Prelude

acpReadClientFileTool :: ACP.ACP :> es => Tool (Eff es)
acpReadClientFileTool =
  tagged [workTag]
  . allowWhen acpOnly
  . withDescription "Read a UTF-8 text file from the connected ACP client workspace. Relative paths are resolved against the ACP session cwd."
  $ tool "acp_read_client_file"
      ( requiredText "path" "Absolute file path, or a path relative to the ACP session cwd."
      , optionalInt "line" "Optional 1-based starting line."
      , optionalInt "limit" "Optional maximum number of lines to read."
      )
      \path line limit -> do
        context <- askToolContext
        ACP.readClientFile context.message path line limit >>= \case
          Left err ->
            pure (clientFileFailure err)
          Right content ->
            pure (toolText content)

acpWriteClientFileTool :: ACP.ACP :> es => Tool (Eff es)
acpWriteClientFileTool =
  tagged [workTag]
  . allowWhen acpOnly
  . withDescription "Write a UTF-8 text file in the connected ACP client workspace. Relative paths are resolved against the ACP session cwd."
  $ tool "acp_write_client_file"
      ( requiredText "path" "Absolute file path, or a path relative to the ACP session cwd."
      , requiredText "content" "Complete UTF-8 text content to write."
      )
      \path content -> do
        context <- askToolContext
        ACP.writeClientFile context.message path content >>= \case
          Left err ->
            pure (clientFileFailure err)
          Right () ->
            pure (toolText "File written.")

acpOnly :: Context -> Bool
acpOnly context =
  context.message.platform == PlatformACP

clientFileFailure :: Text -> ToolResult
clientFileFailure err =
  toolFailure (permanentArgumentFailure err err)
