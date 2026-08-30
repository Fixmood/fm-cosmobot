{-|
Module      : Bot.Agent.Tools.Meta
Description : Agent meta tools
Stability   : experimental
-}
module Bot.Agent.Tools.Meta
  ( toolEnableTool
  )
where

import Bot.Agent.Tool
import Bot.Agent.Tools.Common
import Bot.Agent.Types
import Bot.Prelude
import qualified Data.Text as Text

toolEnableTool :: Tool (Eff es)
toolEnableTool =
  withDescription "Enable additional tool tags for the current thread."
  $ tool toolEnableName
      (requiredArgument (fieldTextArray "tags" "Tool tags to enable."))
      \tags ->
        pure . toolText $
          "Enabled tool tags: " <> Text.intercalate ", " (ordNub tags)
