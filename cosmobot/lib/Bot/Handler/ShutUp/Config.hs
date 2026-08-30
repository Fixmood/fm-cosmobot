{-|
Module      : Bot.Handler.ShutUp.Config
Description : Shut-up handler configuration
Stability   : experimental
-}

module Bot.Handler.ShutUp.Config
  ( ShutUpConfig (..)
  , DeletePattern (..)
  , defaultShutUpConfig
  )
where

import Bot.Prelude
import Prelude (Show (..), showString)
import Toml.Schema
import Text.Regex.TDFA
  ( Regex
  , defaultCompOpt
  , defaultExecOpt
  , makeRegexOptsM
  )

-- | Compiled message deletion rule.
data DeletePattern = DeletePattern
  { source :: !Text
  , regex :: !Regex
  }

instance Show DeletePattern where
  showsPrec _ DeletePattern{source} =
    showString [i|DeletePattern #{source}|]

newtype ShutUpConfig = ShutUpConfig
  { deletePatterns :: [DeletePattern]
  }
  deriving (Show)

defaultShutUpConfig :: ShutUpConfig
defaultShutUpConfig = ShutUpConfig
  { deletePatterns = []
  }

instance FromValue ShutUpConfig where
  fromValue = parseTableFromValue do
    patterns <- fromMaybe [] <$> optKey "delete_patterns"
    deletePatterns <- traverse compileDeletePattern patterns
    pure ShutUpConfig{deletePatterns}

compileDeletePattern :: Text -> ParseTable l DeletePattern
compileDeletePattern source =
  case makeRegexOptsM defaultCompOpt defaultExecOpt source :: Either String Regex of
    Left err ->
      fail [i|Invalid handler.shutup.delete_patterns regex #{source}: #{err}|]
    Right regex ->
      pure DeletePattern{source, regex}
