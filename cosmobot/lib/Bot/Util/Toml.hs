{-|
Module      : Bot.Util.Toml
Description : Shared TOML parsing helpers
Stability   : experimental
-}

module Bot.Util.Toml
  ( optToken
  , normalizeToken
  )
where

import Bot.Prelude
import Toml.Schema

optToken :: Text -> ParseTable l (Maybe Text)
optToken key = normalizeToken <$> optKey key

normalizeToken :: Maybe Text -> Maybe Text
normalizeToken = \case
  Nothing -> Nothing
  Just "" -> Nothing
  Just t  -> Just t
