{-|
Module      : Bot.Handler.Saucenao.Config
Description : SauceNAO handler file configuration
Stability   : experimental
-}

module Bot.Handler.Saucenao.Config
  ( SaucenaoConfig (..)
  , defaultSaucenaoConfig
  )
where

import Bot.Util.Toml
import Bot.Prelude
import Toml.Schema

-- | SauceNAO integration settings.
newtype SaucenaoConfig = SaucenaoConfig
  { apiKey :: Maybe Text
  }
  deriving (Show)

defaultSaucenaoConfig :: SaucenaoConfig
defaultSaucenaoConfig = SaucenaoConfig
  { apiKey = Nothing
  }

instance FromValue SaucenaoConfig where
  fromValue = parseTableFromValue $ SaucenaoConfig
    <$> optToken "api_key"
