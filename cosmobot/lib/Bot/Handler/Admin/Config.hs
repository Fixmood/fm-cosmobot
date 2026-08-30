{-|
Module      : Bot.Handler.Admin.Config
Description : Administrative handler configuration
Stability   : experimental
-}

module Bot.Handler.Admin.Config
  ( AdminConfig (..)
  , UpgradeConfig (..)
  , defaultAdminConfig
  )
where

import Bot.Prelude
import Toml.Schema

-- | Administrative command settings.
newtype AdminConfig = AdminConfig
  { upgrade :: Maybe UpgradeConfig
  }
  deriving (Show)

-- | Configured maintenance script for @!upgrade@.
newtype UpgradeConfig = UpgradeConfig
  { script :: FilePath
  }
  deriving (Show)

defaultAdminConfig :: AdminConfig
defaultAdminConfig = AdminConfig
  { upgrade = Nothing
  }

instance FromValue AdminConfig where
  fromValue = parseTableFromValue $ AdminConfig
    <$> optKey "upgrade"

instance FromValue UpgradeConfig where
  fromValue = parseTableFromValue $ UpgradeConfig
    <$> reqKey "script"
