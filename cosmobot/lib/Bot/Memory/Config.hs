{-|
Module      : Bot.Memory.Config
Description : Memory file configuration
Stability   : experimental
-}

module Bot.Memory.Config
  ( FileConfig (..)
  , defaultFileConfig
  , toMemoryConfig
  )
where

import qualified Bot.Memory as Memory
import Bot.Prelude
import Toml.Schema

newtype FileConfig = FileConfig
  { dir :: FilePath
  }
  deriving (Show)

defaultFileConfig :: FileConfig
defaultFileConfig = FileConfig
  { dir = "memory"
  }

instance FromValue FileConfig where
  fromValue = parseTableFromValue do
    dir <- fromMaybe defaultFileConfig.dir <$> optKey "dir"
    pure FileConfig{dir}

toMemoryConfig :: FileConfig -> Memory.MemoryConfig
toMemoryConfig cfg =
  Memory.MemoryConfig
    { dir = cfg.dir
    }
