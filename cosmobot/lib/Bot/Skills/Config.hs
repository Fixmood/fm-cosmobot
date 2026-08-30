{-|
Module      : Bot.Skills.Config
Description : Skills file configuration
Stability   : experimental
-}

module Bot.Skills.Config
  ( FileConfig (..)
  , defaultFileConfig
  , toSkillsConfig
  )
where

import Bot.Prelude
import qualified Bot.Skills as Skills
import Toml.Schema

newtype FileConfig = FileConfig
  { dir :: FilePath
  }
  deriving (Show)

defaultFileConfig :: FileConfig
defaultFileConfig = FileConfig
  { dir = "skills"
  }

instance FromValue FileConfig where
  fromValue = parseTableFromValue do
    dir <- fromMaybe defaultFileConfig.dir <$> optKey "dir"
    pure FileConfig{dir}

toSkillsConfig :: FileConfig -> Skills.SkillsConfig
toSkillsConfig cfg =
  Skills.SkillsConfig
    { dir = cfg.dir
    }
