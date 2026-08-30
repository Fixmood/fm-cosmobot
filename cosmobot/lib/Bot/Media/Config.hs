{-|
Module      : Bot.Media.Config
Description : Media cache and public mirror configuration
Stability   : experimental
-}

module Bot.Media.Config
  ( Config (..)
  , GcConfig (..)
  , defaultConfig
  , defaultGcConfig
  )
where

import Bot.Prelude
import qualified Bot.Media.S3.Config as S3
import qualified Bot.Util.Image as Image
import Toml.Schema

data Config = Config
  { cacheDir :: !FilePath
  , publicBaseUrl :: !(Maybe Text)
  , compression :: !Image.ImageCompressionConfig
  , gc :: !GcConfig
  , s3 :: !S3.Config
  }
  deriving (Show, Eq)

data GcConfig = GcConfig
  { enabled :: !Bool
  , olderThanDays :: !Int
  , intervalHours :: !Int
  }
  deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config
  { cacheDir = "media-cache"
  , publicBaseUrl = Nothing
  , compression = Image.ImageCompressionConfig{compressionFormat = Nothing, compressionLevel = Nothing}
  , gc = defaultGcConfig
  , s3 = S3.defaultConfig
  }

defaultGcConfig :: GcConfig
defaultGcConfig = GcConfig
  { enabled = False
  , olderThanDays = 7
  , intervalHours = 24
  }

instance FromValue Config where
  fromValue = parseTableFromValue do
    cacheDir <- fromMaybe defaultConfig.cacheDir <$> optKey "cache_dir"
    publicBaseUrl <- optKey "public_base_url"
    compressionFormat <- optKey "compression_format"
    compressionLevel <- optKey "compression_level"
    gc <- fromMaybe defaultConfig.gc <$> optKey "gc"
    s3 <- fromMaybe defaultConfig.s3 <$> optKey "s3"
    let compression = Image.ImageCompressionConfig{compressionFormat, compressionLevel}
    pure Config{cacheDir, publicBaseUrl, compression, gc, s3}

instance FromValue GcConfig where
  fromValue = parseTableFromValue do
    enabled <- fromMaybe defaultGcConfig.enabled <$> optKey "enabled"
    olderThanDays <- fromMaybe defaultGcConfig.olderThanDays <$> optKey "older_than_days"
    intervalHours <- fromMaybe defaultGcConfig.intervalHours <$> optKey "interval_hours"
    pure GcConfig{enabled, olderThanDays, intervalHours}
