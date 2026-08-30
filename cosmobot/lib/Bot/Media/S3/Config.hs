{-|
Module      : Bot.Media.S3.Config
Description : S3 media storage configuration
Stability   : experimental
-}

module Bot.Media.S3.Config
  ( Config (..)
  , defaultConfig
  )
where

import Bot.Prelude
import Toml.Schema

data Config = Config
  { enabled :: !Bool
  , bucket :: !(Maybe Text)
  , region :: !Text
  , endpoint :: !(Maybe Text)
  , accessKeyId :: !(Maybe Text)
  , secretAccessKey :: !(Maybe Text)
  , prefix :: !Text
  , publicReadAcl :: !Bool
  , addressingStyle :: !Text
  }
  deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config
  { enabled = False
  , bucket = Nothing
  , region = "us-east-1"
  , endpoint = Nothing
  , accessKeyId = Nothing
  , secretAccessKey = Nothing
  , prefix = "cosmobot/media"
  , publicReadAcl = False
  , addressingStyle = "auto"
  }

instance FromValue Config where
  fromValue = parseTableFromValue do
    enabled <- fromMaybe defaultConfig.enabled <$> optKey "enabled"
    bucket <- optKey "bucket"
    region <- fromMaybe defaultConfig.region <$> optKey "region"
    endpoint <- optKey "endpoint"
    accessKeyId <- optKey "access_key_id"
    secretAccessKey <- optKey "secret_access_key"
    prefix <- fromMaybe defaultConfig.prefix <$> optKey "prefix"
    publicReadAcl <- fromMaybe defaultConfig.publicReadAcl <$> optKey "public_read_acl"
    addressingStyle <- fromMaybe defaultConfig.addressingStyle <$> optKey "addressing_style"
    pure Config{enabled, bucket, region, endpoint, accessKeyId, secretAccessKey, prefix, publicReadAcl, addressingStyle}
