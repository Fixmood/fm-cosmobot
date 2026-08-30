{-|
Module      : Bot.Chat.Driver.Matrix.Config
Description : Matrix driver file configuration
Stability   : experimental
-}

module Bot.Chat.Driver.Matrix.Config
  ( FileConfig (..)
  , defaultFileConfig
  , toRuntimeConfig
  )
where

import Bot.Util.Toml
import qualified Bot.Chat.Driver.Matrix as Matrix
import Bot.Prelude
import Toml.Schema

data FileConfig = FileConfig
  { homeserver :: !Text
  , loginUser :: !(Maybe Text)
  , loginPassword :: !(Maybe Text)
  , deviceId :: !(Maybe Text)
  , directRooms :: ![Text]
  , botId :: !(Maybe Text)
  , allowedRooms :: ![Text]
  , superusers :: ![Text]
  }
  deriving (Show)

defaultFileConfig :: FileConfig
defaultFileConfig = FileConfig
  { homeserver = "https://matrix.org"
  , loginUser = Nothing
  , loginPassword = Nothing
  , deviceId = Nothing
  , directRooms = []
  , botId = Nothing
  , allowedRooms = []
  , superusers = []
  }

instance FromValue FileConfig where
  fromValue = parseTableFromValue $ FileConfig
    <$> fmap (fromMaybe defaultFileConfig.homeserver) (optKey "homeserver")
    <*> optToken "login_user"
    <*> optToken "login_password"
    <*> optToken "device_id"
    <*> fmap (fromMaybe []) (optKey "direct_rooms")
    <*> optKey "bot_id"
    <*> fmap (fromMaybe []) (optKey "allowed_rooms")
    <*> fmap (fromMaybe []) (optKey "superusers")

toRuntimeConfig :: FileConfig -> Matrix.Config
toRuntimeConfig cfg =
  Matrix.Config
    { homeserver = cfg.homeserver
    , loginUser = cfg.loginUser
    , loginPassword = cfg.loginPassword
    , deviceId = cfg.deviceId
    , directRooms = cfg.directRooms
    , userId = cfg.botId
    , allowedRooms = cfg.allowedRooms
    , superusers = cfg.superusers
    }
