{-|
Module      : Bot.Chat.Driver.QQ.Config
Description : QQ driver file configuration
Stability   : experimental
-}

module Bot.Chat.Driver.QQ.Config
  ( FileConfig (..)
  , toRuntimeConfig
  )
where

import qualified Bot.Chat.Driver.QQ as QQ
import Bot.Util.Toml
import Bot.Prelude
import Toml.Schema

data FileConfig = FileConfig
  { host  :: !String
  , port  :: !Int
  , path  :: !String
  , token :: !(Maybe Text)
  , botId :: !(Maybe Integer)
  , allowedGroups :: ![Integer]
  , allowedUsers :: ![Integer]
  , allowAllGroups :: !Bool
  , allowAllPrivate :: !Bool
  , blockedGroups :: ![Integer]
  , blockedUsers :: ![Integer]
  , superusers :: ![Integer]
  }
  deriving (Show)

instance FromValue FileConfig where
  fromValue = parseTableFromValue $ FileConfig
    <$> reqKey "host"
    <*> reqKey "port"
    <*> reqKey "path"
    <*> optToken "token"
    <*> optKey "bot_id"
    <*> fmap (fromMaybe []) (optKey "allowed_groups")
    <*> fmap (fromMaybe []) (optKey "allowed_users")
    <*> fmap (fromMaybe True) (optKey "allow_all_groups")
    <*> fmap (fromMaybe True) (optKey "allow_all_private")
    <*> fmap (fromMaybe []) (optKey "blocked_groups")
    <*> fmap (fromMaybe []) (optKey "blocked_users")
    <*> fmap (fromMaybe []) (optKey "superusers")

toRuntimeConfig :: FileConfig -> QQ.Config
toRuntimeConfig cfg =
  QQ.Config
    { host = cfg.host
    , port = cfg.port
    , path = cfg.path
    , token = cfg.token
    , botQQ = cfg.botId
    , allowedGroups = cfg.allowedGroups
    , allowedUsers = cfg.allowedUsers
    , allowAllGroups = cfg.allowAllGroups
    , allowAllPrivate = cfg.allowAllPrivate
    , blockedGroups = cfg.blockedGroups
    , blockedUsers = cfg.blockedUsers
    , superusers = cfg.superusers
    }
