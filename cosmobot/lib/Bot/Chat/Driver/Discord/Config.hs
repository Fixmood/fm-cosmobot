{-|
Module      : Bot.Chat.Driver.Discord.Config
Description : Discord driver file configuration
Stability   : experimental
-}

module Bot.Chat.Driver.Discord.Config
  ( FileConfig (..)
  , defaultFileConfig
  , toRuntimeConfig
  )
where

import qualified Bot.Chat.Driver.Discord as Discord
import Bot.Prelude
import qualified Data.Text as Text
import qualified Toml.Semantics.Types as TomlValue
import Toml.Schema

data FileConfig = FileConfig
  { botToken :: !Text
  , botId :: !(Maybe Text)
  , applicationId :: !(Maybe Text)
  , allowedGuilds :: ![Integer]
  , allowedChannels :: ![Integer]
  , allowedUsers :: ![Text]
  , superusers :: ![Text]
  , gatewayHost :: !String
  , gatewayPath :: !String
  }
  deriving (Show)

defaultFileConfig :: FileConfig
defaultFileConfig = FileConfig
  { botToken = ""
  , botId = Nothing
  , applicationId = Nothing
  , allowedGuilds = []
  , allowedChannels = []
  , allowedUsers = []
  , superusers = []
  , gatewayHost = "gateway.discord.gg"
  , gatewayPath = "/?v=10&encoding=json"
  }

instance FromValue FileConfig where
  fromValue = parseTableFromValue $ FileConfig
    <$> fmap (fromMaybe defaultFileConfig.botToken) (optKey "bot_token")
    <*> optSnowflakeText "bot_id"
    <*> optSnowflakeText "application_id"
    <*> fmap (fromMaybe []) (optKey "allowed_guilds")
    <*> fmap (fromMaybe []) (optKey "allowed_channels")
    <*> fmap (maybe [] (map discordSnowflakeText)) (optKey "allowed_users")
    <*> fmap (maybe [] (map discordSnowflakeText)) (optKey "superusers")
    <*> fmap (fromMaybe defaultFileConfig.gatewayHost) (optKey "gateway_host")
    <*> fmap (fromMaybe defaultFileConfig.gatewayPath) (optKey "gateway_path")

toRuntimeConfig :: FileConfig -> Discord.Config
toRuntimeConfig cfg =
  Discord.Config
    { botToken = cfg.botToken
    , botId = cfg.botId
    , applicationId = cfg.applicationId
    , allowedGuilds = cfg.allowedGuilds
    , allowedChannels = cfg.allowedChannels
    , allowedUsers = cfg.allowedUsers
    , superusers = cfg.superusers
    , gatewayHost = cfg.gatewayHost
    , gatewayPath = cfg.gatewayPath
    }

optSnowflakeText :: Text -> ParseTable l (Maybe Text)
optSnowflakeText key =
  fmap discordSnowflakeText <$> optKey key

newtype DiscordSnowflake = DiscordSnowflake Text

discordSnowflakeText :: DiscordSnowflake -> Text
discordSnowflakeText (DiscordSnowflake value) =
  value

instance FromValue DiscordSnowflake where
  fromValue = \case
    TomlValue.Text' _ value ->
      pure (DiscordSnowflake (Text.strip value))
    TomlValue.Integer' _ value ->
      pure (DiscordSnowflake (show value))
    _ ->
      fail "expected Discord snowflake string or integer"
