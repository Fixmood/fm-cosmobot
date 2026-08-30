{-|
Module      : Bot.Agent.Config
Description : Agent tool file configuration
Stability   : experimental
-}

module Bot.Agent.Config
  ( FileConfig (..)
  , PythonFileConfig (..)
  , WebFetchFileConfig (..)
  , WebSearchFileConfig (..)
  , defaultFileConfig
  , toToolConfig
  )
where

import qualified Bot.Agent.Types as Agent
import Bot.Util.Toml
import Bot.Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Toml.Semantics.Types as TomlValue
import Toml.Schema

data FileConfig = FileConfig
  { webSearch :: !WebSearchFileConfig
  , webFetch :: !WebFetchFileConfig
  , datetime :: !Bool
  , python :: !PythonFileConfig
  }
  deriving (Show)

data PythonFileConfig = PythonFileConfig
  { enable :: !Bool
  , wallTimeoutSeconds :: !Int
  , cpuSeconds :: !Int
  , memoryMiB :: !Int
  , maxToolCalls :: !Int
  }
  deriving (Show)

data WebFetchFileConfig = WebFetchFileConfig
  { enable :: !Bool
  , maxUses :: !(Maybe Int)
  , maxContentTokens :: !(Maybe Int)
  }
  deriving (Show)

data WebSearchFileConfig = WebSearchFileConfig
  { enable :: !Bool
  , api :: !Agent.WebSearchApi
  , maxResults :: !(Maybe Int)
  , braveApiKey :: !(Maybe Text)
  , tavilyApiKey :: !(Maybe Text)
  , exaApiKey :: !(Maybe Text)
  }
  deriving (Show)

defaultFileConfig :: FileConfig
defaultFileConfig = FileConfig
  { webSearch = defaultWebSearchFileConfig
  , webFetch = defaultWebFetchFileConfig
  , datetime = Agent.defaultToolConfig.datetime
  , python = defaultPythonFileConfig
  }

defaultPythonFileConfig :: PythonFileConfig
defaultPythonFileConfig = PythonFileConfig
  { enable = Agent.defaultPythonConfig.enabled
  , wallTimeoutSeconds = Agent.defaultPythonConfig.wallTimeoutSeconds
  , cpuSeconds = Agent.defaultPythonConfig.cpuSeconds
  , memoryMiB = Agent.defaultPythonConfig.memoryMiB
  , maxToolCalls = Agent.defaultPythonConfig.maxToolCalls
  }

defaultWebFetchFileConfig :: WebFetchFileConfig
defaultWebFetchFileConfig = WebFetchFileConfig
  { enable = Agent.defaultToolConfig.webFetch
  , maxUses = Agent.defaultToolConfig.webFetchMaxUses
  , maxContentTokens = Agent.defaultToolConfig.webFetchMaxContentTokens
  }

defaultWebSearchFileConfig :: WebSearchFileConfig
defaultWebSearchFileConfig = WebSearchFileConfig
  { enable = Agent.defaultToolConfig.webSearchEnable
  , api = Agent.defaultToolConfig.webSearchApi
  , maxResults = Agent.defaultToolConfig.webSearchMaxResults
  , braveApiKey = Agent.defaultToolConfig.braveApiKey
  , tavilyApiKey = Agent.defaultToolConfig.tavilyApiKey
  , exaApiKey = Agent.defaultToolConfig.exaApiKey
  }

instance FromValue FileConfig where
  fromValue = parseTableFromValue do
    webSearch <- fromMaybe defaultFileConfig.webSearch <$> optKey "web_search"
    webFetch <- fromMaybe defaultFileConfig.webFetch <$> optKey "web_fetch"
    datetime <- fromMaybe defaultFileConfig.datetime <$> optKey "datetime"
    python <- fromMaybe defaultFileConfig.python <$> optKey "python"
    pure FileConfig
      { webSearch = webSearch
      , webFetch = webFetch
      , datetime = datetime
      , python = python
      }

instance FromValue PythonFileConfig where
  fromValue = parseTableFromValue do
    enable <- fromMaybe defaultPythonFileConfig.enable <$> optKey "enable"
    wallTimeoutSeconds <- fromMaybe defaultPythonFileConfig.wallTimeoutSeconds <$> optKey "wall_timeout_seconds"
    cpuSeconds <- fromMaybe defaultPythonFileConfig.cpuSeconds <$> optKey "cpu_seconds"
    memoryMiB <- fromMaybe defaultPythonFileConfig.memoryMiB <$> optKey "memory_mib"
    maxToolCalls <- fromMaybe defaultPythonFileConfig.maxToolCalls <$> optKey "max_tool_calls"
    rejectUnknownPythonKeys
    requirePositive "wall_timeout_seconds" wallTimeoutSeconds
    requirePositive "cpu_seconds" cpuSeconds
    requirePositive "memory_mib" memoryMiB
    requirePositive "max_tool_calls" maxToolCalls
    when (wallTimeoutSeconds > Agent.maxPythonWallTimeoutSeconds) $
      fail [i|tool.python.wall_timeout_seconds must not exceed #{Agent.maxPythonWallTimeoutSeconds}|]
    requireSafeProduct "memory_mib" (1024 * 1024) memoryMiB
    pure PythonFileConfig{enable, wallTimeoutSeconds, cpuSeconds, memoryMiB, maxToolCalls}

rejectUnknownPythonKeys :: ParseTable l ()
rejectUnknownPythonKeys = do
  TomlValue.MkTable remaining <- getTable
  unless (Map.null remaining) $
    fail [i|unknown tool.python keys: #{Text.intercalate ", " (Map.keys remaining)}|]

requirePositive :: Text -> Int -> ParseTable l ()
requirePositive name value =
  unless (value > 0) $
    fail [i|tool.python.#{name} must be positive|]

requireSafeProduct :: Text -> Int -> Int -> ParseTable l ()
requireSafeProduct name multiplier value =
  when (value > maxBound `div` multiplier) $
    fail [i|tool.python.#{name} is too large|]

instance FromValue WebFetchFileConfig where
  fromValue = parseTableFromValue do
    enable <- fromMaybe defaultWebFetchFileConfig.enable <$> optKey "enable"
    maxUses <- optKey "max_uses"
    maxContentTokens <- optKey "max_content_tokens"
    pure WebFetchFileConfig
      { enable = enable
      , maxUses = maxUses
      , maxContentTokens = maxContentTokens
      }

instance FromValue WebSearchFileConfig where
  fromValue = parseTableFromValue do
    enable <- fromMaybe defaultWebSearchFileConfig.enable <$> optKey "enable"
    api <- maybe (pure defaultWebSearchFileConfig.api) parseWebSearchApi =<< optKey "api"
    maxResults <- optKey "max_results"
    braveApiKey <- optToken "brave_api_key"
    tavilyApiKey <- optToken "tavily_api_key"
    exaApiKey <- optToken "exa_api_key"
    pure WebSearchFileConfig
      { enable = enable
      , api = api
      , maxResults = maxResults
      , braveApiKey = braveApiKey
      , tavilyApiKey = tavilyApiKey
      , exaApiKey = exaApiKey
      }

parseWebSearchApi :: Text -> ParseTable l Agent.WebSearchApi
parseWebSearchApi value =
  case Text.toLower (Text.strip value) of
    "tavily" -> pure Agent.WebSearchTavily
    "brave"  -> pure Agent.WebSearchBrave
    "exa"    -> pure Agent.WebSearchExa
    _        -> fail "tool.web_search.api must be one of: tavily, brave, exa"

toToolConfig :: FileConfig -> Agent.ToolConfig
toToolConfig cfg =
  Agent.ToolConfig
    { webSearchEnable = cfg.webSearch.enable
    , webSearchApi = cfg.webSearch.api
    , webSearchMaxResults = cfg.webSearch.maxResults
    , braveApiKey = cfg.webSearch.braveApiKey
    , tavilyApiKey = cfg.webSearch.tavilyApiKey
    , exaApiKey = cfg.webSearch.exaApiKey
    , webFetch = cfg.webFetch.enable
    , webFetchMaxUses = cfg.webFetch.maxUses
    , webFetchMaxContentTokens = cfg.webFetch.maxContentTokens
    , datetime = cfg.datetime
    , python = Agent.PythonConfig
        { enabled = cfg.python.enable
        , wallTimeoutSeconds = cfg.python.wallTimeoutSeconds
        , cpuSeconds = cfg.python.cpuSeconds
        , memoryMiB = cfg.python.memoryMiB
        , maxToolCalls = cfg.python.maxToolCalls
        }
    , sandboxImage = Agent.defaultToolConfig.sandboxImage
    }
