{-|
Module      : Bot.LLM.OpenAI.Config
Description : LLM file configuration
Stability   : experimental
-}

module Bot.LLM.OpenAI.Config
  ( Config (..)
  , ChatProviderConfig (..)
  , ImageProviderConfig (..)
  , AudioProviderConfig (..)
  , defaultConfig
  , defaultChatProviderConfig
  , defaultImageProviderConfig
  , defaultAudioProviderConfig
  , FileConfig (..)
  , toRuntimeConfig
  )
where

import Bot.Util.Toml
import Bot.Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Toml.Schema

-- | Runtime configuration for OpenAI-compatible LLM endpoints.
data Config = Config
  { chatProvider :: !(Maybe ChatProviderConfig)
  , chatProviderName :: !(Maybe Text)
  , chatProviders :: !(Map Text ChatProviderConfig)
  , imageProvider :: !(Maybe ImageProviderConfig)
  , imageFallbackProvider :: !(Maybe ImageProviderConfig)
  , audioProvider :: !(Maybe AudioProviderConfig)
  }
  deriving (Eq, Show)

data ChatProviderConfig = ChatProviderConfig
  { baseUrl :: !Text
  , apiKey :: !(Maybe Text)
  , model :: !Text
  , reasoningEffort :: !Text
  , requestTimeout :: !Int
  }
  deriving (Eq, Show)

data ImageProviderConfig = ImageProviderConfig
  { baseUrl :: !Text
  , apiKey :: !(Maybe Text)
  , model :: !Text
  , canGenerate :: !Bool
  , canEdit :: !Bool
  , requestTimeout :: !Int
  , outputFormat :: !(Maybe Text)
  , quality :: !(Maybe Text)
  , size :: !(Maybe Text)
  , aspectRatio :: !(Maybe Text)
  , background :: !(Maybe Text)
  , moderation :: !(Maybe Text)
  }
  deriving (Eq, Show)

data AudioProviderConfig = AudioProviderConfig
  { baseUrl :: !Text
  , apiKey :: !(Maybe Text)
  , model :: !Text
  , voice :: !Text
  , responseFormat :: !Text
  , requestTimeout :: !Int
  , speed :: !(Maybe Double)
  , instructions :: !(Maybe Text)
  }
  deriving (Eq, Show)

-- | Defaults for optional LLM features.
defaultConfig :: Config
defaultConfig = Config
  { chatProvider = Nothing
  , chatProviderName = Nothing
  , chatProviders = Map.empty
  , imageProvider = Nothing
  , imageFallbackProvider = Nothing
  , audioProvider = Nothing
  }

defaultChatProviderConfig :: ChatProviderConfig
defaultChatProviderConfig = ChatProviderConfig
  { baseUrl = "https://openrouter.ai/api/v1"
  , apiKey = Nothing
  , model = "openai/gpt-4o-mini"
  , reasoningEffort = "low"
  , requestTimeout = 60
  }

defaultImageProviderConfig :: ImageProviderConfig
defaultImageProviderConfig = ImageProviderConfig
  { baseUrl = "https://api.openai.com/v1"
  , apiKey = Nothing
  , model = "gpt-image-1.5"
  , canGenerate = True
  , canEdit = False
  , requestTimeout = 300
  , outputFormat = Nothing
  , quality = Nothing
  , size = Nothing
  , aspectRatio = Nothing
  , background = Nothing
  , moderation = Nothing
  }

defaultAudioProviderConfig :: AudioProviderConfig
defaultAudioProviderConfig = AudioProviderConfig
  { baseUrl = "https://api.openai.com/v1"
  , apiKey = Nothing
  , model = "gpt-4o-mini-tts"
  , voice = "coral"
  , responseFormat = "mp3"
  , requestTimeout = 300
  , speed = Nothing
  , instructions = Nothing
  }

data FileConfig = FileConfig
  { chatProvider :: !(Maybe ChatProviderFileConfig)
  , chatProviderName :: !(Maybe Text)
  , chatProviders :: !(Map Text ChatProviderFileConfig)
  , imageProvider :: !(Maybe ImageProviderFileConfig)
  , imageFallbackProvider :: !(Maybe ImageProviderFileConfig)
  , audioProvider :: !(Maybe AudioProviderFileConfig)
  }
  deriving (Show)

data ChatProviderFileConfig = ChatProviderFileConfig
  { baseUrl :: !Text
  , apiKey :: !(Maybe Text)
  , model :: !Text
  , reasoningEffort :: !Text
  , requestTimeout :: !Int
  }
  deriving (Show)

data ImageProviderFileConfig = ImageProviderFileConfig
  { baseUrl :: !Text
  , apiKey :: !(Maybe Text)
  , model :: !Text
  , canGenerate :: !Bool
  , canEdit :: !Bool
  , requestTimeout :: !Int
  , outputFormat :: !(Maybe Text)
  , quality :: !(Maybe Text)
  , size :: !(Maybe Text)
  , aspectRatio :: !(Maybe Text)
  , background :: !(Maybe Text)
  , moderation :: !(Maybe Text)
  }
  deriving (Show)

data AudioProviderFileConfig = AudioProviderFileConfig
  { baseUrl :: !Text
  , apiKey :: !(Maybe Text)
  , model :: !Text
  , voice :: !Text
  , responseFormat :: !Text
  , requestTimeout :: !Int
  , speed :: !(Maybe Double)
  , instructions :: !(Maybe Text)
  }
  deriving (Show)

instance FromValue FileConfig where
  fromValue = parseTableFromValue do
    selectedChat <- optKey "chat"
    selectedImage <- optKey "image"
    selectedImageFallback <- optKey "image_fallback"
    selectedAudio <- optKey "audio"
    chatProviders <- fmap (fromMaybe Map.empty) (optKey "chat_provider")
    imageProviders <- fmap (fromMaybe Map.empty) (optKey "image_provider")
    audioProviders <- fmap (fromMaybe Map.empty) (optKey "audio_provider")
    chatProvider <- selectedProvider "llm.chat" "llm.chat_provider" selectedChat chatProviders
    imageProvider <- selectedProvider "llm.image" "llm.image_provider" selectedImage imageProviders
    imageFallbackProvider <- selectedProvider "llm.image_fallback" "llm.image_provider" selectedImageFallback imageProviders
    audioProvider <- selectedProvider "llm.audio" "llm.audio_provider" selectedAudio audioProviders
    pure FileConfig
      { chatProvider = chatProvider
      , chatProviderName = nonBlank selectedChat
      , chatProviders = chatProviders
      , imageProvider = imageProvider
      , imageFallbackProvider = imageFallbackProvider
      , audioProvider = audioProvider
      }

nonBlank :: Maybe Text -> Maybe Text
nonBlank selected =
  selected >>= \value -> case Text.strip value of
    "" -> Nothing
    stripped -> Just stripped

selectedProvider
  :: Text
  -> Text
  -> Maybe Text
  -> Map Text provider
  -> ParseTable l (Maybe provider)
selectedProvider selectorName tableName selected providers =
  case Text.strip <$> selected of
    Nothing ->
      pure Nothing
    Just "" ->
      pure Nothing
    Just name ->
      case Map.lookup name providers of
        Just provider ->
          pure (Just provider)
        Nothing ->
          fail [i|#{selectorName} selects #{name}, but #{tableName}.#{name} is not defined|]

instance FromValue ChatProviderFileConfig where
  fromValue = parseTableFromValue do
    baseUrl <- fmap (fromMaybe defaultChatProviderConfig.baseUrl) (optKey "base_url")
    apiKey <- optToken "api_key"
    model <- fmap (fromMaybe defaultChatProviderConfig.model) (optKey "model")
    reasoningEffort <- fmap (fromMaybe defaultChatProviderConfig.reasoningEffort) (optKey "reasoning_effort")
    requestTimeout <- fmap (fromMaybe defaultChatProviderConfig.requestTimeout) (optKey "timeout")
    when (requestTimeout <= 0) (fail "llm.chat_provider.<name>.timeout must be positive")
    pure ChatProviderFileConfig
      { baseUrl = baseUrl
      , apiKey = apiKey
      , model = model
      , reasoningEffort = reasoningEffort
      , requestTimeout = requestTimeout
      }

instance FromValue ImageProviderFileConfig where
  fromValue = parseTableFromValue do
    baseUrl <- fmap (fromMaybe defaultImageProviderConfig.baseUrl) (optKey "base_url")
    apiKey <- optToken "api_key"
    model <- fmap (fromMaybe defaultImageProviderConfig.model) (optKey "model")
    canGenerate <- fmap (fromMaybe defaultImageProviderConfig.canGenerate) (optKey "can_generate")
    canEdit <- fmap (fromMaybe defaultImageProviderConfig.canEdit) (optKey "can_edit")
    requestTimeout <- fmap (fromMaybe defaultImageProviderConfig.requestTimeout) (optKey "timeout")
    outputFormat <- optKey "output_format"
    quality <- optKey "quality"
    size <- optKey "size"
    aspectRatio <- optKey "aspect_ratio"
    background <- optKey "background"
    moderation <- optKey "moderation"
    when (requestTimeout <= 0) (fail "llm.image_provider.<name>.timeout must be positive")
    pure ImageProviderFileConfig
      { baseUrl = baseUrl
      , apiKey = apiKey
      , model = model
      , canGenerate = canGenerate
      , canEdit = canEdit
      , requestTimeout = requestTimeout
      , outputFormat = outputFormat
      , quality = quality
      , size = size
      , aspectRatio = aspectRatio
      , background = background
      , moderation = moderation
      }

instance FromValue AudioProviderFileConfig where
  fromValue = parseTableFromValue do
    baseUrl <- fmap (fromMaybe defaultAudioProviderConfig.baseUrl) (optKey "base_url")
    apiKey <- optToken "api_key"
    model <- fmap (fromMaybe defaultAudioProviderConfig.model) (optKey "model")
    voice <- fmap (fromMaybe defaultAudioProviderConfig.voice) (optKey "voice")
    responseFormat <- fmap (fromMaybe defaultAudioProviderConfig.responseFormat) (optKey "response_format")
    requestTimeout <- fmap (fromMaybe defaultAudioProviderConfig.requestTimeout) (optKey "timeout")
    speed <- optKey "speed"
    instructions <- optKey "instructions"
    when (requestTimeout <= 0) (fail "llm.audio_provider.<name>.timeout must be positive")
    traverse_ (\value -> when (value <= 0) (fail "llm.audio_provider.<name>.speed must be positive")) speed
    pure AudioProviderFileConfig
      { baseUrl = baseUrl
      , apiKey = apiKey
      , model = model
      , voice = voice
      , responseFormat = responseFormat
      , requestTimeout = requestTimeout
      , speed = speed
      , instructions = instructions
      }

toRuntimeConfig :: FileConfig -> Config
toRuntimeConfig cfg =
  Config
    { chatProvider = toRuntimeChatProviderConfig <$> cfg.chatProvider
    , chatProviderName = cfg.chatProviderName
    , chatProviders = toRuntimeChatProviderConfig <$> cfg.chatProviders
    , imageProvider = toRuntimeImageProviderConfig <$> cfg.imageProvider
    , imageFallbackProvider = toRuntimeImageProviderConfig <$> cfg.imageFallbackProvider
    , audioProvider = toRuntimeAudioProviderConfig <$> cfg.audioProvider
    }

toRuntimeChatProviderConfig :: ChatProviderFileConfig -> ChatProviderConfig
toRuntimeChatProviderConfig cfg =
  ChatProviderConfig
    { baseUrl = cfg.baseUrl
    , apiKey = cfg.apiKey
    , model = cfg.model
    , reasoningEffort = cfg.reasoningEffort
    , requestTimeout = cfg.requestTimeout
    }

toRuntimeImageProviderConfig :: ImageProviderFileConfig -> ImageProviderConfig
toRuntimeImageProviderConfig cfg =
  ImageProviderConfig
    { baseUrl = cfg.baseUrl
    , apiKey = cfg.apiKey
    , model = cfg.model
    , canGenerate = cfg.canGenerate
    , canEdit = cfg.canEdit
    , requestTimeout = cfg.requestTimeout
    , outputFormat = cfg.outputFormat
    , quality = cfg.quality
    , size = cfg.size
    , aspectRatio = cfg.aspectRatio
    , background = cfg.background
    , moderation = cfg.moderation
    }

toRuntimeAudioProviderConfig :: AudioProviderFileConfig -> AudioProviderConfig
toRuntimeAudioProviderConfig cfg =
  AudioProviderConfig
    { baseUrl = cfg.baseUrl
    , apiKey = cfg.apiKey
    , model = cfg.model
    , voice = cfg.voice
    , responseFormat = cfg.responseFormat
    , requestTimeout = cfg.requestTimeout
    , speed = cfg.speed
    , instructions = cfg.instructions
    }
