module Main (main) where

import qualified Bot.LLM.OpenAI.Config as LLMConfig
import qualified Bot.LLM.OpenAI as OpenAI
import Bot.Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Toml
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "llm-openai-config"
      [ testCase "selects named chat provider" testSelectsNamedChatProvider
      , testCase "retains all chat providers for runtime switching" testRetainsAllChatProviders
      , testCase "resolves model switch targets by profile or model id" testResolvesModelSwitchTargets
      , testCase "selects named image provider" testSelectsNamedImageProvider
      , testCase "selects named audio provider" testSelectsNamedAudioProvider
      , testCase "blank selectors disable providers" testBlankSelectorsDisableProviders
      , testCase "missing selected provider fails clearly" testMissingSelectedProviderFailsClearly
      , testCase "provider defaults are applied" testProviderDefaultsAreApplied
      , testCase "non-positive chat timeout fails" testNonPositiveChatTimeoutFails
      , testCase "non-positive image timeout fails" testNonPositiveImageTimeoutFails
      , testCase "non-positive audio timeout fails" testNonPositiveAudioTimeoutFails
      ]

testSelectsNamedChatProvider :: IO ()
testSelectsNamedChatProvider = do
  cfg <- parseRuntimeConfig $
    Text.unlines
      [ "chat = \"openrouter\""
      , ""
      , "[chat_provider.openrouter]"
      , "base_url = \"https://openrouter.ai/api/v1\""
      , "api_key = \"chat-key\""
      , "model = \"openai/gpt-5.4-mini\""
      , "reasoning_effort = \"medium\""
      , "timeout = 42"
      ]
  case cfg.chatProvider of
    Just provider -> do
      provider.baseUrl @?= "https://openrouter.ai/api/v1"
      provider.apiKey @?= Just "chat-key"
      provider.model @?= "openai/gpt-5.4-mini"
      provider.reasoningEffort @?= "medium"
      provider.requestTimeout @?= 42
    Nothing ->
      assertFailure "expected selected chat provider"
  cfg.imageProvider @?= Nothing
  cfg.audioProvider @?= Nothing

testRetainsAllChatProviders :: IO ()
testRetainsAllChatProviders = do
  cfg <- twoChatProviderConfig
  cfg.chatProviderName @?= Just "deepseek"
  Map.keys cfg.chatProviders @?= ["deepseek", "gpt"]
  (.model) <$> Map.lookup "deepseek" cfg.chatProviders @?= Just "deepseek-chat"
  (.model) <$> Map.lookup "gpt" cfg.chatProviders @?= Just "gpt-sol-5.6"

testResolvesModelSwitchTargets :: IO ()
testResolvesModelSwitchTargets = do
  cfg <- twoChatProviderConfig
  OpenAI.resolveChatModelTarget cfg "GPT" @?= Right (Just "gpt")
  OpenAI.resolveChatModelTarget cfg "gpt-sol-5.6" @?= Right (Just "gpt")
  assertBool "unknown models should be rejected" (isLeft (OpenAI.resolveChatModelTarget cfg "missing"))

twoChatProviderConfig :: IO LLMConfig.Config
twoChatProviderConfig =
  parseRuntimeConfig $ Text.unlines
    [ "chat = \"deepseek\""
    , ""
    , "[chat_provider.deepseek]"
    , "api_key = \"deepseek-key\""
    , "model = \"deepseek-chat\""
    , ""
    , "[chat_provider.gpt]"
    , "api_key = \"gpt-key\""
    , "model = \"gpt-sol-5.6\""
    ]

testSelectsNamedImageProvider :: IO ()
testSelectsNamedImageProvider = do
  cfg <- parseRuntimeConfig $
    Text.unlines
      [ "image = \"openai\""
      , ""
      , "[image_provider.openai]"
      , "base_url = \"https://api.openai.com/v1\""
      , "api_key = \"image-key\""
      , "model = \"gpt-image-1.5\""
      , "can_generate = false"
      , "can_edit = true"
      , "timeout = 180"
      , "quality = \"high\""
      , "size = \"1024x1536\""
      , "aspect_ratio = \"portrait\""
      , "background = \"transparent\""
      , "moderation = \"low\""
      , "output_format = \"webp\""
      ]
  cfg.chatProvider @?= Nothing
  case cfg.imageProvider of
    Just provider -> do
      provider.baseUrl @?= "https://api.openai.com/v1"
      provider.apiKey @?= Just "image-key"
      provider.model @?= "gpt-image-1.5"
      provider.canGenerate @?= False
      provider.canEdit @?= True
      provider.requestTimeout @?= 180
      provider.quality @?= Just "high"
      provider.size @?= Just "1024x1536"
      provider.aspectRatio @?= Just "portrait"
      provider.background @?= Just "transparent"
      provider.moderation @?= Just "low"
      provider.outputFormat @?= Just "webp"
    Nothing ->
      assertFailure "expected selected image provider"
  cfg.audioProvider @?= Nothing

testSelectsNamedAudioProvider :: IO ()
testSelectsNamedAudioProvider = do
  cfg <- parseRuntimeConfig $
    Text.unlines
      [ "audio = \"openai\""
      , ""
      , "[audio_provider.openai]"
      , "base_url = \"https://api.openai.com/v1\""
      , "api_key = \"audio-key\""
      , "model = \"gpt-4o-mini-tts\""
      , "timeout = 120"
      , "voice = \"verse\""
      , "response_format = \"opus\""
      , "speed = 1.25"
      , "instructions = \"Speak warmly.\""
      ]
  cfg.chatProvider @?= Nothing
  cfg.imageProvider @?= Nothing
  case cfg.audioProvider of
    Just provider -> do
      provider.baseUrl @?= "https://api.openai.com/v1"
      provider.apiKey @?= Just "audio-key"
      provider.model @?= "gpt-4o-mini-tts"
      provider.voice @?= "verse"
      provider.responseFormat @?= "opus"
      provider.requestTimeout @?= 120
      provider.speed @?= Just 1.25
      provider.instructions @?= Just "Speak warmly."
    Nothing ->
      assertFailure "expected selected audio provider"

testBlankSelectorsDisableProviders :: IO ()
testBlankSelectorsDisableProviders = do
  cfg <- parseRuntimeConfig $
    Text.unlines
      [ "chat = \"  \""
      , "image = \"\""
      , "audio = \" \""
      , ""
      , "[chat_provider.openrouter]"
      , "api_key = \"chat-key\""
      , ""
      , "[image_provider.openai]"
      , "api_key = \"image-key\""
      , ""
      , "[audio_provider.openai]"
      , "api_key = \"audio-key\""
      ]
  cfg.chatProvider @?= Nothing
  cfg.imageProvider @?= Nothing
  cfg.audioProvider @?= Nothing

testMissingSelectedProviderFailsClearly :: IO ()
testMissingSelectedProviderFailsClearly = do
  case parseRuntimeConfigEither "chat = \"missing\"" of
    Left err ->
      assertBool
        [i|unexpected error: #{err}|]
        ("llm.chat selects missing, but llm.chat_provider.missing is not defined" `Text.isInfixOf` Text.pack err)
    Right cfg ->
      assertFailure [i|expected parse failure, got #{show cfg :: String}|]
  case parseRuntimeConfigEither "audio = \"missing\"" of
    Left err ->
      assertBool
        [i|unexpected error: #{err}|]
        ("llm.audio selects missing, but llm.audio_provider.missing is not defined" `Text.isInfixOf` Text.pack err)
    Right cfg ->
      assertFailure [i|expected parse failure, got #{show cfg :: String}|]

testProviderDefaultsAreApplied :: IO ()
testProviderDefaultsAreApplied = do
  cfg <- parseRuntimeConfig $
    Text.unlines
      [ "chat = \"default_chat\""
      , "image = \"default_image\""
      , "audio = \"default_audio\""
      , ""
      , "[chat_provider.default_chat]"
      , ""
      , "[image_provider.default_image]"
      , ""
      , "[audio_provider.default_audio]"
      ]
  cfg.chatProvider @?= Just LLMConfig.defaultChatProviderConfig
  cfg.imageProvider @?= Just LLMConfig.defaultImageProviderConfig
  cfg.imageFallbackProvider @?= Nothing
  cfg.audioProvider @?= Just LLMConfig.defaultAudioProviderConfig

testNonPositiveChatTimeoutFails :: IO ()
testNonPositiveChatTimeoutFails =
  assertParseFailureContains "llm.chat_provider.<name>.timeout must be positive" $
    Text.unlines
      [ "chat = \"bad\""
      , "[chat_provider.bad]"
      , "timeout = 0"
      ]

testNonPositiveImageTimeoutFails :: IO ()
testNonPositiveImageTimeoutFails =
  assertParseFailureContains "llm.image_provider.<name>.timeout must be positive" $
    Text.unlines
      [ "image = \"bad\""
      , "[image_provider.bad]"
      , "timeout = 0"
      ]

testNonPositiveAudioTimeoutFails :: IO ()
testNonPositiveAudioTimeoutFails =
  assertParseFailureContains "llm.audio_provider.<name>.timeout must be positive" $
    Text.unlines
      [ "audio = \"bad\""
      , "[audio_provider.bad]"
      , "timeout = 0"
      ]

parseRuntimeConfig :: Text -> IO LLMConfig.Config
parseRuntimeConfig source =
  either assertFailure pure (parseRuntimeConfigEither source)

parseRuntimeConfigEither :: Text -> Either String LLMConfig.Config
parseRuntimeConfigEither source =
  case Toml.decode source of
    Toml.Failure errors ->
      Left (Text.unpack (Text.unlines (map toText errors)))
    Toml.Success _warnings fileConfig ->
      Right (LLMConfig.toRuntimeConfig fileConfig)

assertParseFailureContains :: Text -> Text -> IO ()
assertParseFailureContains expected source =
  case parseRuntimeConfigEither source of
    Left err ->
      assertBool [i|expected #{expected} in #{err}|] (expected `Text.isInfixOf` Text.pack err)
    Right cfg ->
      assertFailure [i|expected parse failure, got #{show cfg :: String}|]
