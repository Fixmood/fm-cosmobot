module Main (main) where

import qualified Bot.Config as Config
import qualified Bot.Agent.Types as Agent
import qualified Bot.ACP.Config as ACPConfig
import Bot.Chat.Driver.Telegram (Config (..))
import Bot.Core.Message (ChatPlatform (..))
import qualified Bot.RPC.Config as RPCConfig
import Bot.Prelude
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "config"
      [ testCase "drivers table may be omitted" testDriversTableMayBeOmitted
      , testCase "configured telegram driver is enabled alone" testConfiguredTelegramDriverEnabledAlone
      , testCase "incomplete matrix and discord driver tables are disabled" testIncompleteMatrixAndDiscordDisabled
      , testCase "ask context compaction threshold uses ktokens" testAskCompactionThresholdUsesKTokens
      , testCase "sandbox image is configurable" testSandboxImage
      , testCase "Exa web search is configurable" testExaWebSearch
      , testCase "Python tool defaults are safe and enabled" testPythonDefaults
      , testCase "Python tool limits are configurable" testPythonConfig
      , testCase "Python tool bounds must be positive" testPythonPositiveBounds
      , testCase "Python tool converted bounds reject overflow" testPythonOverflow
      , testCase "Python wall timeout is at most one hour" testPythonWallTimeoutBound
      , testCase "Python tool rejects unknown keys" testPythonUnknownKey
      ]

testDriversTableMayBeOmitted :: IO ()
testDriversTableMayBeOmitted = do
  cfg <- loadConfigText minimalConfig
  assertBool "expected QQ driver to be disabled" (isNothing cfg.qq)
  assertBool "expected Telegram driver to be disabled" (isNothing cfg.telegram)
  assertBool "expected Matrix driver to be disabled" (isNothing cfg.matrix)
  assertBool "expected Discord driver to be disabled" (isNothing cfg.discord)
  let RPCConfig.Config{enabled = rpcEnabled} = cfg.rpc
  rpcEnabled @?= False
  let ACPConfig.Config{enabled = acpEnabled} = cfg.acp
  acpEnabled @?= False
  let Agent.ToolConfig{sandboxImage} = cfg.tool
  sandboxImage @?= "fm-cosmobox:latest"

testConfiguredTelegramDriverEnabledAlone :: IO ()
testConfiguredTelegramDriverEnabledAlone = do
  cfg <- loadConfigText $
    minimalConfig
      <> Text.unlines
        [ ""
        , "[driver.telegram]"
        , "bot_token = \"telegram-token\""
        , "bot_id = \"cosmobot\""
        ]
  assertBool "expected QQ driver to be disabled" (isNothing cfg.qq)
  case cfg.telegram of
    Just telegram ->
      telegram.botToken @?= "telegram-token"
    Nothing ->
      assertFailure "expected Telegram driver config"
  assertBool "expected Matrix driver to be disabled" (isNothing cfg.matrix)
  assertBool "expected Discord driver to be disabled" (isNothing cfg.discord)
  cfg.handlers.ask.botIds @?= [(PlatformTelegram, "cosmobot")]

testIncompleteMatrixAndDiscordDisabled :: IO ()
testIncompleteMatrixAndDiscordDisabled = do
  cfg <- loadConfigText $
    minimalConfig
      <> Text.unlines
        [ ""
        , "[driver.matrix]"
        , "homeserver = \"https://matrix.example.test\""
        , "bot_id = \"@bot:matrix.example.test\""
        , ""
        , "[driver.discord]"
        , "bot_id = 424242"
        ]
  assertBool "expected Matrix driver to be disabled" (isNothing cfg.matrix)
  assertBool "expected Discord driver to be disabled" (isNothing cfg.discord)
  cfg.handlers.ask.botIds @?=
    [ (PlatformMatrix, "@bot:matrix.example.test")
    , (PlatformDiscord, "424242")
    ]

testAskCompactionThresholdUsesKTokens :: IO ()
testAskCompactionThresholdUsesKTokens = do
  cfg <- loadConfigText $
    minimalConfig
      <> Text.unlines
        [ "context_compaction_threshold_ktokens = 123"
        ]
  cfg.handlers.ask.contextCompactionThresholdKTokens @?= 123

testSandboxImage :: IO ()
testSandboxImage = do
  cfg <- loadConfigText $
    minimalConfig
      <> Text.unlines
        [ ""
        , "[resource.sandbox]"
        , "image = \"registry.example.test/custom:latest\""
        ]
  let Agent.ToolConfig{sandboxImage} = cfg.tool
  sandboxImage @?= "registry.example.test/custom:latest"

testExaWebSearch :: IO ()
testExaWebSearch = do
  cfg <- loadConfigText $
    minimalConfig
      <> Text.unlines
        [ ""
        , "[tool.web_search]"
        , "api = \"exa\""
        , "exa_api_key = \"exa-test-key\""
        ]
  let Agent.ToolConfig{webSearchApi, exaApiKey} = cfg.tool
  webSearchApi @?= Agent.WebSearchExa
  exaApiKey @?= Just "exa-test-key"

testPythonDefaults :: IO ()
testPythonDefaults = do
  cfg <- loadConfigText minimalConfig
  cfg.tool.python @?= Agent.defaultPythonConfig

testPythonConfig :: IO ()
testPythonConfig = do
  cfg <- loadConfigText $
    minimalConfig
      <> Text.unlines
        [ ""
        , "[tool.python]"
        , "enable = false"
        , "wall_timeout_seconds = 45"
        , "cpu_seconds = 25"
        , "memory_mib = 768"
        , "max_tool_calls = 80"
        ]
  cfg.tool.python @?= Agent.PythonConfig
    { enabled = False
    , wallTimeoutSeconds = 45
    , cpuSeconds = 25
    , memoryMiB = 768
    , maxToolCalls = 80
    }

testPythonPositiveBounds :: IO ()
testPythonPositiveBounds =
  for_ ["wall_timeout_seconds", "cpu_seconds", "memory_mib", "max_tool_calls"] \key ->
    assertConfigFailureContains [i|tool.python.#{key} must be positive|] $
      pythonConfig [key <> " = 0"]

testPythonOverflow :: IO ()
testPythonOverflow =
  assertConfigFailureContains "tool.python.memory_mib is too large" $
    pythonConfig [[i|memory_mib = #{maxBound `div` (1024 * 1024) + 1 :: Int}|]]

testPythonWallTimeoutBound :: IO ()
testPythonWallTimeoutBound = do
  cfg <- loadConfigText (pythonConfig ["wall_timeout_seconds = 3600"])
  cfg.tool.python.wallTimeoutSeconds @?= 3600
  assertConfigFailureContains "tool.python.wall_timeout_seconds must not exceed 3600" $
    pythonConfig ["wall_timeout_seconds = 3601"]

testPythonUnknownKey :: IO ()
testPythonUnknownKey =
  assertConfigFailureContains "unknown tool.python keys: timeout" $
    pythonConfig ["timeout = 30"]

pythonConfig :: [Text] -> Text
pythonConfig fields =
  minimalConfig <> Text.unlines ("" : "[tool.python]" : fields)

assertConfigFailureContains :: Text -> Text -> IO ()
assertConfigFailureContains expected source = do
  outcome <- runEff $ trySync (liftIO (loadConfigText source))
  case outcome of
    Left err ->
      assertBool
        [i|expected config error containing #{expected}, got #{displayException err}|]
        (expected `Text.isInfixOf` Text.pack (displayException err))
    Right cfg ->
      assertFailure [i|expected config failure, got #{show cfg :: String}|]

minimalConfig :: Text
minimalConfig =
  Text.unlines
    [ "[llm]"
    , ""
    , "[handler.ask]"
    , "command = \"!ask\""
    , "system_prompt = \"You are cosmobot.\""
    ]

loadConfigText :: Text -> IO Config.BotConfig
loadConfigText source =
  withSystemTempDirectory "cosmobot-config-spec-" \dir -> do
    let path = dir <> "/config.toml"
    TextIO.writeFile path source
    runEff . runFailIO $ Config.loadConfig path
