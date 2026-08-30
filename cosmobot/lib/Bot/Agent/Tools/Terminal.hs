{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Bot.Agent.Tools.Terminal
Description : ACP client terminal tool
Stability   : experimental
-}
module Bot.Agent.Tools.Terminal
  ( terminalTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Agent.Types
import Bot.Core.Message
import qualified Bot.Effect.ACP as ACP
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text

terminalTool :: ACP.ACP :> es => Tool (Eff es)
terminalTool =
  tagged [workTag]
  . allowWhen ((== PlatformACP) . (.platform) . (.message))
  . withDescription "Run or manage a command in the connected ACP client. Actions: create, output, wait_for_exit, kill, release."
  $ tool "terminal"
      (parsedArguments
        (objectSchema
          [ fieldText "action" "One of: create, output, wait_for_exit, kill, release."
          , fieldText "terminal_id" "Terminal id returned by create."
          , fieldText "command" "Command to execute for create."
          , fieldTextArray "args" "Command arguments for create."
          , ("env", envSchema)
          , fieldText "cwd" "Working directory for create. Defaults to the ACP session cwd."
          , fieldInteger "output_byte_limit" "Maximum retained output bytes for create."
          ]
          ["action"])
        parseTerminalArgs)
      \call -> do
        context <- askToolContext
        runTerminalCall context.message call <&> either clientFailure toolText

data TerminalCall
  = TerminalCreate !ACP.TerminalCreate
  | TerminalOutput !Text
  | TerminalWaitForExit !Text
  | TerminalKill !Text
  | TerminalRelease !Text
  deriving (Eq, Show)

runTerminalCall
  :: ACP.ACP :> es
  => IncomingMessage
  -> TerminalCall
  -> Eff es (Either Text Text)
runTerminalCall message = \case
  TerminalCreate create ->
    ACP.createClientTerminal message create
      <&> fmap (\terminalId -> jsonText (Aeson.object ["terminalId" Aeson..= terminalId]))
  TerminalOutput terminalId ->
    ACP.readClientTerminalOutput message terminalId <&> fmap (jsonText . terminalOutputValue)
  TerminalWaitForExit terminalId ->
    ACP.waitForClientTerminalExit message terminalId <&> fmap (jsonText . terminalExitStatusValue)
  TerminalKill terminalId ->
    ACP.killClientTerminal message terminalId <&> fmap (const "Terminal killed.")
  TerminalRelease terminalId ->
    ACP.releaseClientTerminal message terminalId <&> fmap (const "Terminal released.")

parseTerminalArgs :: Aeson.Value -> AesonTypes.Parser TerminalCall
parseTerminalArgs =
  Aeson.withObject "terminal arguments" \o -> do
    action <- o Aeson..: Key.fromText "action"
    case action :: Text of
      "create" -> do
        command <- o Aeson..: Key.fromText "command" >>= validText "command"
        args <- fromMaybe [] <$> o Aeson..:? Key.fromText "args"
        traverse_ (validValue "argument") args
        envValues <- fromMaybe [] <$> o Aeson..:? Key.fromText "env"
        env <- traverse parseEnv envValues
        cwd <- traverse (validText "cwd") =<< nonEmptyText =<< o Aeson..:? Key.fromText "cwd"
        outputByteLimit <- o Aeson..:? Key.fromText "output_byte_limit"
        when (maybe False (<= 0) outputByteLimit) $ fail "output_byte_limit must be positive."
        pure $ TerminalCreate ACP.TerminalCreate{command, args, env, cwd, outputByteLimit}
      "output" -> TerminalOutput <$> requiredTerminalId o
      "wait_for_exit" -> TerminalWaitForExit <$> requiredTerminalId o
      "kill" -> TerminalKill <$> requiredTerminalId o
      "release" -> TerminalRelease <$> requiredTerminalId o
      _ -> fail "action must be one of: create, output, wait_for_exit, kill, release."
  where
    parseEnv = Aeson.withObject "environment variable" \envObject -> do
      name <- envObject Aeson..: Key.fromText "name" >>= validText "environment variable name"
      when (Text.any (== '=') name) $ fail "environment variable name must not contain '='."
      value <- envObject Aeson..: Key.fromText "value" >>= validValue "environment variable value"
      pure (name, value)

validText :: String -> Text -> AesonTypes.Parser Text
validText label value = do
  when (Text.null (Text.strip value)) $ fail (label <> " must not be empty.")
  validValue label value

validValue :: String -> Text -> AesonTypes.Parser Text
validValue label value = do
  when (Text.any (== '\NUL') value) $ fail (label <> " must not contain NUL.")
  pure value

requiredTerminalId :: AesonTypes.Object -> AesonTypes.Parser Text
requiredTerminalId o =
  o Aeson..: Key.fromText "terminal_id" >>= validText "terminal_id"

nonEmptyText :: Maybe Text -> AesonTypes.Parser (Maybe Text)
nonEmptyText = pure . (>>= \text -> text <$ guard (not (Text.null (Text.strip text))))

terminalOutputValue :: ACP.TerminalOutput -> Aeson.Value
terminalOutputValue output = Aeson.object
  [ "output" Aeson..= output.output
  , "truncated" Aeson..= output.truncated
  , "exitStatus" Aeson..= fmap terminalExitStatusValue output.exitStatus
  ]

terminalExitStatusValue :: ACP.TerminalExitStatus -> Aeson.Value
terminalExitStatusValue status = Aeson.object
  [ "exitCode" Aeson..= status.exitCode
  , "signal" Aeson..= status.signal
  ]

clientFailure :: Text -> ToolResult
clientFailure err =
  toolFailure (permanentArgumentFailure err err)

envSchema :: Aeson.Value
envSchema = Aeson.object
  [ "type" Aeson..= ("array" :: Text)
  , "description" Aeson..= ("Environment variables for create." :: Text)
  , "items" Aeson..= Aeson.object
      [ "type" Aeson..= ("object" :: Text)
      , "properties" Aeson..= Aeson.object
          [ "name" Aeson..= Aeson.object ["type" Aeson..= ("string" :: Text)]
          , "value" Aeson..= Aeson.object ["type" Aeson..= ("string" :: Text)]
          ]
      , "required" Aeson..= ["name" :: Text, "value"]
      , "additionalProperties" Aeson..= False
      ]
  ]
