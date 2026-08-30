{-|
Module      : Bot.Agent.Tools.Shell
Description : Agent shell execution tool
Stability   : experimental
-}

module Bot.Agent.Tools.Shell
  ( runBashTool
  , commandTool
  , runBashSafe
  , runSandboxBashSafe
  , runSandboxBashStreaming
  , observeCommand
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Agent.Types
import Bot.Prelude
import qualified Bot.Resource.Sandbox as Sandbox
import qualified Bot.Resource.Command as Command
import qualified Bot.Effect.Resource as Resource
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Util.Process as ProcessUtil
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import Effectful.FileSystem (FileSystem)
import qualified Effectful.Process.Typed as TypedProcess
import Effectful.Timeout

runBashTool :: (Resource.Resource :> es, FileSystem :> es, IOE :> es, Fail :> es, Timeout :> es, Concurrency.Concurrency :> es, Concurrent :> es, TypedProcess.TypedProcess :> es) => Tool (Eff es)
runBashTool =
  tagged [workTag]
  . allowWhen (\context -> superuserOnly context && hasResourceIdentity context)
  . withDescription "Run a bash script and obtain outputs; do not run malicious code. Avoid it except when doing a whole-system-level task."
  $ tool "run_bash"
      ( validateArgument validScript
          (requiredText "script" "The bash script to be executed in the cwd")
      , validateArgument validTimeout
          (withDefault 30 (optionalInt "timeout_seconds" "Maximum seconds to wait before killing the process. Defaults to 30."))
      )
      \script timeoutSeconds -> do
        context <- askToolContext
        metadata <- askToolCallMetadata
        case Resource.accessFromMessage context.message of
          Left err -> pure (resourceToolFailure err)
          Right access -> do
            command <- Command.createAndStart access metadata.resourceOwner Resource.Init
              { message = context.message, arguments = () }
              (\_ command -> Right <$> runBashStreaming timeoutSeconds (Text.unpack script) command)
            case command of
              Left err -> pure (resourceToolFailure err)
              Right commandId -> observeCommand True access metadata.resourceOwner commandId 10 0 0

commandTool :: (Resource.Resource :> es, Concurrency.Concurrency :> es, Timeout :> es, Concurrent :> es) => Tool (Eff es)
commandTool =
  tagged [workTag]
  . allowWhen hasResourceIdentity
  . withDescription "Query, wait for, or cancel an asynchronous command handle."
  $ tool "command"
      (parsedArguments (objectSchema
        [ fieldText "op" "One of: query, wait, cancel."
        , fieldText "command" "Command handle."
        , fieldInteger "timeout_seconds" "Seconds to wait; defaults to 10 for wait."
        , fieldInteger "stdout_offset" "Characters already read from stdout; defaults to 0."
        , fieldInteger "stderr_offset" "Characters already read from stderr; defaults to 0."
        ] ["op", "command"]) parseCommandCall)
      \call -> do
        context <- askToolContext
        metadata <- askToolCallMetadata
        case Resource.accessFromMessage context.message of
          Left err -> pure (resourceToolFailure err)
          Right access -> case call of
            Query commandId stdoutOffset stderrOffset -> observeCommand False access metadata.resourceOwner commandId 0 stdoutOffset stderrOffset
            Wait commandId seconds stdoutOffset stderrOffset -> observeCommand False access metadata.resourceOwner commandId seconds stdoutOffset stderrOffset
            Cancel commandId -> Resource.destroy access commandId <&> either resourceToolFailure (const (toolText "Command cancelled."))

data CommandCall = Query Text Int Int | Wait Text Int Int Int | Cancel Text

parseCommandCall :: Aeson.Value -> AesonTypes.Parser CommandCall
parseCommandCall = Aeson.withObject "command arguments" \o -> do
  op <- o Aeson..: Key.fromText "op"
  commandId <- o Aeson..: Key.fromText "command"
  stdoutOffset <- fromMaybe 0 <$> o Aeson..:? Key.fromText "stdout_offset"
  stderrOffset <- fromMaybe 0 <$> o Aeson..:? Key.fromText "stderr_offset"
  when (stdoutOffset < 0 || stderrOffset < 0) $ fail "offsets must be non-negative."
  when (Text.null (Text.strip commandId)) $ fail "command must not be empty."
  case op :: Text of
    "query" -> pure (Query commandId stdoutOffset stderrOffset)
    "wait" -> do
      seconds <- fromMaybe 10 <$> o Aeson..:? Key.fromText "timeout_seconds"
      when (seconds <= 0) $ fail "timeout_seconds must be positive."
      pure (Wait commandId seconds stdoutOffset stderrOffset)
    "cancel" -> pure (Cancel commandId)
    _ -> fail "op must be one of: query, wait, cancel."

observeCommand
  :: (Resource.Resource :> es, Concurrency.Concurrency :> es, Timeout :> es, Concurrent :> es)
  => Bool -> Resource.ResourceAccess -> Maybe Concurrency.Handle -> Text -> Int -> Int -> Int -> Eff es ToolResult
observeCommand initial access owner commandId seconds stdoutOffset stderrOffset = do
  result <- Resource.withResource @Command.Command access commandId owner \command ->
    if seconds == 0 then Command.queryCommand command else Command.waitCommand seconds command
  case result of
    Left err -> pure (resourceToolFailure err)
    Right (Command.Running stdoutText stderrText) -> pure (toolText (commandSnapshot initial commandId "running" stdoutOffset stderrOffset stdoutText stderrText))
    Right (Command.Finished outcome stdoutText stderrText) -> do
      when (seconds /= 0) $ void (Resource.destroy access commandId)
      pure $ either clientFailure (\output -> if seconds == 0 then toolText (commandSnapshot False commandId "completed" stdoutOffset stderrOffset stdoutText stderrText) else toolText output) outcome

commandSnapshot :: Bool -> Text -> Text -> Int -> Int -> Text -> Text -> Text
commandSnapshot initial commandId status stdoutOffset stderrOffset stdoutText stderrText =
  jsonText (Aeson.object
    ( [ "command" Aeson..= commandId
    , "status" Aeson..= status
    , "stdout" Aeson..= Text.drop stdoutOffset stdoutText
    , "stderr" Aeson..= Text.drop stderrOffset stderrText
    , "next_stdout_offset" Aeson..= Text.length stdoutText
    , "next_stderr_offset" Aeson..= Text.length stderrText
    ] <> ["next_step" Aeson..= ("Use the command tool with op query or wait and the returned offsets." :: Text) | initial]
    ))

hasResourceIdentity :: Context -> Bool
hasResourceIdentity = isRight . Resource.accessFromMessage . (.message)

clientFailure :: Text -> ToolResult
clientFailure err = toolFailure (permanentArgumentFailure err err)

runBashSafe :: (IOE :> es, Fail :> es, Timeout :> es, Concurrent :> es, TypedProcess.TypedProcess :> es) => Int -> String -> Eff es Text
runBashSafe timeoutSeconds script = do
  let effectiveTimeout = max 1 timeoutSeconds
      processConfig =
        TypedProcess.setCreateGroup True .
        TypedProcess.setStdin TypedProcess.closed .
        TypedProcess.setStdout TypedProcess.byteStringOutput .
        TypedProcess.setStderr TypedProcess.byteStringOutput $
        TypedProcess.shell script
  process <- TypedProcess.startProcess processConfig
  let killProcess =
        ProcessUtil.killProcessGroup (TypedProcess.unsafeProcessHandle process)
  outcome <- timeout (effectiveTimeout * 1_000_000) (TypedProcess.waitExitCode process)
    `onException` killProcess
  case outcome of
    Nothing -> do
      killProcess
      _ <- timeout processExitGraceMicroseconds (TypedProcess.waitExitCode process)
      stdoutText <- ProcessUtil.processOutputText (TypedProcess.getStdout process)
      stderrText <- ProcessUtil.processOutputText (TypedProcess.getStderr process)
      pure $ Text.strip $ Text.unlines $ filter (not . Text.null)
        [ "Script timed out after " <> Text.pack (show effectiveTimeout) <> " seconds and was killed."
        , if Text.null stdoutText then "" else "stdout:\n" <> stdoutText
        , if Text.null stderrText then "" else "stderr:\n" <> stderrText
        ]
    Just exitCode -> do
      stdoutText <- ProcessUtil.processOutputText (TypedProcess.getStdout process)
      stderrText <- ProcessUtil.processOutputText (TypedProcess.getStderr process)
      pure (formatBashResult exitCode stdoutText stderrText)

runBashStreaming
  :: (FileSystem :> es, IOE :> es, Timeout :> es, Concurrency.Concurrency :> es, Concurrent :> es, TypedProcess.TypedProcess :> es)
  => Int -> String -> Command.Command -> Eff es Text
runBashStreaming timeoutSeconds script command = do
  let effectiveTimeout = max 1 timeoutSeconds
      processConfig =
        TypedProcess.setCreateGroup True .
        TypedProcess.setStdin TypedProcess.closed .
        TypedProcess.setStdout TypedProcess.createPipe .
        TypedProcess.setStderr TypedProcess.createPipe $
        TypedProcess.shell script
  process <- TypedProcess.startProcess processConfig
  let killProcess = ProcessUtil.killProcessGroup (TypedProcess.unsafeProcessHandle process)
  stdoutWorker <- Concurrency.fork "command stdout" (drainOutput (TypedProcess.getStdout process) (Command.appendStdout command))
  stderrWorker <- Concurrency.fork "command stderr" (drainOutput (TypedProcess.getStderr process) (Command.appendStderr command))
  outcome <- timeout (effectiveTimeout * 1_000_000) (TypedProcess.waitExitCode process) `onException` killProcess
  case outcome of
    Nothing -> killProcess
    Just _ -> pure ()
  traverse_ Concurrency.await [stdoutWorker, stderrWorker]
  status <- Command.queryCommand command
  let (stdoutText, stderrText) = case status of
        Command.Running out err -> (out, err)
        Command.Finished _ out err -> (out, err)
  pure $ case outcome of
    Nothing -> Text.strip $ Text.unlines $ filter (not . Text.null)
      ["Script timed out after " <> show effectiveTimeout <> " seconds and was killed.", if Text.null stdoutText then "" else "stdout:\n" <> stdoutText, if Text.null stderrText then "" else "stderr:\n" <> stderrText]
    Just exitCode -> formatBashResult exitCode stdoutText stderrText

drainOutput :: FileSystem :> es => Handle -> (Text -> Eff es ()) -> Eff es ()
drainOutput outputHandle append = do
  chunk <- FileSystemByteString.hGetSome outputHandle 4096
  unless (ByteString.null chunk) $ do
    append (TextEncoding.decodeUtf8With TextEncodingError.lenientDecode chunk)
    drainOutput outputHandle append

formatBashResult :: Show exitCode => exitCode -> Text -> Text -> Text
formatBashResult exitCode stdoutText stderrText =
  Text.strip $ Text.unlines $ filter (not . Text.null)
    [ if Text.null stdoutText then "" else "stdout:\n" <> stdoutText
    , if Text.null stderrText then "" else "stderr:\n" <> stderrText
    , "exit code: " <> Text.pack (show exitCode)
    ]

processExitGraceMicroseconds :: Int
processExitGraceMicroseconds =
  5 * 1_000_000

runSandboxBashSafe
  :: (IOE :> es, Timeout :> es, Concurrent :> es, TypedProcess.TypedProcess :> es)
  => Int
  -> Sandbox.Sandbox
  -> Text
  -> Maybe Int
  -> Eff es (Either Text Text)
runSandboxBashSafe timeoutSeconds sandbox script outputByteLimit =
  Sandbox.runCommand timeoutSeconds sandbox script outputByteLimit <&> fmap \output ->
    if output.timedOut
      then formatSandboxTimeout (max 1 timeoutSeconds) output
      else formatSandboxResult (fromMaybe 1 output.exitCode) output

runSandboxBashStreaming
  :: (FileSystem :> es, IOE :> es, Timeout :> es, Concurrency.Concurrency :> es, Concurrent :> es, TypedProcess.TypedProcess :> es)
  => Int -> Sandbox.Sandbox -> Text -> Maybe Int -> Command.Command -> Eff es (Either Text Text)
runSandboxBashStreaming timeoutSeconds sandbox script outputByteLimit command = do
  let effectiveTimeout = max 1 timeoutSeconds
      limit = fromMaybe (1024 * 1024) outputByteLimit
      processConfig =
        TypedProcess.setCreateGroup True .
        TypedProcess.setStdin TypedProcess.closed .
        TypedProcess.setStdout TypedProcess.createPipe .
        TypedProcess.setStderr TypedProcess.createPipe $
        TypedProcess.proc "docker" (Sandbox.podmanExecArgs sandbox.containerId effectiveTimeout limit script)
  process <- TypedProcess.startProcess processConfig
  let killProcess = ProcessUtil.killProcessGroup (TypedProcess.unsafeProcessHandle process)
  stdoutWorker <- Concurrency.fork "sandbox command stdout" (drainOutput (TypedProcess.getStdout process) (Command.appendStdout command))
  stderrWorker <- Concurrency.fork "sandbox command stderr" (drainOutput (TypedProcess.getStderr process) (Command.appendStderr command))
  outcome <- timeout ((effectiveTimeout + 10) * 1_000_000) (TypedProcess.waitExitCode process) `onException` killProcess
  case outcome of
    Nothing -> killProcess
    Just _ -> pure ()
  traverse_ Concurrency.await [stdoutWorker, stderrWorker]
  status <- Command.queryCommand command
  let (stdoutText, stderrText) = case status of
        Command.Running out err -> (out, err)
        Command.Finished _ out err -> (out, err)
      render exitCode = Text.strip $ Text.unlines $ filter (not . Text.null)
        [if Text.null stdoutText then "" else "stdout:\n" <> stdoutText, if Text.null stderrText then "" else "stderr:\n" <> stderrText, "exit code: " <> show exitCode]
  pure $ case outcome of
    Nothing -> Left "Docker command did not exit after its timeout."
    Just exitCode -> Right (render exitCode)

formatSandboxTimeout :: Int -> Sandbox.SandboxOutput -> Text
formatSandboxTimeout timeoutSeconds output =
  Text.strip $ Text.unlines $ filter (not . Text.null)
    [ "Script timed out after " <> show timeoutSeconds <> " seconds and was killed."
    , renderSandboxOutput output
    ]

formatSandboxResult :: Int -> Sandbox.SandboxOutput -> Text
formatSandboxResult exitCode output =
  Text.strip $ Text.unlines $ filter (not . Text.null)
    [ renderSandboxOutput output
    , "exit code: " <> show exitCode
    ]

renderSandboxOutput :: Sandbox.SandboxOutput -> Text
renderSandboxOutput output
  | Text.null output.output = ""
  | output.truncated = "output (truncated):\n" <> output.output
  | otherwise = "output:\n" <> output.output

validScript :: Text -> Either Text Text
validScript script
  | Text.any (== '\NUL') script =
      Left "script must not contain NUL."
  | otherwise =
      Right script

validTimeout :: Int -> Either Text Int
validTimeout timeoutSeconds
  | timeoutSeconds <= 0 =
      Left "timeout_seconds must be positive."
  | otherwise =
      Right timeoutSeconds
