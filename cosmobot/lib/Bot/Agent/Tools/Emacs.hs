{-|
Module      : Bot.Agent.Tools.Emacs
Description : Agent Emacs evaluation tool
Stability   : experimental
-}

module Bot.Agent.Tools.Emacs
  ( emacsEvalTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Agent.Types
import Bot.Prelude
import qualified Data.Text as Text
import System.Exit (ExitCode (..))
import System.IO.Error (userError)
import Effectful.Process (Process, StdStream (..), proc, readCreateProcessWithExitCode, waitForProcess, createProcess, std_err, std_out)
import Effectful.Timeout
import Effectful.FileSystem.IO (FileSystem, hClose)

emacsSocketName :: String
emacsSocketName =
  "cosmobot"

emacsEvalTool :: (Process :> es, Timeout :> es, Concurrent :> es, IOE :> es, FileSystem :> es) => Tool (Eff es)
emacsEvalTool =
  tagged [workTag]
  . allowWhen superuserOnly
  . withDescription "Evaluate Emacs Lisp in a cosmobot-owned, persistent Emacs 30 daemon for coding, scripting, reading/writing files, starting subprocesses, managing terminals, recording temporary memory in buffers, etc. Prefer it to other tools if it uses less tokens, and always use it if there are multiple operations that can be batched."
  $ tool "emacs_eval"
      ( validateArgument validExpression
          (requiredText "expression" "Emacs Lisp expression to evaluate.")
      , validateArgument validTimeout
          (withDefault 10 (optionalInt "timeout_seconds" "Maximum seconds to wait before returning a timeout. Defaults to 10."))
      )
      \expression timeoutSeconds -> do
        result <- runEmacsEval timeoutSeconds expression
        pure (toolText result)

runEmacsEval :: (Process :> es, Timeout :> es, FileSystem :> es) => Int -> Text -> Eff es Text
runEmacsEval timeoutSeconds expression = do
  firstAttempt <- tryEval timeoutSeconds expression
  case firstAttempt of
    Right result ->
      pure result
    Left _ -> do
      startEmacsDaemon timeoutSeconds
      either throwIO pure =<< tryEval timeoutSeconds expression

tryEval :: (Timeout :> es, Process :> es) => Int -> Text -> Eff es (Either SomeException Text)
tryEval timeoutSeconds expression =
  try do
    let effectiveTimeout = max 1 timeoutSeconds
        process = proc "emacsclient" ["--socket-name", emacsSocketName, "--eval", Text.unpack expression]
    outcome <- timeout (effectiveTimeout * 1_000_000) (readCreateProcessWithExitCode process "")
    case outcome of
      Nothing ->
        throwIO (userError [i|emacs_eval timed out after #{effectiveTimeout} seconds.|])
      Just (exitCode, stdoutText, stderrText) ->
        pure (formatProcessResult "emacsclient" exitCode (Text.pack stdoutText) (Text.pack stderrText))

startEmacsDaemon :: (Process :> es, Timeout :> es, FileSystem :> es) => Int -> Eff es ()
startEmacsDaemon timeoutSeconds = do
  let effectiveTimeout = max 1 timeoutSeconds
  (_, outHandle, errHandle, processHandle) <- createProcess
    (proc "emacs" ["-Q", "--daemon=" <> emacsSocketName])
      { std_out = CreatePipe
      , std_err = CreatePipe
      }
  outcome <- timeout (effectiveTimeout * 1_000_000) (waitForProcess processHandle)
  traverse_ hClose outHandle
  traverse_ hClose errHandle
  case outcome of
    Nothing ->
      throwIO (userError [i|emacs daemon startup timed out after #{effectiveTimeout} seconds.|])
    Just ExitSuccess ->
      pure ()
    Just exitCode ->
      throwIO (userError [i|emacs daemon startup failed: #{show exitCode :: String}|])

formatProcessResult :: Text -> ExitCode -> Text -> Text -> Text
formatProcessResult commandName exitCode stdoutText stderrText =
  Text.strip $ Text.unlines $ filter (not . Text.null)
    [ if Text.null stdoutText then "" else "stdout:\n" <> stdoutText
    , if Text.null stderrText then "" else "stderr:\n" <> stderrText
    , [i|exit code: #{show exitCode :: String}|]
    , [i|command: #{commandName}|]
    ]

validExpression :: Text -> Either Text Text
validExpression rawExpression
  | Text.null expression =
      Left "expression must not be empty."
  | otherwise =
      Right expression
  where
    expression = Text.strip rawExpression

validTimeout :: Int -> Either Text Int
validTimeout timeoutSeconds
  | timeoutSeconds <= 0 =
      Left "timeout_seconds must be positive."
  | otherwise =
      Right (min 60 timeoutSeconds)
