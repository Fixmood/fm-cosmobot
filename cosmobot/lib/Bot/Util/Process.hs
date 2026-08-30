{-|
Module      : Bot.Util.Process
Description : Process output helpers
Stability   : experimental
-}

module Bot.Util.Process
  ( processOutputText
  , killProcessGroup
  , readProcessGroupWithExitCode
  )
where

import Bot.Prelude
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.STM as STM
import qualified Effectful.Process as Process
import qualified Effectful.Process.Typed as TypedProcess
import System.Exit (ExitCode)
import System.Posix.Signals (signalProcess, signalProcessGroup, sigKILL)

processOutputText :: Concurrent :> es => STM.STM LazyByteString.ByteString -> Eff es Text
processOutputText =
  fmap (TextEncoding.decodeUtf8Lenient . LazyByteString.toStrict) . STM.atomically

killProcessGroup :: (IOE :> es, Process.Process :> es) => Process.ProcessHandle -> Eff es ()
killProcessGroup processHandle = do
  mPid <- Process.getPid processHandle
  traverse_ killPid mPid
  where
    killPid pid =
      ignoreIO $
        (liftIO $ signalProcessGroup sigKILL (fromIntegral pid))
          `catchSync` \_ ->
            liftIO $ signalProcess sigKILL pid

ignoreIO :: IOE :> es => Eff es () -> Eff es ()
ignoreIO action =
  action `catchSync` \_ -> pure ()

readProcessGroupWithExitCode
  :: (IOE :> es, Concurrent :> es, TypedProcess.TypedProcess :> es)
  => FilePath
  -> [String]
  -> Eff es (ExitCode, Text, Text)
readProcessGroupWithExitCode executable args = do
  let processConfig =
        TypedProcess.setCreateGroup True .
        TypedProcess.setStdin TypedProcess.closed .
        TypedProcess.setStdout TypedProcess.byteStringOutput .
        TypedProcess.setStderr TypedProcess.byteStringOutput $
        TypedProcess.proc executable args
  process <- TypedProcess.startProcess processConfig
  let killProcess =
        killProcessGroup (TypedProcess.unsafeProcessHandle process)
  code <- TypedProcess.waitExitCode process `onException` killProcess
  stdoutText <- processOutputText (TypedProcess.getStdout process)
  stderrText <- processOutputText (TypedProcess.getStderr process)
  pure (code, stdoutText, stderrText)
