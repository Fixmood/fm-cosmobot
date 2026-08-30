{-# LANGUAGE OverloadedRecordDot #-}

module Bot.Resource.Python.Sandbox
  ( Config
  , GatedSandbox
  , RunningSandbox
  , prepare
  , launchGated
  , start
  , stdinHandle
  , stdoutHandle
  , terminalOutcome
  , stopGated
  , stopRunning
  )
where

import Bot.Prelude
import qualified Bot.Util.Process as ProcessUtil
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Effectful.Concurrent.STM as STM
import qualified Effectful.FileSystem as FileSystem
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem.IO as FileSystemIO
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified Effectful.Process as Process
import qualified Effectful.Process.Typed as TypedProcess
import Effectful.Timeout (Timeout, timeout)
import System.Exit (ExitCode (..))
import qualified System.Posix.IO as Posix
import System.Posix.Signals (signalProcess, signalProcessGroup, sigKILL)
import System.Posix.Types (Fd (..))

data Config = Config
  { bwrapPath :: !FilePath
  , prlimitPath :: !FilePath
  , workerPath :: !FilePath
  }

newtype SandboxError = SandboxError Text
  deriving stock (Show)
  deriving anyclass (Exception)

type SandboxProcess =
  TypedProcess.Process Handle Handle (STM.STM LazyByteString.ByteString)

data GatedSandbox = GatedSandbox
  { process :: !SandboxProcess
  , gate :: !Handle
  , childPid :: !Int
  , prlimitPath :: !FilePath
  }

data RunningSandbox = RunningSandbox
  { process :: SandboxProcess
  , childPid :: Int
  }

stdinHandle :: RunningSandbox -> Handle
stdinHandle = TypedProcess.getStdin . (.process)

stdoutHandle :: RunningSandbox -> Handle
stdoutHandle = TypedProcess.getStdout . (.process)

terminalOutcome
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es)
  => RunningSandbox
  -> Eff es (ExitCode, Text)
terminalOutcome running = do
  exitCode <- TypedProcess.waitExitCode running.process
  stderrText <- ProcessUtil.processOutputText (TypedProcess.getStderr running.process)
  pure (exitCode, Text.take (64 * 1024) stderrText)

prepare
  :: ( Concurrent :> es
     , FileSystem :> es
     , TypedProcess.TypedProcess :> es
     , Timeout :> es
     , IOE :> es
     )
  => FilePath
  -> Eff es (Either Text Config)
prepare workerPath = trySync (discover workerPath) <&> first (Text.pack . displayException)

discover
  :: ( Concurrent :> es
     , FileSystem :> es
     , TypedProcess.TypedProcess :> es
     , Timeout :> es
     , IOE :> es
     )
  => FilePath
  -> Eff es Config
discover workerPath = do
  traverse_ requireFile requiredFiles
  requirePythonVersion
  checkBubblewrap
  checkPrlimit
  pure Config
    { bwrapPath = "/usr/bin/bwrap"
    , prlimitPath = "/usr/bin/prlimit"
    , workerPath
    }
  where
    requiredFiles =
      [ "/usr/bin/python3"
      , "/usr/bin/bwrap"
      , "/usr/bin/prlimit"
      , "/usr"
      , "/etc/resolv.conf"
      , "/etc/hosts"
      , "/etc/nsswitch.conf"
      , "/etc/ssl/certs"
      , "/dev/null"
      , workerPath
      ]

    requireFile path = do
      exists <- FileSystem.doesPathExist path
      unless exists (throwSandbox ("missing Python sandbox prerequisite: " <> Text.pack path))

requirePythonVersion
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, Timeout :> es, IOE :> es)
  => Eff es ()
requirePythonVersion = do
  (exitCode, stdoutText, stderrText) <-
    healthCheck "Python version" "/usr/bin/python3" ["--version"]
  unless (exitCode == ExitSuccess) $
    throwSandbox ("Python version check failed: " <> Text.take 500 stderrText)
  let versionText = fromMaybe (Text.strip stderrText) (Text.stripPrefix "Python " (Text.strip stdoutText))
      components = Text.splitOn "." versionText
      parsed = case components of
        major : minor : _ -> (,) <$> readMaybe (Text.unpack major) <*> readMaybe (Text.unpack minor)
        _ -> Nothing
  case parsed of
    Just version | version >= (3 :: Int, 10 :: Int) -> pure ()
    _ -> throwSandbox "py requires Python 3.10 or newer"

checkBubblewrap
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, Timeout :> es, IOE :> es)
  => Eff es ()
checkBubblewrap = do
  let args =
        [ "--unshare-all", "--share-net", "--unshare-user"
        , "--die-with-parent", "--new-session", "--disable-userns"
        , "--cap-drop", "ALL", "--dir", "/usr", "--ro-bind", "/usr", "/usr"
        , "--symlink", "usr/lib", "/lib", "--symlink", "usr/lib64", "/lib64"
        , "--", "/usr/bin/python3", "-I", "-S", "-B", "-c", "pass"
        ]
  (exitCode, _stdoutText, stderrText) <-
    healthCheck "bubblewrap" "/usr/bin/bwrap" args
  unless (exitCode == ExitSuccess) $
    throwSandbox ("bubblewrap health check failed: " <> Text.take 500 stderrText)

checkPrlimit
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, Timeout :> es, IOE :> es)
  => Eff es ()
checkPrlimit = do
  (exitCode, _stdoutText, stderrText) <-
    healthCheck "prlimit" "/usr/bin/prlimit" ["--version"]
  unless (exitCode == ExitSuccess) $
    throwSandbox ("prlimit health check failed: " <> Text.take 500 stderrText)

healthCheck
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, Timeout :> es, IOE :> es)
  => Text
  -> FilePath
  -> [String]
  -> Eff es (ExitCode, Text, Text)
healthCheck label executable arguments =
  timeout (5 * 1_000_000) (readSandboxProcess executable arguments) >>= \case
    Nothing -> throwSandbox (label <> " health check timed out")
    Just outcome -> pure outcome

launchGated
  :: ( TypedProcess.TypedProcess :> es
     , FileSystem :> es
     , Timeout :> es
     , IOE :> es
     )
  => Config
  -> Eff es GatedSandbox
launchGated config = mask \restore -> do
  (gateReadRaw, gateWriteRaw) <- Process.createPipeFd
  (infoReadRaw, infoWriteRaw) <- Process.createPipeFd
    `onException` traverse_ closeFdQuietly [Fd gateReadRaw, Fd gateWriteRaw]
  let gateRead = Fd gateReadRaw
      gateWriteFd = Fd gateWriteRaw
      infoReadFd = Fd infoReadRaw
      infoWrite = Fd infoWriteRaw
      closeInitial = traverse_ closeFdQuietly [gateRead, gateWriteFd, infoReadFd, infoWrite]
  (liftIO $ do
    Posix.setFdOption gateRead Posix.CloseOnExec False
    Posix.setFdOption infoWrite Posix.CloseOnExec False
    Posix.setFdOption gateWriteFd Posix.CloseOnExec True
    Posix.setFdOption infoReadFd Posix.CloseOnExec True)
    `onException` closeInitial
  gateWrite <- liftIO (Posix.fdToHandle gateWriteFd) `onException` closeInitial
  infoRead <- liftIO (Posix.fdToHandle infoReadFd)
    `onException` (closeQuietly gateWrite >> traverse_ closeFdQuietly [gateRead, infoReadFd, infoWrite])
  process <- restore (TypedProcess.startProcess (processConfig config gateRead infoWrite))
    `onException` (closeFdQuietly gateRead >> closeFdQuietly infoWrite >> traverse_ closeQuietly [gateWrite, infoRead])
  closeFdQuietly gateRead
  closeFdQuietly infoWrite
  childPid <- restore (readChildPid infoRead)
    `onException` cleanupStarted process gateWrite infoRead
  closeQuietly infoRead
  pure GatedSandbox{process, gate = gateWrite, childPid, prlimitPath = config.prlimitPath}

start
  :: ( Concurrent :> es
     , TypedProcess.TypedProcess :> es
     , FileSystem :> es
     , Timeout :> es
     , IOE :> es
     )
  => Int
  -> Int
  -> GatedSandbox
  -> Eff es RunningSandbox
start cpuSeconds memoryBytes gated = do
  let limits =
        [ "--pid", show gated.childPid
        , [i|--cpu=#{cpuSeconds}:#{cpuSeconds}|]
        , [i|--as=#{memoryBytes}:#{memoryBytes}|]
        , "--nproc=2:2"
        , "--nofile=64:64"
        ]
  (exitCode, _stdoutText, stderrText) <-
    readSandboxProcess gated.prlimitPath limits
  unless (exitCode == ExitSuccess) $
    throwSandbox ("prlimit failed: " <> Text.take 500 stderrText)
  FileSystemByteString.hPut gated.gate "x"
  FileSystemIO.hFlush gated.gate
  FileSystemIO.hClose gated.gate
  pure RunningSandbox{process = gated.process, childPid = gated.childPid}

readSandboxProcess
  :: ( Concurrent :> es
     , TypedProcess.TypedProcess :> es
     , Timeout :> es
     , IOE :> es
     )
  => FilePath
  -> [String]
  -> Eff es (ExitCode, Text, Text)
readSandboxProcess executable arguments = do
  let config =
        TypedProcess.setCreateGroup True .
        TypedProcess.setStdin TypedProcess.closed .
        TypedProcess.setStdout TypedProcess.byteStringOutput .
        TypedProcess.setStderr TypedProcess.byteStringOutput $
        TypedProcess.proc executable arguments
  mask \restore -> do
    process <- TypedProcess.startProcess config
    let waitAndRead = do
          exitCode <- TypedProcess.waitExitCode process
          stdoutText <- ProcessUtil.processOutputText (TypedProcess.getStdout process)
          stderrText <- ProcessUtil.processOutputText (TypedProcess.getStderr process)
          pure (exitCode, stdoutText, stderrText)
        stopAndReap = do
          ProcessUtil.killProcessGroup (TypedProcess.unsafeProcessHandle process)
          void (timeout (5 * 1_000_000) waitAndRead)
    restore waitAndRead `onException` stopAndReap

stopGated
  :: ( TypedProcess.TypedProcess :> es
     , FileSystem :> es
     , IOE :> es
     )
  => GatedSandbox
  -> Eff es ()
stopGated gated = do
  stopProcess gated.childPid gated.process
    `finally` (closeQuietly gated.gate `finally` closeProcessHandles gated.process)

stopRunning
  :: ( TypedProcess.TypedProcess :> es
     , FileSystem :> es
     , IOE :> es
     )
  => RunningSandbox
  -> Eff es ()
stopRunning running = do
  stopProcess running.childPid running.process
    `finally` closeProcessHandles running.process

stopProcess
  :: ( TypedProcess.TypedProcess :> es
     , IOE :> es
     )
  => Int
  -> SandboxProcess
  -> Eff es ()
stopProcess childPid process = do
  killSandboxGroup childPid
  ProcessUtil.killProcessGroup (TypedProcess.unsafeProcessHandle process)
  void (TypedProcess.waitExitCode process)

cleanupStarted
  :: ( TypedProcess.TypedProcess :> es
     , FileSystem :> es
     , Timeout :> es
     , IOE :> es
     )
  => SandboxProcess
  -> Handle
  -> Handle
  -> Eff es ()
cleanupStarted process gate info = do
  ProcessUtil.killProcessGroup (TypedProcess.unsafeProcessHandle process)
  void (timeout (5 * 1_000_000) (TypedProcess.waitExitCode process))
    `finally` (traverse_ closeQuietly [gate, info] >> closeProcessHandles process)

closeProcessHandles
  :: (FileSystem :> es, IOE :> es)
  => SandboxProcess
  -> Eff es ()
closeProcessHandles process =
  traverse_ closeQuietly
    [ TypedProcess.getStdin process
    , TypedProcess.getStdout process
    ]

killSandboxGroup :: IOE :> es => Int -> Eff es ()
killSandboxGroup childPid =
  (liftIO $ signalProcessGroup sigKILL (fromIntegral childPid))
    `catchSync` \_ ->
      (liftIO $ signalProcess sigKILL (fromIntegral childPid)) `catchSync` \_ -> pure ()

readChildPid :: (FileSystem :> es, IOE :> es) => Handle -> Eff es Int
readChildPid info = do
  payload <- readInfo 0 []
  case Aeson.eitherDecodeStrict' payload of
    Left err -> throwSandbox ("invalid bwrap info: " <> Text.pack err)
    Right (Aeson.Object object) -> case AesonTypes.parseMaybe (Aeson..: "child-pid") object of
      Just pid | pid > 0 -> pure pid
      _ -> throwSandbox "bwrap info omitted child-pid"
    Right _ -> throwSandbox "bwrap info was not an object"
  where
    readInfo size chunks = do
      chunk <- FileSystemByteString.hGetSome info 1024
      if ByteString.null chunk
        then pure (ByteString.concat (reverse chunks))
        else
          if size + ByteString.length chunk > 4096
            then throwSandbox "bwrap info exceeds 4 KiB"
            else readInfo (size + ByteString.length chunk) (chunk : chunks)

processConfig :: Config -> Fd -> Fd -> TypedProcess.ProcessConfig Handle Handle (STM.STM LazyByteString.ByteString)
processConfig config gateFd infoFd =
  TypedProcess.setCloseFds False .
  TypedProcess.setCreateGroup True .
  TypedProcess.setStdin TypedProcess.createPipe .
  TypedProcess.setStdout TypedProcess.createPipe .
  TypedProcess.setStderr TypedProcess.byteStringOutput $
  TypedProcess.proc config.bwrapPath (bwrapArguments config gateFd infoFd)

bwrapArguments :: Config -> Fd -> Fd -> [String]
bwrapArguments config gateFd infoFd =
  [ "--unshare-all", "--share-net", "--unshare-user"
  , "--die-with-parent", "--new-session", "--as-pid-1"
  , "--disable-userns", "--cap-drop", "ALL"
  , "--info-fd", showFd infoFd
  , "--block-fd", showFd gateFd
  , "--dir", "/usr", "--ro-bind", "/usr", "/usr"
  , "--symlink", "usr/lib", "/lib"
  , "--symlink", "usr/lib64", "/lib64"
  , "--dir", "/opt", "--dir", "/opt/cosmobot"
  , "--dir", "/etc", "--dir", "/etc/ssl", "--dir", "/etc/ssl/certs"
  , "--ro-bind", config.workerPath, "/opt/cosmobot/worker.py"
  ]
  <> [ "--ro-bind", "/etc/resolv.conf", "/etc/resolv.conf"
     , "--ro-bind", "/etc/hosts", "/etc/hosts"
     , "--ro-bind", "/etc/nsswitch.conf", "/etc/nsswitch.conf"
     , "--ro-bind", "/etc/ssl/certs", "/etc/ssl/certs"
     , "--dir", "/dev", "--dev-bind", "/dev/null", "/dev/null", "--chmod", "0555", "/dev"
     , "--size", "67108864", "--tmpfs", "/work", "--chmod", "0700", "/work"
     , "--dir", "/work/tmp", "--chdir", "/work", "--clearenv"
     , "--setenv", "HOME", "/work"
     , "--setenv", "TMPDIR", "/work/tmp"
     , "--setenv", "LANG", "C.UTF-8"
     , "--setenv", "PYTHONDONTWRITEBYTECODE", "1"
     , "--setenv", "SSL_CERT_FILE", "/etc/ssl/certs/ca-certificates.crt"
     , "--setenv", "SSL_CERT_DIR", "/etc/ssl/certs"
     , "--", "/usr/bin/python3", "-I", "-S", "-B", "/opt/cosmobot/worker.py"
     ]
  where
    showFd (Fd fd) = show fd

closeQuietly :: (FileSystem :> es, IOE :> es) => Handle -> Eff es ()
closeQuietly fileHandle = FileSystemIO.hClose fileHandle `catchSync` \_ -> pure ()

closeFdQuietly :: IOE :> es => Fd -> Eff es ()
closeFdQuietly fd = liftIO (Posix.closeFd fd) `catchSync` \_ -> pure ()

throwSandbox :: IOE :> es => Text -> Eff es a
throwSandbox = throwIO . SandboxError
