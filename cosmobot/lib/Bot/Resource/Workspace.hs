{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Bot.Resource.Workspace
Description : Superuser work directories
Stability   : experimental
-}
module Bot.Resource.Workspace
  ( Workspace
  , WorkspaceArgs (..)
  , validateWorkId
  , createWorkspaceAt
  , restoreWorkspaceAt
  , queryWorkspace
  , updateWorkspace
  )
where

import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Bot.Util.Process as ProcessUtil
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Char as Char
import qualified Effectful.Concurrent.MVar as MVar
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified Effectful.Process.Typed as TypedProcess
import System.Exit (ExitCode (..))
import System.FilePath ((</>))

data Workspace = Workspace
  { workId :: !Text
  , path :: !FilePath
  , lock :: !(MVar.MVar ())
  }

data WorkspaceArgs = WorkspaceArgs
  { workId :: !Text
  , goal :: !Text
  , ttlMinutes :: !Int
  }

instance
  (FileSystem.FileSystem :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Resource.ResourceObject (Eff es) Workspace where
  type CreationArgs Workspace = WorkspaceArgs

  resourceTypeName _ = "Workspace"
  resourceTTLSeconds = Resource.ttlFromMinutes . (.ttlMinutes)

  resourcePersistence _ = Resource.PersistentResource
    { encodeResource = (.workId)
    , restoreResource = restoreWorkspaceAt "/work"
    }

  createResourceObject Resource.Init{arguments} =
    createWorkspaceAt "/work" arguments

  destroyResourceObject workspace =
    attempt "destroy" (FileSystem.removePathForcibly workspace.path)

  probeResourceObject workspace =
    attempt "probe" (FileSystem.doesDirectoryExist workspace.path) <&> \case
      Right True -> Right "ready"
      Right False -> Left "Workspace directory is missing."
      Left err -> Left err

  describeResourceObject workspace _ =
    pure workspace.workId

  detailResourceObject workspace =
    withWorkspace workspace $
      readWork workspace <&> either id ("WORK.md:\n" <>)

createWorkspaceAt
  :: (FileSystem.FileSystem :> es, Concurrent :> es, IOE :> es)
  => FilePath
  -> WorkspaceArgs
  -> Eff es (Either Text Workspace)
createWorkspaceAt root arguments = mask \restore -> do
  case validateWorkId arguments.workId of
    Left err -> pure (Left err)
    Right workId -> do
      let path = root </> Text.unpack workId
          cleanup = void (trySync (FileSystem.removePathForcibly path))
      attempt "create root" (restore (FileSystem.createDirectoryIfMissing True root)) >>= \case
        Left err -> pure (Left err)
        Right () -> attempt "create" (restore (FileSystem.createDirectory path)) >>= \case
          Left err -> pure (Left err)
          Right () -> attempt "write WORK.md"
            (restore (writeWork path arguments.goal) `onException` cleanup) >>= \case
              Left err -> pure (Left err)
              Right () -> Right . Workspace workId path <$> MVar.newMVar ()

restoreWorkspaceAt
  :: (FileSystem.FileSystem :> es, Concurrent :> es)
  => FilePath
  -> Text
  -> Eff es (Either Text Workspace)
restoreWorkspaceAt root payload =
  case validateWorkId payload of
    Left err -> pure (Left err)
    Right workId -> do
      let path = root </> Text.unpack workId
      FileSystem.doesDirectoryExist path >>= \case
        False -> pure (Left "Workspace directory is missing.")
        True -> Right . Workspace workId path <$> MVar.newMVar ()

queryWorkspace
  :: (FileSystem.FileSystem :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Workspace
  -> Eff es (Either Text Text)
queryWorkspace workspace = withWorkspace workspace do
  goalResult <- readWork workspace
  treeResult <- attemptProcess "query" "tree" ["-L", "1", workspace.path]
  pure do
    goal <- goalResult
    tree <- treeResult
    pure $ "WORK.md:\n" <> goal <> "\n\ntree -L 1 " <> Text.pack workspace.path <> ":\n" <> tree

updateWorkspace
  :: (FileSystem.FileSystem :> es, Concurrent :> es, IOE :> es)
  => Workspace
  -> Text
  -> Eff es (Either Text ())
updateWorkspace workspace goal =
  withWorkspace workspace (attempt "update WORK.md" (writeWork workspace.path goal))

readWork :: FileSystem.FileSystem :> es => Workspace -> Eff es (Either Text Text)
readWork workspace = do
  let path = workFile workspace.path
  FileSystem.doesFileExist path >>= \case
    False -> pure (Left "Workspace WORK.md is missing.")
    True ->
      first (const "WORK.md is not valid UTF-8.")
        . TextEncoding.decodeUtf8'
        <$> FileSystemByteString.readFile path

withWorkspace :: Concurrent :> es => Workspace -> Eff es a -> Eff es a
withWorkspace workspace action =
  mask \restore -> do
    MVar.takeMVar workspace.lock
    restore action `finally` MVar.putMVar workspace.lock ()

writeWork :: FileSystem.FileSystem :> es => FilePath -> Text -> Eff es ()
writeWork path goal =
  (FileSystemByteString.writeFile temporary (TextEncoding.encodeUtf8 goal)
    >> FileSystem.renameFile temporary (workFile path))
    `onException` void (trySync (FileSystem.removeFile temporary))
  where
    temporary = path </> ".WORK.md.tmp"

workFile :: FilePath -> FilePath
workFile path = path </> "WORK.md"

validateWorkId :: Text -> Either Text Text
validateWorkId value
  | Text.null value = Left "id must not be empty."
  | value `elem` [".", ".."] || Text.any (not . validCharacter) value =
      Left "id may contain only letters, digits, dot, underscore, and hyphen."
  | otherwise = Right value
  where
    validCharacter character = Char.isAscii character && (Char.isAlphaNum character || character `elem` ("._-" :: String))

attempt :: IOE :> es => Text -> Eff es a -> Eff es (Either Text a)
attempt operation action =
  trySync action <&> first (\err -> "Workspace " <> operation <> " failed: " <> Text.take 500 (show err))

attemptProcess
  :: (Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Text
  -> FilePath
  -> [String]
  -> Eff es (Either Text Text)
attemptProcess operation executable args =
  trySync (ProcessUtil.readProcessGroupWithExitCode executable args) <&> \case
    Left _ -> Left ("Workspace " <> operation <> " failed: " <> Text.pack executable <> " is unavailable.")
    Right (ExitSuccess, stdoutText, _) -> Right stdoutText
    Right (ExitFailure code, stdoutText, stderrText) ->
      let detail = Text.take 500 . Text.strip $ if Text.null (Text.strip stderrText) then stdoutText else stderrText
      in Left $ "Workspace " <> operation <> " failed (exit " <> show code <> ")"
        <> if Text.null detail then "." else ": " <> detail
