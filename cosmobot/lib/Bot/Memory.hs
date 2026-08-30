{-|
Module      : Bot.Memory
Description : Per-sender and per-chat structural memory files
Stability   : experimental
-}

module Bot.Memory
  ( MemoryConfig (..)
  , MemoryScope (..)
  , memoryLimitChars
  , senderMemoryScope
  , chatMemoryScope
  , loadMemory
  , replaceMemory
  , clearMemory
  , listPrivatePersonas
  , listGroupPersonas
  , listMemberStyles
  , initializeMemoryRepo
  , memoryPath
  )
where

import Bot.Core.Message
import Bot.Prelude
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified Effectful.FileSystem.IO.File as FileSystemFile
import Effectful.Process (Process, proc, readCreateProcessWithExitCode)
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO.Error (userError)

-- | Filesystem-backed memory settings.
newtype MemoryConfig = MemoryConfig
  { dir :: FilePath
  }
  deriving (Show)

data MemoryScope
  = SenderMemory !ChatPlatform !Text
  | ChatMemory !ChatPlatform !Integer
  | PrivatePersonaMemory !Text
  | DefaultPrivatePersonaMemory
  | GroupPersonaMemory !Integer
  | DefaultGroupPersonaMemory
  | MemberStyleMemory !Text
  deriving (Eq, Show)

memoryLimitChars :: Int
memoryLimitChars = 1000

senderMemoryScope :: IncomingMessage -> Either Text MemoryScope
senderMemoryScope message =
  case message.senderId of
    Nothing ->
      Left "No sender id is available for this message."
    Just senderId ->
      Right (SenderMemory message.platform senderId)

chatMemoryScope :: IncomingMessage -> Either Text MemoryScope
chatMemoryScope message =
  case message.chatId of
    Nothing ->
      Left "No chat id is available for this message."
    Just chatId ->
      Right (ChatMemory message.platform chatId)

loadMemory :: FileSystem :> es => MemoryConfig -> MemoryScope -> Eff es (Maybe Text)
loadMemory cfg scope = do
  exists <- FileSystem.doesFileExist path
  if exists
    then nonEmptyMemory . TextEncoding.decodeUtf8 <$> FileSystemByteString.readFile path
    else pure Nothing
  where
    path = memoryPath cfg scope

replaceMemory :: (FileSystem :> es, IOE :> es, Process :> es) => MemoryConfig -> MemoryScope -> Text -> Eff es ()
replaceMemory cfg scope memory =
  updateMemory cfg scope $ \path -> do
    FileSystem.createDirectoryIfMissing True (takeDirectory path)
    FileSystemFile.writeBinaryFileAtomic path (TextEncoding.encodeUtf8 (Text.strip memory))

clearMemory :: (FileSystem :> es, IOE :> es, Process :> es) => MemoryConfig -> MemoryScope -> Eff es ()
clearMemory cfg scope =
  updateMemory cfg scope removeMemoryFile

listPrivatePersonas :: FileSystem :> es => MemoryConfig -> Eff es [(Text, Text)]
listPrivatePersonas cfg = do
  let dir = cfg.dir </> "qq" </> "private_persona"
  exists <- FileSystem.doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- List.sort <$> FileSystem.listDirectory dir
      catMaybes <$> traverse loadEntry entries
  where
    loadEntry entry
      | takeExtension entry /= ".md" = pure Nothing
      | not (Text.all (`elem` ['0' .. '9']) (Text.pack (dropExtension entry))) = pure Nothing
      | otherwise = do
          let userId = Text.pack (dropExtension entry)
          content <- loadMemory cfg (PrivatePersonaMemory userId)
          pure ((userId,) <$> content)

listGroupPersonas :: FileSystem :> es => MemoryConfig -> Eff es [(Integer, Text)]
listGroupPersonas cfg = do
  let dir = cfg.dir </> "qq" </> "group_persona"
  exists <- FileSystem.doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- List.sort <$> FileSystem.listDirectory dir
      catMaybes <$> traverse loadEntry entries
  where
    loadEntry entry
      | takeExtension entry /= ".md" = pure Nothing
      | otherwise = case readMaybe (dropExtension entry) of
          Nothing -> pure Nothing
          Just groupId -> do
            content <- loadMemory cfg (GroupPersonaMemory groupId)
            pure ((groupId,) <$> content)

listMemberStyles :: FileSystem :> es => MemoryConfig -> Eff es [(Text, Text)]
listMemberStyles cfg = do
  let dir = cfg.dir </> "qq" </> "member_style"
  exists <- FileSystem.doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- List.sort <$> FileSystem.listDirectory dir
      catMaybes <$> traverse loadEntry entries
  where
    loadEntry entry
      | takeExtension entry /= ".md" = pure Nothing
      | not (Text.all (`elem` ['0' .. '9']) (Text.pack (dropExtension entry))) = pure Nothing
      | otherwise = do
          let userId = Text.pack (dropExtension entry)
          content <- loadMemory cfg (MemberStyleMemory userId)
          pure ((userId,) <$> content)

initializeMemoryRepo :: (FileSystem :> es, IOE :> es, Process :> es) => MemoryConfig -> Eff es ()
initializeMemoryRepo cfg = do
  FileSystem.createDirectoryIfMissing True cfg.dir
  runGit cfg ["init", "--quiet"]
  hasHead <- gitSucceeds cfg ["rev-parse", "--verify", "HEAD"]
  commitMemory cfg (if hasHead then "Update memory" else "Initialize memory") (not hasHead)

updateMemory
  :: (FileSystem :> es, IOE :> es, Process :> es)
  => MemoryConfig
  -> MemoryScope
  -> (FilePath -> Eff es ())
  -> Eff es ()
updateMemory cfg scope update = mask \restore -> do
  previous <- readMemoryFile path
  restore (update path >> commitMemoryPath cfg path)
    `onException` rollbackMemoryFile cfg path previous
  where
    path = memoryPath cfg scope

readMemoryFile :: FileSystem :> es => FilePath -> Eff es (Maybe ByteString)
readMemoryFile path = do
  exists <- FileSystem.doesFileExist path
  if exists
    then Just <$> FileSystemByteString.readFile path
    else pure Nothing

rollbackMemoryFile
  :: (FileSystem :> es, IOE :> es, Process :> es)
  => MemoryConfig
  -> FilePath
  -> Maybe ByteString
  -> Eff es ()
rollbackMemoryFile cfg path previous = do
  case previous of
    Nothing -> removeMemoryFile path
    Just contents -> FileSystemFile.writeBinaryFileAtomic path contents
  runGit cfg ["reset", "--quiet", "HEAD", "--", makeRelative cfg.dir path]

removeMemoryFile :: FileSystem :> es => FilePath -> Eff es ()
removeMemoryFile path = do
  exists <- FileSystem.doesFileExist path
  when exists (FileSystem.removeFile path)

commitMemoryPath :: (IOE :> es, Process :> es) => MemoryConfig -> FilePath -> Eff es ()
commitMemoryPath cfg path = do
  stageMemoryPath cfg path
  changed <- not <$> gitSucceeds cfg ["diff", "--cached", "--quiet", "--", makeRelative cfg.dir path]
  when changed $
    runGit cfg
      [ "-c", "user.name=Cosmobot"
      , "-c", "user.email=cosmobot@localhost"
      , "commit", "--quiet", "-m", "Update memory"
      ]

stageMemoryPath :: (IOE :> es, Process :> es) => MemoryConfig -> FilePath -> Eff es ()
stageMemoryPath cfg path =
  runGit cfg ["add", "--all", "--", makeRelative cfg.dir path]

commitMemory :: (IOE :> es, Process :> es) => MemoryConfig -> String -> Bool -> Eff es ()
commitMemory cfg message allowEmpty = do
  runGit cfg ["add", "--all"]
  changed <- not <$> gitSucceeds cfg ["diff", "--cached", "--quiet"]
  when (allowEmpty || changed) $
    runGit cfg
      [ "-c", "user.name=Cosmobot"
      , "-c", "user.email=cosmobot@localhost"
      , "commit", "--quiet", "--allow-empty", "-m", message
      ]

runGit :: (IOE :> es, Process :> es) => MemoryConfig -> [String] -> Eff es ()
runGit cfg args = do
  (exitCode, _, stderrText) <- git cfg args
  unless (exitCode == ExitSuccess) $
    throwIO $ userError $ "git " <> List.intercalate " " args <> " failed: " <> stderrText

gitSucceeds :: Process :> es => MemoryConfig -> [String] -> Eff es Bool
gitSucceeds cfg args = do
  (exitCode, _, _) <- git cfg args
  pure (exitCode == ExitSuccess)

git :: Process :> es => MemoryConfig -> [String] -> Eff es (ExitCode, String, String)
git cfg args =
  readCreateProcessWithExitCode (proc "git" (["-C", cfg.dir] <> args)) ""

memoryPath :: MemoryConfig -> MemoryScope -> FilePath
memoryPath cfg scope =
  cfg.dir </> platformPathPart platform </> scopeKind </> Text.unpack scopeId <.> "md"
  where
    (platform, scopeKind, scopeId) =
      case scope of
        SenderMemory scopePlatform senderId ->
          (scopePlatform, "sender", senderId)
        ChatMemory scopePlatform chatId ->
          (scopePlatform, "chat", Text.pack (show chatId))
        PrivatePersonaMemory userId ->
          (PlatformQQ, "private_persona", userId)
        DefaultPrivatePersonaMemory ->
          (PlatformQQ, "private_persona", "_default")
        GroupPersonaMemory groupId ->
          (PlatformQQ, "group_persona", Text.pack (show groupId))
        DefaultGroupPersonaMemory ->
          (PlatformQQ, "group_persona", "_default")
        MemberStyleMemory userId ->
          (PlatformQQ, "member_style", userId)

platformPathPart :: ChatPlatform -> FilePath
platformPathPart = toString . chatPlatformKey

nonEmptyMemory :: Text -> Maybe Text
nonEmptyMemory text =
  let stripped = Text.strip text
  in if Text.null stripped then Nothing else Just stripped
