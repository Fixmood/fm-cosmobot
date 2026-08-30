{-# LANGUAGE OverloadedLabels #-}
{-|
Module      : Bot.Media.Cache
Description : Local content-addressed media cache
Stability   : experimental
-}


module Bot.Media.Cache
  ( CachedMedia (..)
  , CachedMediaWrite (..)
  , CacheConfig (..)
  , cacheMediaObject
  , deleteCachedMedia
  , loadCachedMedia
  , loadMediaCacheEntry
  , loadMediaFileInfo
  , loadCachedMediaByRef
  , loadCachedMediaBySource
  , listMediaFiles
  , mediaCacheStats
  , mediaIdForFileId
  , parseMediaId
  , isMediaId
  , gcMediaCache
  , gcMediaCacheRetaining
  , loadPlatformRef
  , storePlatformRef
  , extensionFor
  )
where

import Bot.Effect.Media (MediaCacheEntry (..), MediaCacheStats (..), MediaFileInfo (..), MediaObject (..), MediaPlatformRefInfo (..))
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Media.Mime as Mime
import Bot.Prelude
import Bot.Storage.Prelude
import Crypto.Hash (Context, Digest, SHA256, hashFinalize, hashInit, hashUpdate)
import qualified Crypto.Random as CryptoRandom
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Time.Clock.POSIX as POSIX
import Control.Monad.Trans.Resource (runResourceT)
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO as FileSystemIO
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified Effectful.Temporary as Temporary
import qualified Streaming.ByteString as Q
import Streaming (Of (..))
import System.FilePath ((<.>), (</>), takeExtension)

data CacheConfig = CacheConfig
  { directory :: !FilePath
  }
  deriving (Show, Eq)

data CachedMedia = CachedMedia
  { fileId :: !Text
  , digest :: !Text
  , mimeType :: !Text
  , sourceName :: !(Maybe Text)
  , path :: !FilePath
  , size :: !Int
  }
  deriving (Show, Eq)

data CachedMediaWrite
  = CreatedCachedMedia !CachedMedia
  | ReusedCachedMedia !CachedMedia
  deriving (Show, Eq)

data MediaObjectRow = MediaObjectRow
  { file_id :: Text
  , digest :: Text
  , mime_type :: Text
  , source_name :: Maybe Text
  , path :: Text
  , size_bytes :: Int
  , created_at_unix :: Int
  , last_used_at_unix :: Int
  }
  deriving (Generic)

instance SqlRow MediaObjectRow

data MediaSourceRow = MediaSourceRow
  { source_ref :: Text
  , file_id :: Text
  }
  deriving (Generic)

instance SqlRow MediaSourceRow

data MediaPlatformRefRow = MediaPlatformRefRow
  { platform_key :: Text
  , scope_key :: Text
  , file_id :: Text
  , platform_ref :: Text
  , created_at_unix :: Int
  , last_used_at_unix :: Int
  }
  deriving (Generic)

instance SqlRow MediaPlatformRefRow

mediaObjectRows :: Table MediaObjectRow
mediaObjectRows =
  table "media_files"
    [ #file_id :- primary
    , #digest :- index
    ]

mediaSourceRows :: Table MediaSourceRow
mediaSourceRows =
  table "media_sources"
    [ #source_ref :- primary
    ]

mediaPlatformRefRows :: Table MediaPlatformRefRow
mediaPlatformRefRows =
  table "media_platform_refs"
    [ #platform_key :- index
    , #scope_key :- index
    , #file_id :- index
    ]

cacheMediaObject
  :: (Storage.Storage :> es, FileSystem :> es, IOE :> es)
  => CacheConfig
  -> Maybe Text
  -> MediaObject
  -> Eff es CachedMediaWrite
cacheMediaObject cfg sourceRef mediaObject = do
  ensureMediaCacheTables
  case sourceRef of
    Just ref -> do
      cached <- lookupCachedSource cfg ref
      case cached of
        Just media ->
          pure (ReusedCachedMedia media)
        Nothing ->
          storeMediaObject cfg sourceRef mediaObject
    Nothing ->
      storeMediaObject cfg sourceRef mediaObject

loadCachedMedia :: (Storage.Storage :> es, FileSystem :> es, IOE :> es) => CacheConfig -> Text -> Eff es (Maybe CachedMedia)
loadCachedMedia _ targetFileId = do
  ensureMediaCacheTables
  rows <- runSelda $
    query do
      object <- select mediaObjectRows
      restrict (object ! #file_id .== literal targetFileId)
      pure object
  existing <- filterM (FileSystem.doesFileExist . Text.unpack . (.path)) rows
  for_ (viaNonEmpty head existing) \row ->
    touchMediaFile row.file_id
  pure (mediaObjectRowToCached <$> viaNonEmpty head existing)

loadCachedMediaByRef :: (Storage.Storage :> es, FileSystem :> es, IOE :> es) => CacheConfig -> Text -> Eff es (Maybe CachedMedia)
loadCachedMediaByRef cfg ref =
  case parseMediaId ref of
    Nothing ->
      pure Nothing
    Just fileId ->
      loadCachedMedia cfg fileId

loadCachedMediaBySource :: (Storage.Storage :> es, FileSystem :> es, IOE :> es) => CacheConfig -> Text -> Eff es (Maybe CachedMedia)
loadCachedMediaBySource =
  lookupCachedSource

loadMediaFileInfo :: (Storage.Storage :> es, FileSystem :> es) => CacheConfig -> Text -> Eff es (Maybe MediaFileInfo)
loadMediaFileInfo _ targetFileId = do
  ensureMediaCacheTables
  rows <- runSelda $
    query $
      queryLimit 0 1 do
        object <- select mediaObjectRows
        restrict (object ! #file_id .== literal targetFileId)
        pure object
  traverse mediaObjectRowToInfo (viaNonEmpty head rows)

loadMediaCacheEntry :: (Storage.Storage :> es, FileSystem :> es) => CacheConfig -> Text -> Eff es (Maybe MediaCacheEntry)
loadMediaCacheEntry cfg targetFileId = do
  file <- loadMediaFileInfo cfg targetFileId
  traverse
    ( \mediaFile -> do
        sourceRefs <- loadSourceRefs targetFileId
        platformRefs <- loadPlatformRefs targetFileId
        pure MediaCacheEntry
          { file = mediaFile
          , sourceRefs
          , platformRefs
          }
    )
    file

listMediaFiles :: (Storage.Storage :> es, FileSystem :> es) => CacheConfig -> Eff es [MediaFileInfo]
listMediaFiles _ = do
  ensureMediaCacheTables
  rows <- runSelda $
    query do
      object <- select mediaObjectRows
      order (object ! #created_at_unix) descending
      pure object
  traverse mediaObjectRowToInfo rows

mediaCacheStats :: (Storage.Storage :> es, FileSystem :> es) => CacheConfig -> Eff es MediaCacheStats
mediaCacheStats cfg = do
  files <- listMediaFiles cfg
  sourceCount <- runSelda do
    rows <- query (select mediaSourceRows)
    pure (length rows)
  platformRefCount <- runSelda do
    rows <- query (select mediaPlatformRefRows)
    pure (length rows)
  let existingFiles = length (filter (.exists) files)
      missingFiles = length files - existingFiles
      totalBytes = sum [file.size | file <- files, file.exists]
  pure MediaCacheStats
    { files = length files
    , existingFiles
    , missingFiles
    , totalBytes
    , sources = sourceCount
    , platformRefs = platformRefCount
    }

lookupCachedDigest :: (Storage.Storage :> es, FileSystem :> es, IOE :> es) => CacheConfig -> Text -> Eff es (Maybe CachedMedia)
lookupCachedDigest _ targetDigest = do
  ensureMediaCacheTables
  rows <- runSelda $
    query do
      object <- select mediaObjectRows
      restrict (object ! #digest .== literal targetDigest)
      pure object
  existing <- filterM (FileSystem.doesFileExist . Text.unpack . (.path)) rows
  for_ (viaNonEmpty head existing) \row ->
    touchMediaFile row.file_id
  pure (mediaObjectRowToCached <$> viaNonEmpty head existing)

linkSourceRef :: Storage.Storage :> es => Text -> Text -> Eff es ()
linkSourceRef ref fileId = do
  ensureMediaCacheTables
  runSelda do
    deleteFrom_ mediaSourceRows \candidate ->
      candidate ! #source_ref .== literal ref
    insert_ mediaSourceRows [MediaSourceRow{source_ref = ref, file_id = fileId}]

loadPlatformRef :: (Storage.Storage :> es, FileSystem :> es, IOE :> es) => CacheConfig -> Text -> Text -> Text -> Eff es (Maybe Text)
loadPlatformRef cfg platform scope ref =
  case parseMediaId ref of
    Nothing ->
      pure Nothing
    Just fileId -> do
      cached <- loadCachedMedia cfg fileId
      case cached of
        Nothing ->
          pure Nothing
        Just _ -> do
          ensureMediaCacheTables
          rows <- runSelda $
            query do
              row <- select mediaPlatformRefRows
              restrict (row ! #platform_key .== literal platform)
              restrict (row ! #scope_key .== literal scope)
              restrict (row ! #file_id .== literal fileId)
              pure row
          for_ (viaNonEmpty head rows) \_ ->
            touchPlatformRef platform scope fileId
          pure ((.platform_ref) <$> viaNonEmpty head rows)

storePlatformRef :: (Storage.Storage :> es, IOE :> es) => Text -> Text -> Text -> Text -> Eff es ()
storePlatformRef platform scope ref platformRef =
  case parseMediaId ref of
    Nothing ->
      pure ()
    Just fileId -> do
      ensureMediaCacheTables
      now <- liftIO (round <$> POSIX.getPOSIXTime)
      runSelda do
        deleteFrom_ mediaPlatformRefRows \row ->
          row ! #platform_key .== literal platform
            .&& row ! #scope_key .== literal scope
            .&& row ! #file_id .== literal fileId
        insert_ mediaPlatformRefRows
          [ MediaPlatformRefRow
              { platform_key = platform
              , scope_key = scope
              , file_id = fileId
              , platform_ref = platformRef
              , created_at_unix = now
              , last_used_at_unix = now
              }
          ]

loadSourceRefs :: Storage.Storage :> es => Text -> Eff es [Text]
loadSourceRefs targetFileId = do
  ensureMediaCacheTables
  runSelda $
    query do
      row <- select mediaSourceRows
      restrict (row ! #file_id .== literal targetFileId)
      order (row ! #source_ref) ascending
      pure (row ! #source_ref)

loadPlatformRefs :: Storage.Storage :> es => Text -> Eff es [MediaPlatformRefInfo]
loadPlatformRefs targetFileId = do
  ensureMediaCacheTables
  rows <- runSelda $
    query do
      row <- select mediaPlatformRefRows
      restrict (row ! #file_id .== literal targetFileId)
      order (row ! #platform_key) ascending
      order (row ! #scope_key) ascending
      pure row
  pure
    [ MediaPlatformRefInfo
        { platform = row.platform_key
        , scope = row.scope_key
        , platformRef = row.platform_ref
        }
    | row <- rows
    ]

lookupCachedSource :: (Storage.Storage :> es, FileSystem :> es, IOE :> es) => CacheConfig -> Text -> Eff es (Maybe CachedMedia)
lookupCachedSource _ ref = do
  rows <- runSelda $
    query do
      source <- select mediaSourceRows
      object <- select mediaObjectRows
      restrict (source ! #source_ref .== literal ref)
      restrict (source ! #file_id .== object ! #file_id)
      pure object
  existing <- filterM (FileSystem.doesFileExist . Text.unpack . (.path)) rows
  for_ (viaNonEmpty head existing) \row ->
    touchMediaFile row.file_id
  pure (mediaObjectRowToCached <$> viaNonEmpty head existing)

storeMediaObject
  :: (Storage.Storage :> es, FileSystem :> es, IOE :> es)
  => CacheConfig
  -> Maybe Text
  -> MediaObject
  -> Eff es CachedMediaWrite
storeMediaObject cfg sourceRef mediaObject = do
  FileSystem.createDirectoryIfMissing True cfg.directory
  Temporary.runTemporary $
    Temporary.withTempDirectory cfg.directory "cosmobot-media-" \temporaryDirectory -> do
      temporaryFileId <- newFileId
      let temporaryPath = temporaryDirectory </> Text.unpack temporaryFileId <.> "tmp"
      liftIO (runResourceT (Q.writeFile temporaryPath mediaObject.bytes))
      header <- FileSystemIO.withBinaryFile temporaryPath FileSystemIO.ReadMode (`FileSystemByteString.hGet` 512)
      let sniffedMime = Mime.sniffMime header
          storedMediaObject = MediaObject
            { bytes = mediaObject.bytes
            , mimeType = fromMaybe mediaObject.mimeType sniffedMime
            , sourceName = mediaObject.sourceName
            }
          extension = maybe (extensionFor storedMediaObject) Mime.extensionFromMime sniffedMime
      digest <- contentDigestFile temporaryPath
      lookupCachedDigest cfg digest >>= \case
        Just cached -> do
          for_ sourceRef \ref ->
            linkSourceRef ref cached.fileId
          pure (ReusedCachedMedia cached)
        Nothing ->
          CreatedCachedMedia <$> storeStreamedMediaFile cfg sourceRef storedMediaObject extension digest temporaryPath

storeStreamedMediaFile
  :: (Storage.Storage :> es, FileSystem :> es, IOE :> es)
  => CacheConfig
  -> Maybe Text
  -> MediaObject
  -> Text
  -> Text
  -> FilePath
  -> Eff es CachedMedia
storeStreamedMediaFile cfg sourceRef mediaObject extension digest temporaryPath = do
  let relativePath = Text.unpack digest <.> Text.unpack (Text.dropWhile (== '.') extension)
      finalPath = cfg.directory </> relativePath
  exists <- FileSystem.doesFileExist finalPath
  unless exists $
    FileSystem.renameFile temporaryPath finalPath
  when exists $
    removeFileIfExists temporaryPath
  sizeBytes <- fromIntegral <$> FileSystem.getFileSize finalPath
  fileId <- newFileId
  now <- liftIO (round <$> POSIX.getPOSIXTime)
  let row = MediaObjectRow
        { file_id = fileId
        , digest
        , mime_type = mediaObject.mimeType
        , source_name = mediaObject.sourceName
        , path = Text.pack finalPath
        , size_bytes = sizeBytes
        , created_at_unix = now
        , last_used_at_unix = now
        }
  ensureMediaCacheTables
  runSelda do
    insert_ mediaObjectRows [row]
    for_ sourceRef \ref -> do
      deleteFrom_ mediaSourceRows \candidate ->
        candidate ! #source_ref .== literal ref
      insert_ mediaSourceRows [MediaSourceRow{source_ref = ref, file_id = fileId}]
  pure (mediaObjectRowToCached row)

removeFileIfExists :: FileSystem :> es => FilePath -> Eff es ()
removeFileIfExists path = do
  exists <- FileSystem.doesFileExist path
  when exists (FileSystem.removeFile path)
    `catchSync` \_ ->
      pure ()

gcMediaCache
  :: (Storage.Storage :> es, FileSystem :> es, IOE :> es)
  => CacheConfig
  -> Int
  -> Eff es Int
gcMediaCache cfg maxAgeSeconds =
  gcMediaCacheRetaining cfg maxAgeSeconds Set.empty

gcMediaCacheRetaining
  :: (Storage.Storage :> es, FileSystem :> es, IOE :> es)
  => CacheConfig
  -> Int
  -> Set.Set Text
  -> Eff es Int
gcMediaCacheRetaining _ maxAgeSeconds retainedFileIds = do
  ensureMediaCacheTables
  now <- currentUnixSeconds
  let cutoff = now - max 0 maxAgeSeconds
  objects <- runSelda $ query (select mediaObjectRows)
  let expired =
        [ object
        | object <- objects
        , object.last_used_at_unix < cutoff
        , not (Set.member object.file_id retainedFileIds)
        ]
      expiredIds = Set.fromList (map (.file_id) expired)
      retainedPaths =
        Set.fromList
          [ object.path
          | object <- objects
          , not (Set.member object.file_id expiredIds)
          ]
      removable =
        [ object
        | object <- expired
        , not (Set.member object.path retainedPaths)
        ]
      expiredFileIds = map (.file_id) expired
  traverse_ removeCachedFile removable
  runSelda $ transaction do
    for_ expiredFileIds \fileId -> do
      deleteFrom_ mediaPlatformRefRows \row ->
        row ! #file_id .== literal fileId
      deleteFrom_ mediaSourceRows \row ->
        row ! #file_id .== literal fileId
      deleteFrom_ mediaObjectRows \row ->
        row ! #file_id .== literal fileId
  pure (length expired)

deleteCachedMedia
  :: (Storage.Storage :> es, FileSystem :> es)
  => CacheConfig
  -> Text
  -> Eff es Bool
deleteCachedMedia _ targetFileId = do
  ensureMediaCacheTables
  rows <- runSelda $
    query $
      queryLimit 0 1 do
        object <- select mediaObjectRows
        restrict (object ! #file_id .== literal targetFileId)
        pure object
  case viaNonEmpty head rows of
    Nothing ->
      pure False
    Just object -> do
      pathShared <- isPathShared object
      unless pathShared (removeCachedFile object)
      runSelda $ transaction do
        deleteFrom_ mediaPlatformRefRows \row ->
          row ! #file_id .== literal targetFileId
        deleteFrom_ mediaSourceRows \row ->
          row ! #file_id .== literal targetFileId
        deleteFrom_ mediaObjectRows \row ->
          row ! #file_id .== literal targetFileId
      pure True

isPathShared :: Storage.Storage :> es => MediaObjectRow -> Eff es Bool
isPathShared object = do
  rows <- runSelda $
    query do
      row <- select mediaObjectRows
      restrict (row ! #path .== literal object.path)
      restrict (row ! #file_id ./= literal object.file_id)
      pure (row ! #file_id)
  pure (not (null rows))

removeCachedFile :: FileSystem :> es => MediaObjectRow -> Eff es ()
removeCachedFile row = do
  let filePath = Text.unpack row.path
  exists <- FileSystem.doesFileExist filePath
  when exists (FileSystem.removeFile filePath)
    `catchSync` \_ ->
      pure ()

ensureMediaCacheTables :: Storage.Storage :> es => Eff es ()
ensureMediaCacheTables =
  runSelda do
    tryCreateTable mediaObjectRows
    tryCreateTable mediaSourceRows
    tryCreateTable mediaPlatformRefRows

touchMediaFile :: (Storage.Storage :> es, IOE :> es) => Text -> Eff es ()
touchMediaFile fileId = do
  now <- currentUnixSeconds
  runSelda $
    update_
      mediaObjectRows
      (\row -> row ! #file_id .== literal fileId)
      (\row -> row `with` [#last_used_at_unix := literal now])

touchPlatformRef :: (Storage.Storage :> es, IOE :> es) => Text -> Text -> Text -> Eff es ()
touchPlatformRef platform scope fileId = do
  now <- currentUnixSeconds
  runSelda $
    update_
      mediaPlatformRefRows
      ( \row ->
          row ! #platform_key .== literal platform
            .&& row ! #scope_key .== literal scope
            .&& row ! #file_id .== literal fileId
      )
      (\row -> row `with` [#last_used_at_unix := literal now])

currentUnixSeconds :: IOE :> es => Eff es Int
currentUnixSeconds =
  liftIO (round <$> POSIX.getPOSIXTime)

mediaObjectRowToCached :: MediaObjectRow -> CachedMedia
mediaObjectRowToCached row =
  CachedMedia
    { fileId = row.file_id
    , digest = row.digest
    , mimeType = row.mime_type
    , sourceName = row.source_name
    , path = Text.unpack row.path
    , size = row.size_bytes
    }

mediaObjectRowToInfo :: FileSystem :> es => MediaObjectRow -> Eff es MediaFileInfo
mediaObjectRowToInfo row = do
  exists <- FileSystem.doesFileExist (Text.unpack row.path)
  pure MediaFileInfo
    { fileId = row.file_id
    , ref = mediaIdForFileId row.file_id
    , digest = row.digest
    , mimeType = row.mime_type
    , sourceName = row.source_name
    , path = Text.unpack row.path
    , size = row.size_bytes
    , createdAtUnix = row.created_at_unix
    , lastUsedAtUnix = row.last_used_at_unix
    , exists
    }

contentDigestFile :: (IOE :> es) => FilePath -> Eff es Text
contentDigestFile path = do
  digestContext :> () <- liftIO $ runResourceT $
    Q.foldlChunks hashUpdate (hashInit :: Context SHA256) (Q.readFile path)
  pure (Text.pack (show (hashFinalize digestContext :: Digest SHA256)))

mediaIdForFileId :: Text -> Text
mediaIdForFileId fileId =
  "media:" <> fileId

parseMediaId :: Text -> Maybe Text
parseMediaId ref = do
  fileId <- Text.stripPrefix "media:" (Text.strip ref)
  guard (isValidFileId fileId)
  pure fileId
  where
    isValidFileId fileId =
      "mf_" `Text.isPrefixOf` fileId &&
        Text.length fileId >= 10 &&
        Text.all isFileIdChar fileId

    isFileIdChar char =
      (char >= 'a' && char <= 'z') ||
        (char >= 'A' && char <= 'Z') ||
        (char >= '0' && char <= '9') ||
        char == '-' ||
        char == '_'

isMediaId :: Text -> Bool
isMediaId =
  isJust . parseMediaId

newFileId :: IOE :> es => Eff es Text
newFileId = do
  bytes <- liftIO (CryptoRandom.getRandomBytes 16 :: IO StrictByteString.ByteString)
  pure ("mf_" <> TextEncoding.decodeUtf8 (Base64URL.encodeUnpadded bytes))

extensionFor :: MediaObject -> Text
extensionFor mediaObject =
  case mediaObject.sourceName >>= extensionFromName of
    Just ext -> ext
    Nothing -> Mime.extensionFromMime mediaObject.mimeType

extensionFromName :: Text -> Maybe Text
extensionFromName name =
  let ext = Text.pack (takeExtension (Text.unpack name))
  in if Text.null ext then Nothing else Just ext
