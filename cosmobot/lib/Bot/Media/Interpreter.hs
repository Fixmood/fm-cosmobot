{-|
Module      : Bot.Media.Interpreter
Description : Media effect interpreter backed by the local cache and optional S3 publishing
Stability   : experimental
-}

module Bot.Media.Interpreter
  ( runMedia
  )
where

import Bot.Effect.Media
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Media.Cache as Cache
import qualified Bot.Media.Config as MediaConfig
import qualified Bot.Media.Object as MediaObject
import qualified Bot.Media.S3 as S3
import Bot.Prelude
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import Effectful.Process (Process)
import qualified Effectful.Timeout as Timeout
import qualified Network.HTTP.Client as Client
import System.IO.Error (ioError, userError)

data Runtime = Runtime
  { cfg :: !MediaConfig.Config
  , manager :: !Client.Manager
  , s3 :: !S3.Runtime
  }

runMedia
  :: (Concurrent :> es, IOE :> es, KatipE :> es, FileSystem :> es, Process :> es, Fail :> es, HTTP.HTTP :> es, Storage.Storage :> es, Timeout.Timeout :> es)
  => MediaConfig.Config
  -> Eff (Media : es) a
  -> Eff es a
runMedia cfg inner = do
  manager <- HTTP.manager
  s3 <- S3.newRuntime manager cfg
  let runtime = Runtime{cfg, manager, s3}
  interpret
    ( \_ -> \case
        StoreMediaObject mediaObject ->
          Just <$> cacheObject runtime Nothing mediaObject
        StoreMediaObjectFromSource sourceRef mediaObject ->
          Just <$> cacheObject runtime (Just sourceRef) mediaObject
        MediaRefForSource sourceRef ->
          fmap (Cache.mediaIdForFileId . (.fileId)) <$> Cache.loadCachedMediaBySource (cacheConfig runtime) sourceRef
        GetMediaCacheEntry fileId ->
          Cache.loadMediaCacheEntry (cacheConfig runtime) fileId
        DeleteMediaFile fileId ->
          Cache.deleteCachedMedia (cacheConfig runtime) fileId
        GetMediaFileInfo fileId ->
          Cache.loadMediaFileInfo (cacheConfig runtime) fileId
        ListMediaFiles ->
          Cache.listMediaFiles (cacheConfig runtime)
        GetMediaCacheStats ->
          Cache.mediaCacheStats (cacheConfig runtime)
        GcMediaCache maxAgeSeconds retainedFileIds ->
          Cache.gcMediaCacheRetaining (cacheConfig runtime) maxAgeSeconds retainedFileIds
        NormalizeMediaRef ref ->
          normalizeRef runtime ref
        PublicMediaRef ref ->
          publicRef runtime ref
        ModelImageRef ref ->
          modelRef runtime ref
        LocalMediaPath ref ->
          localPath runtime ref
        PlatformMediaRef platform scope ref ->
          Cache.loadPlatformRef (cacheConfig runtime) platform scope ref
        StorePlatformMediaRef platform scope ref platformRef ->
          Cache.storePlatformRef platform scope ref platformRef
    )
    inner

mediaNormalizeTimeoutMicroseconds :: Int
mediaNormalizeTimeoutMicroseconds =
  15_000_000

normalizeRef :: (Concurrent :> es, IOE :> es, KatipE :> es, FileSystem :> es, Process :> es, Fail :> es, Storage.Storage :> es, Timeout.Timeout :> es) => Runtime -> Text -> Eff es Text
normalizeRef runtime ref
  | Cache.isMediaId ref =
      pure ref
  | "data:image/" `Text.isPrefixOf` Text.strip ref =
      case MediaObject.decodeDataMediaObject ref of
        Nothing -> do
          logError "Skipping invalid data:image media reference"
          pure ref
        Just mediaObject ->
          cacheObject runtime Nothing mediaObject
  | "http://" `Text.isPrefixOf` Text.toLower (Text.strip ref) ||
      "https://" `Text.isPrefixOf` Text.toLower (Text.strip ref) = do
      normalizeRemoteRef runtime ref
  | "file://" `Text.isPrefixOf` Text.strip ref = do
      mediaObject <- (Just <$> MediaObject.fileObject ref) `catchSync` \err -> do
        logError [i|Local media read failed: #{show err :: String}|]
        pure Nothing
      maybe (pure ref) (cacheObject runtime (Just (Text.strip ref))) mediaObject
  | otherwise =
      pure ref

normalizeRemoteRef
  :: (Concurrent :> es, IOE :> es, KatipE :> es, FileSystem :> es, Process :> es, Fail :> es, Storage.Storage :> es, Timeout.Timeout :> es)
  => Runtime
  -> Text
  -> Eff es Text
normalizeRemoteRef runtime ref = do
  result <- Timeout.timeout mediaNormalizeTimeoutMicroseconds $
    normalizeRemoteRefUnsafe runtime ref
  case result of
    Just (Just normalized) ->
      pure normalized
    Just Nothing ->
      pure ref
    Nothing -> do
      logWarning [i|Remote media normalization timed out; keeping original ref: #{Text.take 160 ref}|]
      pure ref

normalizeRemoteRefUnsafe
  :: (Concurrent :> es, IOE :> es, KatipE :> es, FileSystem :> es, Process :> es, Fail :> es, Storage.Storage :> es)
  => Runtime
  -> Text
  -> Eff es (Maybe Text)
normalizeRemoteRefUnsafe runtime ref =
  ( do
      downloaded <- MediaObject.downloadObject runtime.manager ref
      Just <$> cacheObject runtime (Just (Text.strip ref)) downloaded
  ) `catchSync` \err -> do
    logError [i|Remote media download failed: #{show err :: String}|]
    pure Nothing

publicRef :: (Concurrent :> es, IOE :> es, KatipE :> es, FileSystem :> es, Process :> es, Fail :> es, Storage.Storage :> es, Timeout.Timeout :> es) => Runtime -> Text -> Eff es Text
publicRef runtime ref =
  case Cache.parseMediaId ref of
    Nothing -> normalizeRef runtime ref >>= publicRef runtime
    Just fileId ->
      Cache.loadCachedMedia (cacheConfig runtime) fileId >>= \case
        Nothing ->
          pure ref
        Just cached ->
          S3.ensurePublicObject runtime.s3 cached

modelRef
  :: (Concurrent :> es, IOE :> es, KatipE :> es, FileSystem :> es, Process :> es, Fail :> es, Storage.Storage :> es, Timeout.Timeout :> es)
  => Runtime
  -> Text
  -> Eff es Text
modelRef runtime rawRef
  | "data:image/" `Text.isPrefixOf` Text.toLower stripped
  , ";base64," `Text.isInfixOf` stripped =
      pure stripped
  | otherwise = do
      normalized <- normalizeRef runtime stripped
      case Cache.parseMediaId normalized of
        Nothing ->
          liftIO $ ioError (userError "The image could not be downloaded into the local media cache.")
        Just fileId ->
          Cache.loadCachedMedia (cacheConfig runtime) fileId >>= \case
            Nothing ->
              liftIO $ ioError (userError "The cached image is no longer available.")
            Just cached
              | not ("image/" `Text.isPrefixOf` Text.toLower cached.mimeType) ->
                  liftIO $ ioError (userError "The referenced media is not an image.")
              | otherwise -> do
                  bytes <- FileSystemByteString.readFile cached.path
                  pure
                    ( "data:"
                        <> cached.mimeType
                        <> ";base64,"
                        <> TextEncoding.decodeUtf8 (Base64.encode bytes)
                    )
  where
    stripped = Text.strip rawRef

localPath :: (FileSystem :> es, Storage.Storage :> es, IOE :> es) => Runtime -> Text -> Eff es (Maybe FilePath)
localPath runtime ref =
  case Cache.parseMediaId ref of
    Nothing ->
      pure Nothing
    Just fileId ->
      fmap (.path) <$> Cache.loadCachedMedia (cacheConfig runtime) fileId

cacheObject :: (Concurrent :> es, IOE :> es, KatipE :> es, FileSystem :> es, Process :> es, Fail :> es, Storage.Storage :> es) => Runtime -> Maybe Text -> MediaObject -> Eff es Text
cacheObject runtime sourceRef mediaObject = do
  prepared <- prepareMediaObject runtime mediaObject
  Cache.cacheMediaObject (cacheConfig runtime) sourceRef prepared >>= \case
    Cache.CreatedCachedMedia cached -> do
      S3.uploadPublicObject True runtime.s3 cached
      pure (Cache.mediaIdForFileId cached.fileId)
    Cache.ReusedCachedMedia cached ->
      pure (Cache.mediaIdForFileId cached.fileId)

prepareMediaObject :: (IOE :> es, KatipE :> es, FileSystem :> es, Process :> es, Fail :> es) => Runtime -> MediaObject -> Eff es MediaObject
prepareMediaObject _ mediaObject =
  pure mediaObject

cacheConfig :: Runtime -> Cache.CacheConfig
cacheConfig runtime =
  Cache.CacheConfig
    { directory = runtime.cfg.cacheDir
    }
