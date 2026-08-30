{-|
Module      : Bot.Media.S3
Description : S3 media publishing
Stability   : experimental
-}


module Bot.Media.S3
  ( Runtime
  , newRuntime
  , ensurePublicObject
  , uploadPublicObject
  )
where

import qualified Amazonka as AWS
import qualified Amazonka.Auth as AWSAuth
import qualified Amazonka.S3 as S3
import qualified Amazonka.S3.PutObject as PutObject
import Bot.Effect.Media (MediaObject (..))
import qualified Bot.Media.Cache as Cache
import qualified Bot.Media.Config as MediaConfig
import Bot.Media.S3.Config
import Bot.Prelude
import qualified Data.ByteString as StrictByteString
import qualified Data.Cache.LRU as LRU
import qualified Data.Text as Text
import qualified Effectful.Concurrent.MVar as MVar
import qualified Streaming.ByteString as Q
import qualified Data.Text.Encoding as TextEncoding
import Effectful.FileSystem (FileSystem)
import GHC.Clock (getMonotonicTimeNSec)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTPStatus
import System.IO.Error (ioError, userError)

data Runtime = Runtime
  { cfg :: !MediaConfig.Config
  , env :: !(Maybe AWS.Env)
  , publicObjects :: !(MVar.MVar (LRU.LRU Text Text))
  }

newRuntime :: (Concurrent :> es, IOE :> es) => HTTP.Manager -> MediaConfig.Config -> Eff es Runtime
newRuntime manager cfg = do
  env <- if cfg.s3.enabled then Just <$> createAmazonkaEnv manager cfg.s3 else pure Nothing
  publicObjects <- MVar.newMVar (LRU.newLRU (Just 512))
  pure Runtime{cfg, env, publicObjects}


createAmazonkaEnv :: IOE :> es => HTTP.Manager -> Config -> Eff es AWS.Env
createAmazonkaEnv manager cfg = do
  baseEnv <- liftIO (AWS.newEnvFromManager manager (pure . credentials cfg))
  endpoint <- liftIO (traverse parseEndpoint cfg.endpoint)
  pure $ configureEndpoint cfg endpoint baseEnv{AWS.region = AWS.Region' cfg.region}
  where
    credentials Config{accessKeyId = Just accessKeyId, secretAccessKey = Just secretAccessKey} =
      AWSAuth.fromKeys (AWS.AccessKey (TextEncoding.encodeUtf8 accessKeyId)) (AWS.SecretKey (TextEncoding.encodeUtf8 secretAccessKey))
    credentials _ =
      AWSAuth.fromKeys "" ""

parseEndpoint :: Text -> IO (Bool, StrictByteString.ByteString, Int)
parseEndpoint endpoint = do
  request <- HTTP.parseRequest (Text.unpack endpoint)
  pure (HTTP.secure request, HTTP.host request, HTTP.port request)

configureEndpoint :: Config -> Maybe (Bool, StrictByteString.ByteString, Int) -> AWS.Env -> AWS.Env
configureEndpoint cfg endpoint env =
  let service = configureAddressing cfg (configureEndpointService endpoint S3.defaultService)
  in AWS.configureService service env

configureEndpointService :: Maybe (Bool, StrictByteString.ByteString, Int) -> AWS.Service -> AWS.Service
configureEndpointService Nothing service =
  service
configureEndpointService (Just (secure, host, port)) service =
  AWS.setEndpoint secure host port service

configureAddressing :: Config -> AWS.Service -> AWS.Service
configureAddressing cfg service =
  case Text.toLower (Text.strip cfg.addressingStyle) of
    "path" ->
      service{AWS.s3AddressingStyle = AWS.S3AddressingStylePath}
    "virtual" ->
      service{AWS.s3AddressingStyle = AWS.S3AddressingStyleVirtual}
    _ ->
      service{AWS.s3AddressingStyle = AWS.S3AddressingStyleAuto}

ensurePublicObject :: (Concurrent :> es, IOE :> es, KatipE :> es, FileSystem :> es) => Runtime -> Cache.CachedMedia -> Eff es Text
ensurePublicObject runtime cached =
  case publicObjectUrl runtime.cfg cached of
    Nothing ->
      pure (Cache.mediaIdForFileId cached.fileId)
    Just url -> do
      startedAt <- monotonicMilliseconds
      let cachedFileId = cached.fileId
      cachedUrl <- MVar.modifyMVar runtime.publicObjects \cache ->
        let (next, found) = LRU.lookup cachedFileId cache
        in pure (next, found)
      case cachedUrl of
        Just cachedPublicUrl -> do
          finishedAt <- monotonicMilliseconds
          logDebug [i|S3 publicize outcome=cache_hit file_id=#{cachedFileId} duration_ms=#{finishedAt - startedAt}|]
          pure cachedPublicUrl
        Nothing -> do
          uploaded <- storeObject False runtime cached
          for_ uploaded (rememberPublicObject runtime cachedFileId)
          finishedAt <- monotonicMilliseconds
          let outcome = if isJust uploaded then "cache_miss_published" else "failed" :: Text
          logDebug [i|S3 publicize outcome=#{outcome} file_id=#{cachedFileId} duration_ms=#{finishedAt - startedAt}|]
          pure url

uploadPublicObject :: (Concurrent :> es, IOE :> es, KatipE :> es, FileSystem :> es) => Bool -> Runtime -> Cache.CachedMedia -> Eff es ()
uploadPublicObject False runtime cached =
  case publicObjectUrl runtime.cfg cached of
    Just _ ->
      void (ensurePublicObject runtime cached)
    Nothing ->
      void (storeObject False runtime cached)
uploadPublicObject True runtime cached =
  when runtime.cfg.s3.enabled do
    startedAt <- monotonicMilliseconds
    let cachedFileId = cached.fileId
    uploaded <- storeObject True runtime cached
    for_ uploaded (rememberPublicObject runtime cachedFileId)
    finishedAt <- monotonicMilliseconds
    let outcome
          | isJust uploaded = "forced_upload" :: Text
          | otherwise = "failed"
    logDebug [i|S3 publicize outcome=#{outcome} file_id=#{cachedFileId} duration_ms=#{finishedAt - startedAt}|]

rememberPublicObject :: Concurrent :> es => Runtime -> Text -> Text -> Eff es ()
rememberPublicObject runtime fileId url =
  MVar.modifyMVar_ runtime.publicObjects (pure . LRU.insert fileId url)

monotonicMilliseconds :: IOE :> es => Eff es Integer
monotonicMilliseconds =
  fromIntegral . (`div` 1_000_000) <$> liftIO getMonotonicTimeNSec

storeObject :: (Concurrent :> es, IOE :> es, KatipE :> es, FileSystem :> es) => Bool -> Runtime -> Cache.CachedMedia -> Eff es (Maybe Text)
storeObject forceUpload runtime cached =
  storeObjectUnsafe forceUpload runtime cached `catchSync` \err -> do
    logError [i|S3 media upload skipped: #{show err :: String}|]
    pure Nothing

storeObjectUnsafe :: (Concurrent :> es, IOE :> es, KatipE :> es, FileSystem :> es) => Bool -> Runtime -> Cache.CachedMedia -> Eff es (Maybe Text)
storeObjectUnsafe _ Runtime{cfg, env = Nothing} _
  | not cfg.s3.enabled =
      pure Nothing
  | otherwise =
      liftIO (ioError (userError (Text.unpack (missingS3ConfigMessage cfg.s3))))
storeObjectUnsafe forceUpload Runtime{cfg, env = Just env} cached
  | not cfg.s3.enabled =
      pure Nothing
  | otherwise = do
      ensureS3Config cfg.s3
      if forceUpload
        then do
          uploadObject env cfg.s3 key cached
          pure (publicObjectUrl cfg cached)
        else
          objectExists env cfg.s3 key >>= \case
            True -> do
              logDebug [i|S3 media upload skipped; object already exists: key=#{key}|]
              pure (publicObjectUrl cfg cached)
            False -> do
              uploadObject env cfg.s3 key cached
              pure (publicObjectUrl cfg cached)
  where
    key = objectKey cfg.s3 cached

objectExists :: IOE :> es => AWS.Env -> Config -> Text -> Eff es Bool
objectExists env cfg key = do
  result <- trySync (liftIO $ AWS.runResourceT (AWS.send env request))
  case result of
    Right _ ->
      pure True
    Left err
      | Just (AWS.ServiceError serviceError) <- fromException err
      , serviceError.status == HTTPStatus.status404 ->
          pure False
      | otherwise ->
          throwIO err
  where
    request =
      S3.newHeadObject (S3.BucketName (fromMaybe "" cfg.bucket)) (S3.ObjectKey key)

uploadObject :: (IOE :> es, KatipE :> es, FileSystem :> es) => AWS.Env -> Config -> Text -> Cache.CachedMedia -> Eff es ()
uploadObject env cfg key cached = do
  body <- liftIO (AWS.Hashed <$> AWS.hashedFile cached.path)
  let mime = cached.mimeType
      request =
        setPublicAcl cfg
          ( (S3.newPutObject (S3.BucketName (fromMaybe "" cfg.bucket)) (S3.ObjectKey key) body)
            { PutObject.contentType = Just mime
            }
          )
  logInfo [i|S3 media upload: key=#{key} mime=#{mime}|]
  void $ liftIO $ AWS.runResourceT (AWS.send env request)

setPublicAcl :: Config -> S3.PutObject -> S3.PutObject
setPublicAcl cfg request
  | cfg.publicReadAcl =
      request{PutObject.acl = Just S3.ObjectCannedACL_Public_read}
  | otherwise =
      request

ensureS3Config :: IOE :> es => Config -> Eff es ()
ensureS3Config cfg
  | missingS3ConfigMessage cfg == "" = pure ()
  | otherwise = liftIO (ioError (userError (Text.unpack (missingS3ConfigMessage cfg))))

missingS3ConfigMessage :: Config -> Text
missingS3ConfigMessage cfg =
  case missing of
    [] -> ""
    fields -> "S3 media storage is enabled but missing config keys: " <> Text.intercalate ", " fields
  where
    missing =
      [ name
      | (name, Nothing) <-
          [ ("bucket", cfg.bucket)
          , ("access_key_id", cfg.accessKeyId)
          , ("secret_access_key", cfg.secretAccessKey)
          ]
      ]

objectKey :: Config -> Cache.CachedMedia -> Text
objectKey cfg mediaObject =
  let ext = Cache.extensionFor MediaObject{bytes = Q.empty, mimeType = mediaObject.mimeType, sourceName = mediaObject.sourceName}
      prefix = Text.dropWhileEnd (== '/') cfg.prefix
  in prefix <> "/" <> mediaObject.digest <> ext

publicObjectUrl :: MediaConfig.Config -> Cache.CachedMedia -> Maybe Text
publicObjectUrl cfg cached = do
  base <- cfg.publicBaseUrl
  pure (Text.dropWhileEnd (== '/') base <> "/" <> objectKey cfg.s3 cached)
