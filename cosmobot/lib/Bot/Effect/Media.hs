{-|
Module      : Bot.Effect.Media
Description : Media normalization and object storage capability
Stability   : experimental
-}

module Bot.Effect.Media
  ( Media (..)
  , MediaObject (..)
  , MediaCacheEntry (..)
  , MediaFileInfo (..)
  , MediaPlatformRefInfo (..)
  , MediaCacheStats (..)
  , storeMediaObject
  , storeMediaObjectFromSource
  , mediaRefForSource
  , mediaCacheEntry
  , deleteMediaFile
  , mediaFileInfo
  , mediaFileInfoByRef
  , listMediaFiles
  , mediaCacheStats
  , gcMediaCache
  , normalizeMediaRef
  , normalizeMediaRefs
  , publicMediaRef
  , modelImageRef
  , localMediaPath
  , platformMediaRef
  , storePlatformMediaRef
  , normalizeIncomingMessage
  , normalizeIncomingMessages
  , normalizeReferencedMessage
  , normalizeReplyBody
  , runMediaPassthrough
  )
where

import qualified Bot.Core.ReplyBody as ReplyBody
import Bot.Core.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Set as Set
import qualified Data.Text as Text
import Control.Monad.Trans.Resource (ResourceT)
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as S

data MediaObject = MediaObject
  { bytes :: Q.ByteStream (ResourceT IO) ()
  , mimeType :: !Text
  , sourceName :: !(Maybe Text)
  }

data MediaFileInfo = MediaFileInfo
  { fileId :: !Text
  , ref :: !Text
  , digest :: !Text
  , mimeType :: !Text
  , sourceName :: !(Maybe Text)
  , path :: !FilePath
  , size :: !Int
  , createdAtUnix :: !Int
  , lastUsedAtUnix :: !Int
  , exists :: !Bool
  }
  deriving (Show, Eq, Generic, Aeson.ToJSON)

data MediaPlatformRefInfo = MediaPlatformRefInfo
  { platform :: !Text
  , scope :: !Text
  , platformRef :: !Text
  }
  deriving (Show, Eq, Generic, Aeson.ToJSON)

data MediaCacheEntry = MediaCacheEntry
  { file :: !MediaFileInfo
  , sourceRefs :: ![Text]
  , platformRefs :: ![MediaPlatformRefInfo]
  }
  deriving (Show, Eq, Generic, Aeson.ToJSON)

data MediaCacheStats = MediaCacheStats
  { files :: !Int
  , existingFiles :: !Int
  , missingFiles :: !Int
  , totalBytes :: !Int
  , sources :: !Int
  , platformRefs :: !Int
  }
  deriving (Show, Eq, Generic, Aeson.ToJSON)

data Media :: Effect where
  StoreMediaObject :: MediaObject -> Media m (Maybe Text)
  StoreMediaObjectFromSource :: Text -> MediaObject -> Media m (Maybe Text)
  MediaRefForSource :: Text -> Media m (Maybe Text)
  GetMediaCacheEntry :: Text -> Media m (Maybe MediaCacheEntry)
  DeleteMediaFile :: Text -> Media m Bool
  GetMediaFileInfo :: Text -> Media m (Maybe MediaFileInfo)
  ListMediaFiles :: Media m [MediaFileInfo]
  GetMediaCacheStats :: Media m MediaCacheStats
  GcMediaCache :: Int -> Set.Set Text -> Media m Int
  NormalizeMediaRef :: Text -> Media m Text
  PublicMediaRef :: Text -> Media m Text
  ModelImageRef :: Text -> Media m Text
  LocalMediaPath :: Text -> Media m (Maybe FilePath)
  PlatformMediaRef :: Text -> Text -> Text -> Media m (Maybe Text)
  StorePlatformMediaRef :: Text -> Text -> Text -> Text -> Media m ()

type instance DispatchOf Media = Dynamic

storeMediaObject :: Media :> es => MediaObject -> Eff es (Maybe Text)
storeMediaObject =
  send . StoreMediaObject

storeMediaObjectFromSource :: Media :> es => Text -> MediaObject -> Eff es (Maybe Text)
storeMediaObjectFromSource sourceRef mediaObject =
  send (StoreMediaObjectFromSource sourceRef mediaObject)

mediaRefForSource :: Media :> es => Text -> Eff es (Maybe Text)
mediaRefForSource =
  send . MediaRefForSource

mediaCacheEntry :: Media :> es => Text -> Eff es (Maybe MediaCacheEntry)
mediaCacheEntry =
  send . GetMediaCacheEntry

deleteMediaFile :: Media :> es => Text -> Eff es Bool
deleteMediaFile =
  send . DeleteMediaFile

mediaFileInfo :: Media :> es => Text -> Eff es (Maybe MediaFileInfo)
mediaFileInfo =
  send . GetMediaFileInfo

mediaFileInfoByRef :: Media :> es => Text -> Eff es (Maybe MediaFileInfo)
mediaFileInfoByRef ref =
  case parseMediaId ref of
    Nothing -> pure Nothing
    Just fileId -> mediaFileInfo fileId

listMediaFiles :: Media :> es => Eff es [MediaFileInfo]
listMediaFiles =
  send ListMediaFiles

mediaCacheStats :: Media :> es => Eff es MediaCacheStats
mediaCacheStats =
  send GetMediaCacheStats

gcMediaCache :: Media :> es => Int -> Set.Set Text -> Eff es Int
gcMediaCache maxAgeSeconds retainedFileIds =
  send (GcMediaCache maxAgeSeconds retainedFileIds)

normalizeMediaRef :: Media :> es => Text -> Eff es Text
normalizeMediaRef =
  send . NormalizeMediaRef

publicMediaRef :: Media :> es => Text -> Eff es Text
publicMediaRef =
  send . PublicMediaRef

-- | Resolve an image reference into a representation accepted by chat models.
modelImageRef :: Media :> es => Text -> Eff es Text
modelImageRef =
  send . ModelImageRef

localMediaPath :: Media :> es => Text -> Eff es (Maybe FilePath)
localMediaPath =
  send . LocalMediaPath

platformMediaRef :: Media :> es => Text -> Text -> Text -> Eff es (Maybe Text)
platformMediaRef platform scope ref =
  send (PlatformMediaRef platform scope ref)

storePlatformMediaRef :: Media :> es => Text -> Text -> Text -> Text -> Eff es ()
storePlatformMediaRef platform scope ref platformRef =
  send (StorePlatformMediaRef platform scope ref platformRef)

normalizeMediaRefs :: Media :> es => [Text] -> Eff es [Text]
normalizeMediaRefs =
  traverse normalizeMediaRef

normalizeIncomingMessage :: Media :> es => IncomingMessage -> Eff es IncomingMessage
normalizeIncomingMessage message = do
  imageUrls <- normalizeMediaRefs message.imageUrls
  files <- traverse normalizeMessageFile message.files
  pure IncomingMessage
    { eventKind = message.eventKind
    , platform = message.platform
    , kind = message.kind
    , chatId = message.chatId
    , chatAliases = message.chatAliases
    , digest = message.digest
    , senderId = message.senderId
    , senderUsername = message.senderUsername
    , messageId = message.messageId
    , replyToMessageId = message.replyToMessageId
    , mentions = message.mentions
    , mentionUsernames = message.mentionUsernames
    , imageUrls
    , files
    , text = message.text
    , raw = message.raw
    }

normalizeIncomingMessages
  :: Media :> es
  => Stream (Of IncomingMessage) (Eff es) ()
  -> Stream (Of IncomingMessage) (Eff es) ()
normalizeIncomingMessages =
  S.mapM normalizeIncomingMessage

normalizeReferencedMessage :: Media :> es => ReferencedMessage -> Eff es ReferencedMessage
normalizeReferencedMessage message = do
  imageUrls <- normalizeMediaRefs message.imageUrls
  files <- traverse normalizeMessageFile message.files
  pure ReferencedMessage
    { messageId = message.messageId
    , senderDisplayName = message.senderDisplayName
    , senderIdentifier = message.senderIdentifier
    , senderIsBot = message.senderIsBot
    , text = message.text
    , imageUrls
    , files
    }

normalizeMessageFile :: Media :> es => MessageFile -> Eff es MessageFile
normalizeMessageFile file = do
  ref <- normalizeMediaRef file.ref
  pure MessageFile{name = file.name, ref}

normalizeReplyBody :: Media :> es => Text -> Eff es Text
normalizeReplyBody =
  ReplyBody.traverseReplyImageUrls normalizeMediaRef

runMediaPassthrough :: Eff (Media : es) a -> Eff es a
runMediaPassthrough =
  interpret \_ -> \case
    StoreMediaObject mediaObject ->
      pure (Just ("data:" <> mediaObject.mimeType <> ";base64,"))
    StoreMediaObjectFromSource _ mediaObject ->
      pure (Just ("data:" <> mediaObject.mimeType <> ";base64,"))
    MediaRefForSource _ ->
      pure Nothing
    GetMediaCacheEntry _ ->
      pure Nothing
    DeleteMediaFile _ ->
      pure False
    GetMediaFileInfo _ ->
      pure Nothing
    ListMediaFiles ->
      pure []
    GetMediaCacheStats ->
      pure MediaCacheStats{files = 0, existingFiles = 0, missingFiles = 0, totalBytes = 0, sources = 0, platformRefs = 0}
    GcMediaCache _ _ ->
      pure 0
    NormalizeMediaRef ref ->
      pure ref
    PublicMediaRef ref ->
      pure ref
    ModelImageRef ref ->
      pure ref
    LocalMediaPath _ ->
      pure Nothing
    PlatformMediaRef _ _ _ ->
      pure Nothing
    StorePlatformMediaRef _ _ _ _ ->
      pure ()

parseMediaId :: Text -> Maybe Text
parseMediaId ref = do
  fileId <- Text.stripPrefix "media:" (Text.strip ref)
  guard (not (Text.null fileId))
  pure fileId
