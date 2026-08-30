{-|
Module      : Bot.Handler.Media
Description : Commands for inspecting cached media
Stability   : experimental
-}
module Bot.Handler.Media
  ( mediaHandlers
  )
where

import Bot.Core.Message
import Bot.Core.Route
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Media as Media
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

mediaHandlers :: (Chat.Chat :> es, Media.Media :> es) => [RouteHandler es]
mediaHandlers =
  [ withHelp (RouteHelp "!media/info <media_id>..." "Show cached media information.") $
      stopOn (command "!media/info") handleInfo
  , withHelp (RouteHelp "!media/url <media_id>..." "Get public URLs for cached media.") $
      stopOn (command "!media/url") handleUrl
  , withHelp (RouteHelp "!media/get <url>..." "Download URLs into the media cache (superuser only).") $
      requireAuth isSuperuser (\message -> reply message "Only superusers can download media.") $
        stopOn (command "!media/get") handleGet
  ]

handleInfo :: (Chat.Chat :> es, Media.Media :> es) => IncomingMessage -> Text -> Eff es ()
handleInfo message input =
  resolveMediaIds "!media/info" message input >>= either (reply message) (replyWithMediaInfo message)

resolveMediaIds
  :: Chat.Chat :> es
  => Text
  -> IncomingMessage
  -> Text
  -> Eff es (Either Text [Text])
resolveMediaIds commandName message input
  | mediaIds@(_ : _) <- ordNub (Text.words input) =
      pure (Right mediaIds)
  | Just messageId <- message.replyToMessageId =
      maybe (Left "Could not read the replied message.") referencedMediaIdsResult
        <$> Chat.getMessageContent message messageId
  | otherwise =
      pure (Left [i|Usage: #{commandName} <media_id>..., or reply to a message containing media.|])

referencedMediaIdsResult :: ReferencedMessage -> Either Text [Text]
referencedMediaIdsResult referenced =
  maybe (Left "No media ids found in the replied message.") (Right . toList)
    (viaNonEmpty id (referencedMediaIds referenced))

handleUrl :: (Chat.Chat :> es, Media.Media :> es) => IncomingMessage -> Text -> Eff es ()
handleUrl message input =
  resolveMediaIds "!media/url" message input >>= \case
    Left err -> reply message err
    Right mediaIds -> traverse mediaPublicUrl mediaIds >>= reply message . Text.intercalate "\n"

handleGet :: (Chat.Chat :> es, Media.Media :> es) => IncomingMessage -> Text -> Eff es ()
handleGet message input =
  case ordNub (Text.words input) of
    [] ->
      reply message "Usage: !media/get <url>..."
    urls ->
      traverse (cacheUrl message) urls >>= reply message . jsonText

cacheUrl :: (Chat.Chat :> es, Media.Media :> es) => IncomingMessage -> Text -> Eff es Aeson.Value
cacheUrl message url
  | not (isRemoteUrl url) =
      pure (cacheFailure url "expected an HTTP, HTTPS, or MXC URL")
  | otherwise = do
      result <- trySync do
        normalized <- Chat.normalizeMediaRef message url >>= Media.normalizeMediaRef
        case mediaIdFromRef normalized of
          Nothing ->
            pure Nothing
          Just mediaId -> do
            publicUrl <- Media.publicMediaRef normalized
            pure (Just (mediaId, publicUrl))
      pure $ case result of
        Left err ->
          cacheFailure url (toText (displayException err))
        Right cached ->
          maybe
            (cacheFailure url "download or cache failed")
            (\(mediaId, publicUrl) -> Aeson.object
              [ "url" Aeson..= url
              , "media_id" Aeson..= mediaId
              , "public_url" Aeson..= publicUrl
              ])
            cached

isRemoteUrl :: Text -> Bool
isRemoteUrl url =
  let normalized = Text.toLower (Text.strip url)
  in any (`Text.isPrefixOf` normalized) ["http://", "https://", "mxc://"]

cacheFailure :: Text -> Text -> Aeson.Value
cacheFailure url err =
  Aeson.object
    [ "url" Aeson..= url
    , "error" Aeson..= err
    ]

replyWithMediaInfo :: (Chat.Chat :> es, Media.Media :> es) => IncomingMessage -> [Text] -> Eff es ()
replyWithMediaInfo message mediaIds =
  traverse mediaInfo mediaIds >>= reply message . jsonText

referencedMediaIds :: ReferencedMessage -> [Text]
referencedMediaIds referenced =
  ordNub $
    mapMaybe mediaIdFromRef
      (referenced.imageUrls <> map (.ref) referenced.files)
      <> mediaIdsInText referenced.text

mediaIdFromRef :: Text -> Maybe Text
mediaIdFromRef ref = do
  fileId <- Text.stripPrefix "media:" (Text.strip ref)
  guard (validMediaFileId fileId)
  pure ("media:" <> fileId)

mediaIdsInText :: Text -> [Text]
mediaIdsInText =
  mapMaybe (mediaIdFromRef . ("media:" <>))
    . drop 1
    . map (Text.takeWhile mediaIdChar)
    . Text.splitOn "media:"

validMediaFileId :: Text -> Bool
validMediaFileId fileId =
  "mf_" `Text.isPrefixOf` fileId && Text.all mediaIdChar fileId

mediaIdChar :: Char -> Bool
mediaIdChar char =
  (Char.isAscii char && Char.isAlphaNum char) || char == '-' || char == '_'

mediaInfo :: Media.Media :> es => Text -> Eff es Aeson.Value
mediaInfo rawMediaId = do
  let (mediaId, fileId) = normalizeMediaId rawMediaId
  Media.mediaCacheEntry fileId <&> \case
    Nothing -> notFound mediaId
    Just info -> Aeson.object
      [ "media_id" Aeson..= mediaId
      , "info" Aeson..= safeMediaInfo info
      ]

safeMediaInfo :: Media.MediaCacheEntry -> Aeson.Value
safeMediaInfo info =
  Aeson.object
    [ "file" Aeson..= Aeson.object
        [ "fileId" Aeson..= info.file.fileId
        , "ref" Aeson..= info.file.ref
        , "digest" Aeson..= info.file.digest
        , "mimeType" Aeson..= info.file.mimeType
        , "sourceName" Aeson..= info.file.sourceName
        , "size" Aeson..= info.file.size
        , "createdAtUnix" Aeson..= info.file.createdAtUnix
        , "lastUsedAtUnix" Aeson..= info.file.lastUsedAtUnix
        , "exists" Aeson..= info.file.exists
        ]
    , "sourceRefs" Aeson..= info.sourceRefs
    , "platformRefs" Aeson..= info.platformRefs
    ]

mediaPublicUrl :: Media.Media :> es => Text -> Eff es Text
mediaPublicUrl rawMediaId = do
  let (mediaId, fileId) = normalizeMediaId rawMediaId
  Media.mediaCacheEntry fileId >>= \case
    Nothing ->
      pure [i|- #{mediaId}: not found|]
    Just info -> do
      publicUrl <- Media.publicMediaRef info.file.ref
      pure [i|- #{mediaId}: #{publicUrl}|]

normalizeMediaId :: Text -> (Text, Text)
normalizeMediaId rawMediaId =
  let stripped = Text.strip rawMediaId
      fileId = fromMaybe stripped (Text.stripPrefix "media:" stripped)
  in ("media:" <> fileId, fileId)

notFound :: Text -> Aeson.Value
notFound mediaId =
  Aeson.object
    [ "media_id" Aeson..= mediaId
    , "error" Aeson..= Aeson.String "not found"
    ]

jsonText :: Aeson.ToJSON a => a -> Text
jsonText value =
  "```json\n"
    <> TextEncoding.decodeUtf8 (LazyByteString.toStrict (AesonPretty.encodePretty value))
    <> "\n```"

reply :: Chat.Chat :> es => IncomingMessage -> Text -> Eff es ()
reply message =
  void . Chat.replyTo message
