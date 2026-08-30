{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.Chat.Driver.ACP
Description : ACP chat driver implementation
Stability   : experimental
-}

module Bot.Chat.Driver.ACP
  ( AcpChatDriver
  , acpChatDriver
  )
where

import qualified Bot.ACP.Content as ACPContent
import qualified Bot.ACP.State as ACP
import qualified Bot.Chat.Types as Chat
import Bot.Chat.Driver.Types
import Bot.Core.Message
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Bot.Effect.Media as Media
import Bot.Effect.Media (MediaObject (..))
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import qualified Bot.Session as Session
import qualified Bot.Storage.Session as SessionStorage
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified JSONRPC
import qualified Streaming.ByteString as Q
import System.FilePath (takeExtension, takeFileName)

newtype AcpChatDriver = AcpChatDriver
  { acpState :: ACP.AcpState
  }

acpChatDriver :: ACP.AcpState -> AcpChatDriver
acpChatDriver acpState =
  AcpChatDriver{acpState}

instance ChatDriver AcpChatDriver where
  type ChatDriverEffects AcpChatDriver es = (Concurrent :> es, IOE :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)

  driverPlatform _ =
    PlatformACP

  sendReplyMessage driver message body =
    storeReply driver message body

  sendStreamingReplyMessage driver message body =
    storeReply driver message body

  editMessage driver message messageId body = do
    let sessionId = ACP.sessionIdFromMessage message
        text = ReplyBody.renderReplyBody body
    previous <- fmap (.text) <$> SessionStorage.loadMessage (ACP.acpSessionIdText sessionId) messageId
    updated <- SessionStorage.updateMessageText (ACP.acpSessionIdText sessionId) messageId text
    when updated $
      for_ (editedTextChunk previous text) \chunk ->
        ACP.broadcast driver.acpState $
          Aeson.toJSON $
            sessionUpdateNotification sessionId $
              agentMessageChunkUpdate messageId (ACPContent.textContentBlock chunk)
    pure updated

  completeMessageEdit driver message messageId = do
    ACP.notifyPromptComplete driver.acpState (ACP.sessionIdFromMessage message) messageId
    pure True

  messageOutPolicy _ _ =
    pure (Chat.EditableMessage 1200 4000)

storeReply
  :: (Concurrent :> es, IOE :> es, Storage.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)
  => AcpChatDriver
  -> IncomingMessage
  -> Text
  -> Eff es (Either Text MessageId)
storeReply driver message body = do
  let sessionId = ACP.sessionIdFromMessage message
      parentMessageId = message.messageId
  reply <- acpReplyContent body
  stored <- SessionStorage.appendMessage
    (ACP.acpSessionIdText sessionId)
    "assistant"
    reply.text
    reply.imageUrls
    reply.attachments
    parentMessageId
    parentMessageId
  case stored of
    Left err ->
      pure (Left err)
    Right Nothing ->
      pure (Left "ACP reply did not produce a message id.")
    Right (Just storedReply) -> do
      let messageId = storedReply.messageId
      traverse_ (broadcastReplyContent driver sessionId messageId) $
        ACPContent.messageContentBlocks reply.text reply.immediateImageUrls (map Session.storedAttachmentToSession reply.attachments)
      pure (Right messageId)

data AcpReplyContent = AcpReplyContent
  { text :: !Text
  , imageUrls :: ![Text]
  , immediateImageUrls :: ![Text]
  , attachments :: ![SessionStorage.StoredMediaRef]
  }

data AcpReplyImage = AcpReplyImage
  { storageImageUrl :: !(Maybe Text)
  , immediateImageUrl :: !Text
  , storageAttachment :: !(Maybe SessionStorage.StoredMediaRef)
  }

acpReplyContent
  :: (Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => Text
  -> Eff es AcpReplyContent
acpReplyContent body = do
  images <- traverse acpReplyImage (ReplyBody.replyImageUrls body)
  pure AcpReplyContent
    { text = ReplyBody.renderReplyBody body
    , imageUrls = mapMaybe (.storageImageUrl) images
    , immediateImageUrls = map (.immediateImageUrl) images
    , attachments = mapMaybe (.storageAttachment) images
    }

acpReplyImage
  :: (Storage.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => Text
  -> Eff es AcpReplyImage
acpReplyImage ref =
  case Text.stripPrefix "file://" (Text.strip ref) of
    Just pathText -> do
      let path = Text.unpack pathText
      exists <- FileSystem.doesFileExist path
      if exists
        then do
          bytes <- FileSystemByteString.readFile path
          let mimeType = imageMediaType path
              dataRef = dataImageRef mimeType bytes
          mediaRef <- Media.storeMediaObject $
            MediaObject
              { bytes = Q.fromStrict bytes
              , mimeType
              , sourceName = Just (Text.pack (takeFileName path))
              }
          attachment <- storedMediaAttachment mediaRef
          pure AcpReplyImage
            { storageImageUrl = Nothing
            , immediateImageUrl = dataRef
            , storageAttachment = attachment
            }
        else
          pure (linkOnlyImage ref)
    Nothing ->
      case Session.parseMediaId ref of
        Nothing ->
          pure (linkOnlyImage ref)
        Just fileId ->
          Media.mediaFileInfo fileId >>= \case
            Nothing ->
              pure (linkOnlyImage ref)
            Just info -> do
              url <- Media.publicMediaRef info.ref
              immediate <- mediaImmediateImageRef info
              pure AcpReplyImage
                { storageImageUrl = Nothing
                , immediateImageUrl = fromMaybe url immediate
                , storageAttachment = Just (Session.storedMediaRef info url)
                }

storedMediaAttachment
  :: Media.Media :> es
  => Maybe Text
  -> Eff es (Maybe SessionStorage.StoredMediaRef)
storedMediaAttachment mediaRef =
  case mediaRef >>= Session.parseMediaId of
    Nothing ->
      pure Nothing
    Just fileId ->
      Media.mediaFileInfo fileId >>= \case
        Nothing ->
          pure Nothing
        Just info -> do
          url <- Media.publicMediaRef info.ref
          pure (Just (Session.storedMediaRef info url))

mediaImmediateImageRef
  :: (FileSystem.FileSystem :> es, IOE :> es)
  => Media.MediaFileInfo
  -> Eff es (Maybe Text)
mediaImmediateImageRef info
  | info.exists && "image/" `Text.isPrefixOf` Text.toLower info.mimeType = do
      bytes <- FileSystemByteString.readFile info.path
      pure (Just (dataImageRef info.mimeType bytes))
  | otherwise =
      pure Nothing

linkOnlyImage :: Text -> AcpReplyImage
linkOnlyImage ref =
  AcpReplyImage
    { storageImageUrl = Just ref
    , immediateImageUrl = ref
    , storageAttachment = Nothing
    }

dataImageRef :: Text -> ByteString -> Text
dataImageRef mimeType bytes =
  "data:" <> mimeType <> ";base64," <> TextEncoding.decodeUtf8 (Base64.encode bytes)

broadcastReplyContent
  :: Concurrent :> es
  => AcpChatDriver
  -> ACP.AcpSessionId
  -> MessageId
  -> ACPContent.AcpContentBlock
  -> Eff es ()
broadcastReplyContent driver sessionId messageId content =
  ACP.broadcast driver.acpState $
    Aeson.toJSON $
      sessionUpdateNotification sessionId $
        agentMessageChunkUpdate messageId content

editedTextChunk :: Maybe Text -> Text -> Maybe Text
editedTextChunk previous text =
  case previous >>= (`Text.stripPrefix` text) of
    Just suffix ->
      nonEmptyTextChunk suffix
    Nothing ->
      nonEmptyTextChunk text
  where
    nonEmptyTextChunk value =
      guard (not (Text.null value)) $> value

sessionUpdateNotification :: ACP.AcpSessionId -> Aeson.Value -> JSONRPC.JSONRPCMessage
sessionUpdateNotification sessionId update =
  JSONRPC.NotificationMessage $
    JSONRPC.JSONRPCNotification
      JSONRPC.rPC_VERSION
      "session/update"
      ( Aeson.object
          [ "sessionId" Aeson..= ACP.acpSessionIdText sessionId
          , "update" Aeson..= update
          ]
      )

agentMessageChunkUpdate :: MessageId -> ACPContent.AcpContentBlock -> Aeson.Value
agentMessageChunkUpdate messageId content =
  Aeson.object
    [ "sessionUpdate" Aeson..= ("agent_message_chunk" :: Text)
    , "messageId" Aeson..= messageId
    , "content" Aeson..= ACPContent.contentBlockValue content
    ]

imageMediaType :: FilePath -> Text
imageMediaType path =
  case Text.toLower (Text.pack (takeExtension path)) of
    ".avif" -> "image/avif"
    ".gif" -> "image/gif"
    ".jpeg" -> "image/jpeg"
    ".jpg" -> "image/jpeg"
    ".png" -> "image/png"
    ".webp" -> "image/webp"
    _ -> "application/octet-stream"
