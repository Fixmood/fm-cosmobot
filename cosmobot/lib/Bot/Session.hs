{-# LANGUAGE TypeFamilies #-}
{-
Module      : Bot.Session
Description : Persistent chat session lifecycle
Stability   : experimental
-}

module Bot.Session
  ( SessionId (..)
  , sessionIdText
  , SessionMessage (..)
  , Session (..)
  , SessionAttachmentRef (..)
  , SessionSend (..)
  , openSession
  , listSessions
  , getSession
  , sessionHistory
  , forkSession
  , renameSession
  , deleteSession
  , appendUserMessage
  , storedSessionToSession
  , storedMessageToSession
  , storedAttachmentToSession
  , storedMediaRef
  , parseMediaId
  , sessionSendLlmImageUrls
  , sessionSendContextText
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Storage as StorageEffect
import Bot.Prelude
import qualified Bot.Storage.Session as Storage
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString

newtype SessionId = SessionId { unSessionId :: Text }
  deriving (Eq, Ord, Show)
    deriving (Aeson.ToJSON, Aeson.FromJSON) via Text

data SessionMessage = SessionMessage
  { sessionId :: !SessionId
  , messageId :: !MessageId
  , sender :: !Text
  , text :: !Text
  , imageUrls :: ![Text]
  , attachments :: ![SessionAttachmentRef]
  , replyToMessageId :: !(Maybe MessageId)
  , parentMessageId :: !(Maybe MessageId)
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

data Session = Session
  { sessionId :: !SessionId
  , label :: !(Maybe Text)
  , parentSessionId :: !(Maybe SessionId)
  , parentMessageId :: !(Maybe MessageId)
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

data SessionAttachmentRef = SessionAttachmentRef
  { attachmentId :: !Text
  , name :: !Text
  , mediaType :: !Text
  , kind :: !Text
  , size :: !Int
  , url :: !Text
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

instance Aeson.FromJSON SessionAttachmentRef where
  parseJSON =
    Aeson.withObject "session attachment" \o -> do
      attachmentId <- o Aeson..: "attachmentId" <|> o Aeson..: "attachment_id" <|> o Aeson..: "id"
      name <- fromMaybe attachmentId <$> o Aeson..:? "name"
      mediaType <- fromMaybe "application/octet-stream" <$> (o Aeson..:? "mediaType" >>= \case
        Just value -> pure (Just value)
        Nothing -> o Aeson..:? "media_type")
      kind <- fromMaybe (kindFromMediaType mediaType) <$> o Aeson..:? "kind"
      size <- fromMaybe 0 <$> o Aeson..:? "size"
      url <- fromMaybe attachmentId <$> o Aeson..:? "url"
      pure SessionAttachmentRef{attachmentId, name, mediaType, kind, size, url}

data SessionSend = SessionSend
  { sessionId :: !SessionId
  , text :: !Text
  , imageUrls :: ![Text]
  , attachments :: ![SessionAttachmentRef]
  , replyToMessageId :: !(Maybe MessageId)
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

sessionIdText :: SessionId -> Text
sessionIdText (SessionId value) =
  value

openSession :: StorageEffect.Storage :> es => Maybe Text -> Eff es Session
openSession label =
  storedSessionToSession <$> Storage.createSession label

listSessions :: StorageEffect.Storage :> es => Eff es [Session]
listSessions =
  map storedSessionToSession <$> Storage.listSessions

getSession :: StorageEffect.Storage :> es => SessionId -> Eff es (Maybe Session)
getSession sessionId =
  fmap storedSessionToSession <$> Storage.loadSession (sessionIdText sessionId)

sessionHistory :: StorageEffect.Storage :> es => SessionId -> Eff es [SessionMessage]
sessionHistory sessionId =
  map storedMessageToSession <$> Storage.loadSessionHistory (sessionIdText sessionId)

forkSession :: StorageEffect.Storage :> es => SessionId -> MessageId -> Maybe Text -> Eff es (Maybe Session)
forkSession sourceSessionId messageId label =
  fmap storedSessionToSession <$> Storage.forkSession (sessionIdText sourceSessionId) messageId label

renameSession :: StorageEffect.Storage :> es => SessionId -> Text -> Eff es (Maybe Session)
renameSession sessionId label =
  fmap storedSessionToSession <$> Storage.renameSession (sessionIdText sessionId) label

deleteSession :: (StorageEffect.Storage :> es, FileSystem.FileSystem :> es) => SessionId -> Eff es Bool
deleteSession sessionId =
  Storage.deleteSession (sessionIdText sessionId)

appendUserMessage
  :: (StorageEffect.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => SessionSend
  -> Eff es (Either Text (Maybe SessionMessage))
appendUserMessage sessionSend = do
  attachments <- resolveMediaRefs (map (.attachmentId) sessionSend.attachments)
  case attachments of
    Left err ->
      pure (Left err)
    Right mediaRefs ->
      fmap (fmap storedMessageToSession) <$> Storage.appendMessage
        (sessionIdText sessionSend.sessionId)
        "user"
        sessionSend.text
        sessionSend.imageUrls
        mediaRefs
        sessionSend.replyToMessageId
        sessionSend.replyToMessageId

storedSessionToSession :: Storage.StoredChatSession -> Session
storedSessionToSession session =
  Session
    { sessionId = SessionId session.sessionId
    , label = session.label
    , parentSessionId = SessionId <$> session.parentSessionId
    , parentMessageId = session.parentMessageId
    }

storedMessageToSession :: Storage.StoredChatMessage -> SessionMessage
storedMessageToSession message =
  SessionMessage
    { sessionId = SessionId message.sessionId
    , messageId = message.messageId
    , sender = message.sender
    , text = message.text
    , imageUrls = message.imageUrls
    , attachments = map storedAttachmentToSession message.attachments
    , replyToMessageId = message.replyToMessageId
    , parentMessageId = message.parentMessageId
    }

storedAttachmentToSession :: Storage.StoredMediaRef -> SessionAttachmentRef
storedAttachmentToSession attachment =
  SessionAttachmentRef
    { attachmentId = attachment.attachmentId
    , name = attachment.name
    , mediaType = attachment.mediaType
    , kind = attachment.kind
    , size = attachment.size
    , url = attachment.url
    }

storedMediaRef :: Media.MediaFileInfo -> Text -> Storage.StoredMediaRef
storedMediaRef media url =
  Storage.StoredMediaRef
    { attachmentId = media.ref
    , name = fromMaybe media.fileId media.sourceName
    , mediaType = media.mimeType
    , kind = kindFromMediaType media.mimeType
    , size = media.size
    , url
    }

resolveMediaRefs
  :: Media.Media :> es
  => [Text]
  -> Eff es (Either Text [Storage.StoredMediaRef])
resolveMediaRefs =
  fmap sequence . traverse resolveMediaRef . ordNub
  where
    resolveMediaRef ref =
      case parseMediaId ref of
        Nothing ->
          pure (Left [i|Unknown media ref: #{ref}|])
        Just _ ->
          Media.mediaFileInfoByRef ref >>= \case
            Nothing ->
              pure (Left [i|Unknown media ref: #{ref}|])
            Just media -> do
              url <- Media.publicMediaRef media.ref
              pure (Right (storedMediaRef media url))

parseMediaId :: Text -> Maybe Text
parseMediaId ref = do
  fileId <- Text.stripPrefix "media:" (Text.strip ref)
  guard (not (Text.null fileId))
  pure fileId

sessionSendLlmImageUrls
  :: (StorageEffect.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => SessionSend
  -> Eff es [Text]
sessionSendLlmImageUrls sessionSend = do
  attachmentRefs <- catMaybes <$> traverse attachmentDataImageRef (filter ((== "image") . (.kind)) sessionSend.attachments)
  pure (ordNub (filter isDirectLlmImageRef sessionSend.imageUrls <> attachmentRefs))

attachmentDataImageRef :: (StorageEffect.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es) => SessionAttachmentRef -> Eff es (Maybe Text)
attachmentDataImageRef attachment =
  case parseMediaId attachment.attachmentId of
    Nothing ->
      pure Nothing
    Just fileId ->
      Media.mediaFileInfo fileId >>= \case
        Nothing ->
          pure Nothing
        Just stored ->
          if stored.exists && "image/" `Text.isPrefixOf` Text.toLower stored.mimeType
            then do
              bytes <- FileSystemByteString.readFile stored.path
              pure (Just (dataImageRef stored.mimeType bytes))
            else
              pure Nothing

dataImageRef :: Text -> ByteString -> Text
dataImageRef mediaType bytes =
  "data:" <> mediaType <> ";base64," <> TextEncoding.decodeUtf8 (Base64.encode bytes)

isDirectLlmImageRef :: Text -> Bool
isDirectLlmImageRef ref =
  let stripped = Text.toLower (Text.strip ref)
  in "https://" `Text.isPrefixOf` stripped || "data:image/" `Text.isPrefixOf` stripped

sessionSendContextText :: SessionSend -> Text
sessionSendContextText sessionSend
  | null nonImageAttachments =
      sessionSend.text
  | Text.null (Text.strip sessionSend.text) =
      attachmentContext
  | otherwise =
      sessionSend.text <> "\n\n" <> attachmentContext
  where
    nonImageAttachments =
      filter ((/= "image") . (.kind)) sessionSend.attachments
    attachmentContext =
      Text.unlines $
        "Attachments:" :
        [ "- " <> attachment.name <> " (" <> attachment.mediaType <> ", " <> attachment.url <> ")"
        | attachment <- nonImageAttachments
        ]

kindFromMediaType :: Text -> Text
kindFromMediaType mediaType
  | "image/" `Text.isPrefixOf` media = "image"
  | "audio/" `Text.isPrefixOf` media = "audio"
  | otherwise = "file"
  where
    media = Text.toLower mediaType
