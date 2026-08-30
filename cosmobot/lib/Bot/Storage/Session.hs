{-# LANGUAGE OverloadedLabels #-}
{-|
Module      : Bot.Storage.Session
Description : Durable storage for persistent chat sessions
Stability   : experimental
-}

module Bot.Storage.Session
  ( StoredMediaRef (..)
  , StoredChatMessage (..)
  , StoredChatSession (..)
  , ensureSessionTables
  , createSession
  , listSessions
  , loadSession
  , loadSessionHistory
  , loadMessage
  , appendMessage
  , updateMessageText
  , renameSession
  , deleteSession
  , forkSession
  , nextMessageId
  , messageAttachmentIds
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import Bot.Storage.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Database.Selda.SQLite as SeldaSQLite

data StoredMediaRef = StoredMediaRef
  { attachmentId :: !Text
  , name :: !Text
  , mediaType :: !Text
  , kind :: !Text
  , size :: !Int
  , url :: !Text
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

data StoredChatSession = StoredChatSession
  { sessionId :: !Text
  , label :: !(Maybe Text)
  , parentSessionId :: !(Maybe Text)
  , parentMessageId :: !(Maybe MessageId)
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

data StoredChatMessage = StoredChatMessage
  { sessionId :: !Text
  , messageId :: !MessageId
  , sender :: !Text
  , text :: !Text
  , imageUrls :: ![Text]
  , attachments :: ![StoredMediaRef]
  , replyToMessageId :: !(Maybe MessageId)
  , parentMessageId :: !(Maybe MessageId)
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

data SessionRow = SessionRow
  { id :: ID SessionRow
  , session_id :: Text
  , label :: Maybe Text
  , parent_session_id :: Maybe Text
  , parent_message_id :: Maybe Text
  }
  deriving (Generic)

instance SqlRow SessionRow

data SessionMessageRow = SessionMessageRow
  { id :: ID SessionMessageRow
  , session_id :: Text
  , message_id :: Text
  , sender :: Text
  , body :: Text
  , image_urls_json :: Text
  , reply_to_message_id :: Maybe Text
  , parent_message_id :: Maybe Text
  }
  deriving (Generic)

instance SqlRow SessionMessageRow

data SessionMessageAttachmentRow = SessionMessageAttachmentRow
  { id :: ID SessionMessageAttachmentRow
  , message_id :: Text
  , attachment_id :: Text
  , name :: Text
  , media_type :: Text
  , kind :: Text
  , size_bytes :: Int
  , url :: Text
  }
  deriving (Generic)

instance SqlRow SessionMessageAttachmentRow

sessionRows :: Table SessionRow
sessionRows =
  table "sessions"
    [ #id :- autoPrimary
    , #session_id :- unique
    , #parent_session_id :- index
    , #parent_message_id :- index
    ]

sessionMessageRows :: Table SessionMessageRow
sessionMessageRows =
  table "session_messages"
    [ #id :- autoPrimary
    , #session_id :- index
    , #message_id :- unique
    , #parent_message_id :- index
    ]

sessionMessageAttachmentRows :: Table SessionMessageAttachmentRow
sessionMessageAttachmentRows =
  table "session_message_attachments"
    [ #id :- autoPrimary
    , #message_id :- index
    , #attachment_id :- index
    ]

ensureSessionTables :: Storage.Storage :> es => Eff es ()
ensureSessionTables = do
  runSelda do
    tryCreateTable sessionRows
    tryCreateTable sessionMessageRows
    tryCreateTable sessionMessageAttachmentRows

createSession :: Storage.Storage :> es => Maybe Text -> Eff es StoredChatSession
createSession label = do
  let cleanLabel = label >>= nonEmptyText
      base = fromMaybe "session" cleanLabel
  allocateSession base \sessionId ->
    StoredChatSession{sessionId, label = cleanLabel, parentSessionId = Nothing, parentMessageId = Nothing}

listSessions :: Storage.Storage :> es => Eff es [StoredChatSession]
listSessions = do
  ensureSessionTables
  rows <- runSelda $
    query do
      row <- select sessionRows
      order (row ! #id) ascending
      pure row
  pure (map sessionFromRow rows)

loadSession :: Storage.Storage :> es => Text -> Eff es (Maybe StoredChatSession)
loadSession targetSessionId = do
  ensureSessionTables
  rows <- runSelda $
    query $
      queryLimit 0 1 do
        row <- select sessionRows
        restrict (row ! #session_id .== literal targetSessionId)
        pure row
  pure (sessionFromRow <$> viaNonEmpty head rows)

loadSessionHistory :: Storage.Storage :> es => Text -> Eff es [StoredChatMessage]
loadSessionHistory targetSessionId = do
  current <- loadOwnMessages targetSessionId
  loadSession targetSessionId >>= \case
    Nothing ->
      pure []
    Just session
      | Just parentSession <- session.parentSessionId
      , Just parentMessage <- session.parentMessageId -> do
          parentHistory <- loadSessionHistory parentSession
          pure (takeThrough parentMessage parentHistory <> current)
      | otherwise ->
          pure current

appendMessage
  :: Storage.Storage :> es
  => Text
  -> Text
  -> Text
  -> [Text]
  -> [StoredMediaRef]
  -> Maybe MessageId
  -> Maybe MessageId
  -> Eff es (Either Text (Maybe StoredChatMessage))
appendMessage sessionId sender body imageUrls attachments replyToMessageId parentMessageId =
  retryingStorageWrite do
    ensureSessionTables
    runSelda $
      transaction do
        matchingSessions <- query do
          row <- select sessionRows
          restrict (row ! #session_id .== literal sessionId)
          pure (row ! #session_id)
        case matchingSessions of
          [] ->
            pure (Right Nothing)
          _ -> do
            message <- insertMessageWithAttachments canonicalAttachments
            pure (Right (Just message))
  where
    canonicalAttachments = ordNubOn (.attachmentId) attachments

    insertMessageWithAttachments mediaRefs = do
      rows <- query do
            row <- select sessionMessageRows
            pure (row ! #message_id)
      let nextNumber = Foldable.maximum (0 : map messageNumber rows) + 1
          message = StoredChatMessage
            { sessionId
            , messageId = textMessageId ("session-" <> show nextNumber)
            , sender
            , text = body
            , imageUrls
            , attachments = mediaRefs
            , replyToMessageId
            , parentMessageId
            }
      insert_ sessionMessageRows [messageRow message]
      insert_ sessionMessageAttachmentRows (map (messageAttachmentRow message.messageId) mediaRefs)
      pure message

updateMessageText :: Storage.Storage :> es => Text -> MessageId -> Text -> Eff es Bool
updateMessageText targetSessionId targetMessageId body = do
  existing <- loadMessage targetSessionId targetMessageId
  case existing of
    Nothing ->
      pure False
    Just _ -> do
      runSelda $
        update_ sessionMessageRows
          (\row -> row ! #session_id .== literal targetSessionId .&& row ! #message_id .== literal (messageIdText targetMessageId))
          (\row -> row `with` [#body := literal body])
      pure True

renameSession :: Storage.Storage :> es => Text -> Text -> Eff es (Maybe StoredChatSession)
renameSession targetSessionId newLabel = do
  ensureSessionTables
  runSelda $
    update_ sessionRows
      (\row -> row ! #session_id .== literal targetSessionId)
      (\row -> row `with` [#label := literal (nonEmptyText newLabel)])
  loadSession targetSessionId

deleteSession :: Storage.Storage :> es => Text -> Eff es Bool
deleteSession targetSessionId = do
  ensureSessionTables
  runSelda (transaction (retireSessionTree targetSessionId))

retireSessionTree :: Text -> SeldaT SeldaSQLite.SQLite IO Bool
retireSessionTree targetSessionId = do
  existingSessions <- query do
    row <- select sessionRows
    pure row
  let deleteIds = descendantSessionIds targetSessionId existingSessions
  case deleteIds of
    [] ->
      pure False
    _ -> do
      messageRows <- messagesInSessions deleteIds
      deleteMessagesAndSessions deleteIds messageRows
      pure True

messagesInSessions :: [Text] -> SeldaT SeldaSQLite.SQLite IO [SessionMessageRow]
messagesInSessions sessionIds =
  query do
    row <- select sessionMessageRows
    restrict (row ! #session_id `isIn` map literal sessionIds)
    pure row

deleteMessagesAndSessions :: [Text] -> [SessionMessageRow] -> SeldaT SeldaSQLite.SQLite IO ()
deleteMessagesAndSessions sessionIds messageRows = do
  unless (null messageRows) $
    deleteFrom_ sessionMessageAttachmentRows \row ->
      row ! #message_id `isIn` map (literal . (.message_id)) messageRows
  deleteFrom_ sessionMessageRows \row ->
    row ! #session_id `isIn` map literal sessionIds
  traverse_ deleteSessionRow sessionIds

deleteSessionRow :: Text -> SeldaT SeldaSQLite.SQLite IO ()
deleteSessionRow sessionId =
  deleteFrom_ sessionRows \row ->
    row ! #session_id .== literal sessionId

forkSession :: Storage.Storage :> es => Text -> MessageId -> Maybe Text -> Eff es (Maybe StoredChatSession)
forkSession sourceSessionId sourceMessageId requestedLabel = do
  source <- loadSession sourceSessionId
  sourceMessages <- loadSessionHistory sourceSessionId
  case (source, find ((== sourceMessageId) . (.messageId)) sourceMessages) of
    (Just sourceSession, Just _) -> do
      let base = fromMaybe (sourceLabelBase sourceSession <> "-fork") (requestedLabel >>= nonEmptyText)
      Just <$> allocateSession base \sessionId ->
        StoredChatSession
          { sessionId
          , label = requestedLabel >>= nonEmptyText
          , parentSessionId = Just sourceSessionId
          , parentMessageId = Just sourceMessageId
          }
    _ ->
      pure Nothing

nextMessageId :: Storage.Storage :> es => Eff es MessageId
nextMessageId = do
  ensureSessionTables
  rows <- runSelda $
    query do
      row <- select sessionMessageRows
      pure (row ! #message_id)
  let nextNumber = Foldable.maximum (0 : map messageNumber rows) + 1
  pure (textMessageId ("session-" <> show nextNumber))

loadOwnMessages :: Storage.Storage :> es => Text -> Eff es [StoredChatMessage]
loadOwnMessages targetSessionId = do
  ensureSessionTables
  rows <- runSelda $
    query do
      row <- select sessionMessageRows
      restrict (row ! #session_id .== literal targetSessionId)
      order (row ! #id) ascending
      pure row
  attachRows <- loadAttachmentsForMessages (map (textMessageId . (.message_id)) rows)
  pure [messageFromRow row (Map.findWithDefault [] (textMessageId row.message_id) attachRows) | row <- rows]

loadMessage :: Storage.Storage :> es => Text -> MessageId -> Eff es (Maybe StoredChatMessage)
loadMessage targetSessionId targetMessageId = do
  ensureSessionTables
  rows <- runSelda $
    query $
      queryLimit 0 1 do
        row <- select sessionMessageRows
        restrict (row ! #session_id .== literal targetSessionId .&& row ! #message_id .== literal (messageIdText targetMessageId))
        pure row
  case viaNonEmpty head rows of
    Nothing ->
      pure Nothing
    Just row -> do
      attachRows <- loadAttachmentsForMessages [textMessageId row.message_id]
      pure (Just (messageFromRow row (Map.findWithDefault [] (textMessageId row.message_id) attachRows)))

messageAttachmentIds :: Storage.Storage :> es => Eff es [Text]
messageAttachmentIds = do
  ensureSessionTables
  rows <- runSelda $
    query do
      row <- select sessionMessageAttachmentRows
      pure (row ! #attachment_id)
  pure (ordNub rows)

allocateSession :: Storage.Storage :> es => Text -> (Text -> StoredChatSession) -> Eff es StoredChatSession
allocateSession base mkSession =
  retryingStorageWrite do
    ensureSessionTables
    runSelda $
      transaction do
        rows <- query do
          row <- select sessionRows
          pure (row ! #session_id)
        let session = mkSession (nextSessionId base rows)
        insert_ sessionRows [sessionRow session]
        pure session

retryingStorageWrite :: Storage.Storage :> es => Eff es a -> Eff es a
retryingStorageWrite action =
  go (3 :: Int)
  where
    go attempts = do
      result <- trySync action
      case result of
        Right value ->
          pure value
        Left err
          | attempts > 1
          , retryableStorageWriteFailure err ->
              go (attempts - 1)
          | otherwise ->
              throwIO err

retryableStorageWriteFailure :: SomeException -> Bool
retryableStorageWriteFailure err =
  any (`Text.isInfixOf` message)
    [ "database is locked"
    , "database table is locked"
    , "sqlite_busy"
    , "sqlite_locked"
    ]
  where
    message = Text.toLower (Text.pack (displayException err))

sessionRow :: StoredChatSession -> SessionRow
sessionRow session =
  SessionRow
    { id = def
    , session_id = session.sessionId
    , label = session.label
    , parent_session_id = session.parentSessionId
    , parent_message_id = messageIdText <$> session.parentMessageId
    }

sessionFromRow :: SessionRow -> StoredChatSession
sessionFromRow row =
  StoredChatSession
    { sessionId = row.session_id
    , label = row.label
    , parentSessionId = row.parent_session_id
    , parentMessageId = textMessageId <$> row.parent_message_id
    }

messageRow :: StoredChatMessage -> SessionMessageRow
messageRow message =
  SessionMessageRow
    { id = def
    , session_id = message.sessionId
    , message_id = messageIdText message.messageId
    , sender = message.sender
    , body = message.text
    , image_urls_json = encodeImageUrls message.imageUrls
    , reply_to_message_id = messageIdText <$> message.replyToMessageId
    , parent_message_id = messageIdText <$> message.parentMessageId
    }

messageFromRow :: SessionMessageRow -> [StoredMediaRef] -> StoredChatMessage
messageFromRow row attachments =
  StoredChatMessage
    { sessionId = row.session_id
    , messageId = textMessageId row.message_id
    , sender = row.sender
    , text = row.body
    , imageUrls = decodeImageUrls row.image_urls_json
    , attachments
    , replyToMessageId = textMessageId <$> row.reply_to_message_id
    , parentMessageId = textMessageId <$> row.parent_message_id
    }

encodeImageUrls :: [Text] -> Text
encodeImageUrls =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

decodeImageUrls :: Text -> [Text]
decodeImageUrls value =
  fromMaybe [] (Aeson.decodeStrict' (TextEncoding.encodeUtf8 value))

messageAttachmentRow :: MessageId -> StoredMediaRef -> SessionMessageAttachmentRow
messageAttachmentRow messageId attachment =
  SessionMessageAttachmentRow
    { id = def
    , message_id = messageIdText messageId
    , attachment_id = attachment.attachmentId
    , name = attachment.name
    , media_type = attachment.mediaType
    , kind = attachment.kind
    , size_bytes = attachment.size
    , url = attachment.url
    }

attachmentFromMessageRow :: SessionMessageAttachmentRow -> StoredMediaRef
attachmentFromMessageRow row =
  StoredMediaRef
    { attachmentId = row.attachment_id
    , name = row.name
    , mediaType = row.media_type
    , kind = row.kind
    , size = row.size_bytes
    , url = row.url
    }

loadAttachmentsForMessages :: Storage.Storage :> es => [MessageId] -> Eff es (Map MessageId [StoredMediaRef])
loadAttachmentsForMessages [] =
  pure Map.empty
loadAttachmentsForMessages messageIds = do
  let requested = ordNub (map messageIdText messageIds)
  rows <- runSelda $
    query do
      row <- select sessionMessageAttachmentRows
      restrict (row ! #message_id `isIn` map literal requested)
      order (row ! #id) ascending
      pure row
  pure $
    Map.fromListWith (flip (<>))
      [ (textMessageId row.message_id, [attachmentFromMessageRow row])
      | row <- rows
      ]

takeThrough :: MessageId -> [StoredChatMessage] -> [StoredChatMessage]
takeThrough target =
  go
  where
    go [] =
      []
    go (message : rest)
      | message.messageId == target =
          [message]
      | otherwise =
          message : go rest

descendantSessionIds :: Text -> [SessionRow] -> [Text]
descendantSessionIds rootSessionId rows
  | rootSessionId `elem` map (.session_id) rows =
      go [rootSessionId] []
  | otherwise =
      []
  where
    go [] deleted =
      reverse deleted
    go (sessionId : pending) deleted
      | sessionId `elem` deleted =
          go pending deleted
      | otherwise =
          let children =
                [ row.session_id
                | row <- rows
                , row.parent_session_id == Just sessionId
                ]
          in go (children <> pending) (sessionId : deleted)

nextSessionId :: Text -> [Text] -> Text
nextSessionId base existing =
  let prefix = base <> "-"
      nextNumber = Foldable.maximum (0 : mapMaybe (Text.stripPrefix prefix >=> readMaybe . Text.unpack) existing) + 1
  in prefix <> show (nextNumber :: Integer)

sourceLabelBase :: StoredChatSession -> Text
sourceLabelBase session =
  fromMaybe session.sessionId session.label

messageNumber :: Text -> Integer
messageNumber value =
  fromMaybe 0 do
    (_, suffix) <- Just (Text.breakOnEnd "-" value)
    readMaybe (Text.unpack suffix)

nonEmptyText :: Text -> Maybe Text
nonEmptyText value =
  let stripped = Text.strip value
  in stripped <$ guard (not (Text.null stripped))
