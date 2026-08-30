{-# LANGUAGE OverloadedLabels #-}
{-|
Module      : Bot.Storage.Scheduler
Description : Persistent scheduler queue
Stability   : experimental
-}

module Bot.Storage.Scheduler
  ( StoredScheduledMessage (..)
  , loadScheduledMessages
  , createScheduledMessage
  , deleteScheduledMessage
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import Bot.Storage.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Int as Int
import qualified Data.Text.Encoding as TextEncoding
import qualified Database.Selda.Backend as SeldaBackend
import qualified Database.Selda.SQLite as SeldaSQLite

data StoredScheduledMessage = StoredScheduledMessage
  { scheduleId :: !Integer
  -- | Absolute due time as Unix epoch seconds.
  --
  -- This is deliberately not a local timestamp string, so persisted schedules
  -- survive timezone and daylight-saving changes without reinterpretation.
  , dueAtUnixSeconds :: !Integer
  , message :: !IncomingMessage
  }
  deriving (Show)

data ScheduledMessageRow = ScheduledMessageRow
  { id :: RowID
  , due_at_unix_seconds :: Int.Int64
  , platform_key :: Text
  , chat_id :: Maybe Int.Int64
  , sender_id :: Maybe Text
  , sender_username :: Maybe Text
  , message_json :: Text
  }
  deriving (Generic)

instance SqlRow ScheduledMessageRow

scheduledMessages :: Table ScheduledMessageRow
scheduledMessages =
  table "scheduled_messages"
    [ #id :- untypedAutoPrimary
    , #due_at_unix_seconds :- index
    , #platform_key :- index
    , #chat_id :- index
    , #sender_id :- index
    , #sender_username :- index
    ]

loadScheduledMessages :: Storage.Storage :> es => Eff es [StoredScheduledMessage]
loadScheduledMessages = do
  ensureScheduledMessagesTable
  rows <- runSelda $
    query do
      row <- select scheduledMessages
      order (row ! #due_at_unix_seconds) ascending
      order (row ! #id) ascending
      pure row
  pure (mapMaybe storedScheduledMessageFromRow rows)

createScheduledMessage :: Storage.Storage :> es => Integer -> IncomingMessage -> Eff es StoredScheduledMessage
createScheduledMessage dueAtUnixSeconds message = do
  ensureScheduledMessagesTable
  scheduleId <- fromIntegral . fromId <$> runSelda
    ( insertWithPK
      scheduledMessages
      [ ScheduledMessageRow
          { id = def
          , due_at_unix_seconds = fromIntegral dueAtUnixSeconds
          , platform_key = chatPlatformKey message.platform
          , chat_id = fromIntegral <$> message.chatId
          , sender_id = message.senderId
          , sender_username = message.senderUsername
          , message_json = encodeMessage message
          }
      ]
    )
  pure StoredScheduledMessage{scheduleId, dueAtUnixSeconds, message}

deleteScheduledMessage :: Storage.Storage :> es => Integer -> Eff es ()
deleteScheduledMessage scheduleId = do
  ensureScheduledMessagesTable
  runSelda $
    deleteFrom_ scheduledMessages \row ->
      row ! #id .== literal (toRowId (fromIntegral scheduleId :: Int.Int64))

ensureScheduledMessagesTable :: Storage.Storage :> es => Eff es ()
ensureScheduledMessagesTable =
  runSelda (transaction migrateScheduledMessagesTable)

migrateScheduledMessagesTable :: SeldaT SeldaSQLite.SQLite IO ()
migrateScheduledMessagesTable =
  SeldaBackend.withBackend \backend -> liftIO do
    runStatement backend [i|CREATE TABLE IF NOT EXISTS scheduled_messages (#{scheduledMessageColumns})|]
    (_, rows) <- SeldaBackend.runStmt backend "PRAGMA table_info(scheduled_messages)" []
    let columns = [name | _ : SeldaBackend.SqlString name : _ <- rows]
    when ("schedule_id" `elem` columns) do
      runStatement backend "DROP TABLE IF EXISTS scheduled_messages_new"
      runStatement backend [i|CREATE TABLE scheduled_messages_new (#{scheduledMessageColumns})|]
      runStatement backend "INSERT INTO scheduled_messages_new (id, due_at_unix_seconds, platform_key, chat_id, sender_id, sender_username, message_json) SELECT schedule_id, due_at_unix_seconds, platform_key, chat_id, sender_id, sender_username, message_json FROM scheduled_messages"
      runStatement backend "DROP TABLE scheduled_messages"
      runStatement backend "ALTER TABLE scheduled_messages_new RENAME TO scheduled_messages"
    traverse_ (runStatement backend)
      [ "CREATE INDEX IF NOT EXISTS scheduled_messages_due_at_idx ON scheduled_messages(due_at_unix_seconds)"
      , "CREATE INDEX IF NOT EXISTS scheduled_messages_platform_idx ON scheduled_messages(platform_key)"
      , "CREATE INDEX IF NOT EXISTS scheduled_messages_chat_idx ON scheduled_messages(chat_id)"
      , "CREATE INDEX IF NOT EXISTS scheduled_messages_sender_idx ON scheduled_messages(sender_id)"
      , "CREATE INDEX IF NOT EXISTS scheduled_messages_username_idx ON scheduled_messages(sender_username)"
      ]
  where
    runStatement backend statement =
      void (SeldaBackend.runStmt backend statement [])

scheduledMessageColumns :: Text
scheduledMessageColumns =
  "id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, due_at_unix_seconds BIGINT NOT NULL, platform_key TEXT NOT NULL, chat_id BIGINT NULL, sender_id TEXT NULL, sender_username TEXT NULL, message_json TEXT NOT NULL"

storedScheduledMessageFromRow :: ScheduledMessageRow -> Maybe StoredScheduledMessage
storedScheduledMessageFromRow row = do
  message <- decodeMessage row.message_json
  pure StoredScheduledMessage
    { scheduleId = fromIntegral (fromRowId row.id)
    , dueAtUnixSeconds = fromIntegral row.due_at_unix_seconds
    , message
    }

encodeMessage :: IncomingMessage -> Text
encodeMessage =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

decodeMessage :: Text -> Maybe IncomingMessage
decodeMessage =
  either (const Nothing) Just . Aeson.eitherDecodeStrict' . TextEncoding.encodeUtf8
