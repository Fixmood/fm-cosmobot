{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import qualified Bot.Concurrency.Manager as ConcurrencyManager
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Effect.Storage as StorageEffect
import qualified Bot.Storage.SQLite as StorageSQLite
import Bot.Core.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Database.Selda as Selda
import qualified Database.Selda.Backend as SeldaBackend
import qualified Database.Selda.SQLite as SeldaSQLite
import Effectful.Timeout (Timeout, runTimeout)
import qualified Streaming.Prelude as S
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "scheduler"
      [ testCase "scheduled messages are scoped by current user" testScheduledMessagesAreScopedByCurrentUser
      , testCase "scheduled messages are scoped by current chat" testScheduledMessagesAreScopedByCurrentChat
      , testCase "username scopes schedules when sender id is absent" testUsernameScopedSchedule
      , testCase "schedule ids increase in insertion order" testScheduleIdsIncrease
      , testCase "scheduled stream yields original message" testScheduledStreamYieldsOriginalMessage
      , testCase "elapsed schedule leaves pending list" testElapsedScheduleLeavesPendingList
      , testCase "same due time yields messages in schedule id order" testSameDueTimeYieldsInScheduleIdOrder
      , testCase "deleted elapsed schedule is not delivered" testDeletedElapsedScheduleIsNotDelivered
      , testCase "pending schedules persist across scheduler restart" testPendingSchedulesPersistAcrossSchedulerRestart
      , testCase "elapsed schedules persist across scheduler restart" testElapsedSchedulesPersistAcrossSchedulerRestart
      , testCase "scheduler migrates old ids to database primary keys" testSchedulerMigratesLegacyIds
      , testCase "storage failures do not commit scheduler memory state" testStorageFailuresDoNotCommitSchedulerState
      ]

data LegacyScheduledMessageRow = LegacyScheduledMessageRow
  { legacy_id :: Selda.RowID
  , legacy_schedule_id :: Int.Int64
  , legacy_due_at_unix_seconds :: Int.Int64
  , legacy_platform_key :: Text
  , legacy_chat_id :: Maybe Int.Int64
  , legacy_sender_id :: Maybe Text
  , legacy_sender_username :: Maybe Text
  , legacy_message_json :: Text
  }
  deriving (Generic)

instance Selda.SqlRow LegacyScheduledMessageRow

legacyScheduledMessages :: Selda.Table LegacyScheduledMessageRow
legacyScheduledMessages =
  Selda.tableFieldMod "scheduled_messages"
    [ #legacy_id Selda.:- Selda.untypedAutoPrimary
    , #legacy_schedule_id Selda.:- Selda.unique
    , #legacy_due_at_unix_seconds Selda.:- Selda.index
    , #legacy_platform_key Selda.:- Selda.index
    , #legacy_chat_id Selda.:- Selda.index
    , #legacy_sender_id Selda.:- Selda.index
    , #legacy_sender_username Selda.:- Selda.index
    ]
    (fromMaybe "" . Text.stripPrefix "legacy_")

testScheduledMessagesAreScopedByCurrentUser :: IO ()
testScheduledMessagesAreScopedByCurrentUser = runSchedulerTest do
  _ <- Scheduler.scheduleMessage 60 (messageFrom "200" "!ask remind me")
  ownSchedules <- Scheduler.listScheduledMessages (messageFrom "200" "what schedules?")
  otherSchedules <- Scheduler.listScheduledMessages (messageFrom "201" "what schedules?")
  liftIO $ length ownSchedules @?= 1
  liftIO $ length otherSchedules @?= 0
  liftIO $ assertBool "remaining seconds are positive" (all ((> 0) . (.remainingSeconds)) ownSchedules)
  liftIO $ assertBool "remaining seconds do not exceed delay" (all ((<= 60) . (.remainingSeconds)) ownSchedules)

testScheduledMessagesAreScopedByCurrentChat :: IO ()
testScheduledMessagesAreScopedByCurrentChat = runSchedulerTest do
  _ <- Scheduler.scheduleMessage 60 (messageFrom "200" "!ask private")
  schedules <- Scheduler.listScheduledMessages (messageFromChat "200" 101 "what schedules?")
  liftIO $ length schedules @?= 0

testUsernameScopedSchedule :: IO ()
testUsernameScopedSchedule = runSchedulerTest do
  _ <- Scheduler.scheduleMessage 60 (messageFromUsername "alice" "!ask by username")
  ownSchedules <- Scheduler.listScheduledMessages (messageFromUsername "alice" "what schedules?")
  otherSchedules <- Scheduler.listScheduledMessages (messageFromUsername "bob" "what schedules?")
  liftIO $ length ownSchedules @?= 1
  liftIO $ length otherSchedules @?= 0

testScheduleIdsIncrease :: IO ()
testScheduleIdsIncrease = runSchedulerTest do
  _ <- Scheduler.scheduleMessage 60 (messageFrom "200" "!ask first")
  _ <- Scheduler.scheduleMessage 60 (messageFrom "200" "!ask second")
  schedules <- Scheduler.listScheduledMessages (messageFrom "200" "what schedules?")
  liftIO $ map (.scheduleId) schedules @?= [1, 2]

testScheduledStreamYieldsOriginalMessage :: IO ()
testScheduledStreamYieldsOriginalMessage = runSchedulerTest do
  let scheduled = messageFrom "200" "!ask now"
  _ <- Scheduler.scheduleMessage 0 scheduled
  delivered <- S.head_ Scheduler.scheduledMessages
  liftIO $ ((.text) <$> delivered) @?= Just scheduled.text

testElapsedScheduleLeavesPendingList :: IO ()
testElapsedScheduleLeavesPendingList = runSchedulerTest do
  _ <- Scheduler.scheduleMessage 0 (messageFrom "200" "!ask now")
  _ <- S.head_ Scheduler.scheduledMessages
  schedules <- Scheduler.listScheduledMessages (messageFrom "200" "what schedules?")
  liftIO $ length schedules @?= 0

testSameDueTimeYieldsInScheduleIdOrder :: IO ()
testSameDueTimeYieldsInScheduleIdOrder = runSchedulerTest do
  _ <- Scheduler.scheduleMessage 0 (messageFrom "200" "!ask first")
  _ <- Scheduler.scheduleMessage 0 (messageFrom "200" "!ask second")
  _ <- Scheduler.scheduleMessage 0 (messageFrom "200" "!ask third")
  firstMessage <- S.head_ Scheduler.scheduledMessages
  secondMessage <- S.head_ Scheduler.scheduledMessages
  thirdMessage <- S.head_ Scheduler.scheduledMessages
  liftIO $ map (fmap (.text)) [firstMessage, secondMessage, thirdMessage] @?= map Just ["!ask first", "!ask second", "!ask third"]

testDeletedElapsedScheduleIsNotDelivered :: IO ()
testDeletedElapsedScheduleIsNotDelivered = runSchedulerTest do
  _ <- Scheduler.scheduleMessage 60 (messageFrom "200" "!ask deleted")
  _ <- Scheduler.scheduleMessage 0 (messageFrom "200" "!ask delivered")
  deleted <- Scheduler.deleteScheduledMessage (messageFrom "200" "delete") 1
  delivered <- S.head_ Scheduler.scheduledMessages
  schedules <- Scheduler.listScheduledMessages (messageFrom "200" "what schedules?")
  liftIO do
    deleted @?= True
    ((.text) <$> delivered) @?= Just "!ask delivered"
    map (.scheduleId) schedules @?= []

testPendingSchedulesPersistAcrossSchedulerRestart :: IO ()
testPendingSchedulesPersistAcrossSchedulerRestart = runSchedulerStorage do
  Scheduler.runScheduler do
    _ <- Scheduler.scheduleMessage 60 (messageFrom "200" "!ask persisted")
    pure ()
  Scheduler.runScheduler do
    schedules <- Scheduler.listScheduledMessages (messageFrom "200" "what schedules?")
    liftIO do
      map (.scheduleId) schedules @?= [1]
      map ((.text) . (.message)) schedules @?= ["!ask persisted"]

testElapsedSchedulesPersistAcrossSchedulerRestart :: IO ()
testElapsedSchedulesPersistAcrossSchedulerRestart = runSchedulerStorage do
  Scheduler.runScheduler do
    _ <- Scheduler.scheduleMessage 0 (messageFrom "200" "!ask after restart")
    pure ()
  Scheduler.runScheduler do
    delivered <- S.head_ Scheduler.scheduledMessages
    schedules <- Scheduler.listScheduledMessages (messageFrom "200" "what schedules?")
    liftIO do
      ((.text) <$> delivered) @?= Just "!ask after restart"
      length schedules @?= 0

testSchedulerMigratesLegacyIds :: IO ()
testSchedulerMigratesLegacyIds = runEff $
  withSQLiteConnection \connection -> do
    let legacyMessage = messageFrom "200" "!ask legacy"
    liftIO $ SeldaBackend.runSeldaT
      (do
        SeldaBackend.withBackend \backend -> liftIO $ void $
          SeldaBackend.runStmt backend
            "CREATE TABLE \"scheduled_messages\"(\n      \"id\" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,\n      \"schedule_id\" BIGINT NOT NULL UNIQUE,\n      \"due_at_unix_seconds\" BIGINT NOT NULL,\n      \"platform_key\" TEXT NOT NULL,\n      \"chat_id\" BIGINT NULL,\n      \"sender_id\" TEXT NULL,\n      \"sender_username\" TEXT NULL,\n      \"message_json\" TEXT NOT NULL,\n      UNIQUE(\"schedule_id\")\n    )"
            []
        Selda.insert_ legacyScheduledMessages
          [ LegacyScheduledMessageRow
              { legacy_id = Selda.def
              , legacy_schedule_id = 7
              , legacy_due_at_unix_seconds = maxBound
              , legacy_platform_key = "telegram"
              , legacy_chat_id = Just 100
              , legacy_sender_id = Just "200"
              , legacy_sender_username = Just "alice"
              , legacy_message_json = encodeMessage legacyMessage
              }
          ])
      connection
    runTimeout $
      runConcurrent $
        runPrim $
          ConcurrencyManager.runConcurrencyManager $
            StorageSQLite.runStorageSQLite connection $
              Scheduler.runScheduler do
                before <- Scheduler.listScheduledMessages legacyMessage
                _ <- Scheduler.scheduleMessage 60 (messageFrom "200" "!ask new")
                migrated <- Scheduler.listScheduledMessages legacyMessage
                liftIO do
                  map (.scheduleId) before @?= [7]
                  map (.scheduleId) migrated @?= [7, 8]

testStorageFailuresDoNotCommitSchedulerState :: IO ()
testStorageFailuresDoNotCommitSchedulerState = runEff $
  withSQLiteConnection \connection ->
    runTimeout $
      runConcurrent $
        runPrim $
          ConcurrencyManager.runConcurrencyManager $
            StorageSQLite.runStorageSQLite connection $
              Scheduler.runScheduler do
                _ <- Scheduler.scheduleMessage 60 (messageFrom "200" "!ask persisted")
                liftIO (SeldaBackend.seldaClose connection)
                failedCreate <- trySync $
                  Scheduler.scheduleMessage 60 (messageFrom "200" "!ask phantom")
                failedDelete <- trySync $
                  Scheduler.deleteScheduledMessage (messageFrom "200" "delete") 1
                schedules <- Scheduler.listScheduledMessages (messageFrom "200" "list")
                liftIO do
                  assertBool "create should report the storage failure" (isLeft failedCreate)
                  assertBool "delete should report the storage failure" (isLeft failedDelete)
                  map (.scheduleId) schedules @?= [1]

runSchedulerTest
  :: Eff '[Scheduler.Scheduler, StorageEffect.Storage, Concurrency.Concurrency, Prim, Concurrent, Timeout, IOE] a
  -> IO a
runSchedulerTest action =
  runSchedulerStorage (Scheduler.runScheduler action)

runSchedulerStorage
  :: Eff '[StorageEffect.Storage, Concurrency.Concurrency, Prim, Concurrent, Timeout, IOE] a
  -> IO a
runSchedulerStorage action =
  runEff $
    ( runTimeout
    . runConcurrent
    . runPrim
    . ConcurrencyManager.runConcurrencyManager
    . StorageSQLite.runStorageSQLitePath ":memory:"
    ) action

encodeMessage :: IncomingMessage -> Text
encodeMessage =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

withSQLiteConnection
  :: (SeldaBackend.SeldaConnection SeldaSQLite.SQLite -> Eff '[IOE] a)
  -> Eff '[IOE] a
withSQLiteConnection =
  bracket
    (liftIO (SeldaSQLite.sqliteOpen ":memory:"))
    (\connection -> void (trySync (liftIO (SeldaBackend.seldaClose connection))))

messageFrom :: Text -> Text -> IncomingMessage
messageFrom senderId text =
  messageFromChat senderId 100 text

messageFromChat :: Text -> Integer -> Text -> IncomingMessage
messageFromChat senderId chatId text =
  IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just chatId
    , chatAliases = []
    , digest = emptyMessageDigest
    , senderId = Just senderId
    , senderUsername = Just "alice"
    , messageId = Just "300"
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , files = []
    , text = text
    , raw = Aeson.Null
    }

messageFromUsername :: Text -> Text -> IncomingMessage
messageFromUsername username text =
  (messageFrom "200" text)
    { senderId = Nothing
    , senderUsername = Just username
    }
