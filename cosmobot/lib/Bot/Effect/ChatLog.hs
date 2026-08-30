{-|
Module      : Bot.Effect.ChatLog
Description : Chat log capability facade
Stability   : experimental
-}

module Bot.Effect.ChatLog
  ( ChatLog
  , ChatLogEntry (..)
  , SenderChatLogScope (..)
  , ChatLogTimeRange (..)
  , unboundedChatLogTimeRange
  , recordMessage
  , recordSelfMessage
  , recordIncomingMessages
  , queryChat
  , lookupMessage
  , queryCurrentSenderChatLog
  , runChatLog
  )
where

import Bot.ChatLog.Record
import Bot.ChatLog.Types
import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Storage.ChatLog as ChatLogStorage
import qualified Effectful.Timeout as Timeout
import qualified Streaming.Prelude as S

chatLogRecordTimeoutMicroseconds :: Int
chatLogRecordTimeoutMicroseconds =
  1_000_000

-- | Append-only chat log used by agent tools for local context.
data ChatLog :: Effect where
  RecordMessage
    :: IncomingMessage
    -> ChatLog m ()
  RecordSelfMessage
    :: IncomingMessage
    -> Text
    -> ChatLog m ()
  QueryChat
    :: IncomingMessage
    -> Maybe Text
    -> Int
    -> Bool
    -> ChatLogTimeRange
    -> ChatLog m [ChatLogEntry]
  LookupMessage
    :: IncomingMessage
    -> MessageId
    -> ChatLog m (Maybe ChatLogEntry)
  QueryCurrentSenderChatLog
    :: IncomingMessage
    -> SenderChatLogScope
    -> [[Text]]
    -> Int
    -> ChatLogTimeRange
    -> ChatLog m [ChatLogEntry]

type instance DispatchOf ChatLog = Dynamic

-- | Record a user/platform message.
recordMessage :: ChatLog :> es => IncomingMessage -> Eff es ()
recordMessage message =
  send (RecordMessage message)

-- | Record every incoming message passing through a stream.
recordIncomingMessages
  :: (ChatLog :> es, KatipE :> es, Timeout.Timeout :> es)
  => Stream (Of IncomingMessage) (Eff es) ()
  -> Stream (Of IncomingMessage) (Eff es) ()
recordIncomingMessages =
  S.mapM \message -> do
    when (message.eventKind == IncomingMessageCreated) $
      Timeout.timeout chatLogRecordTimeoutMicroseconds (recordMessage message) >>= \case
        Just () ->
          pure ()
        Nothing ->
          logWarning [i|chat log record timed out; continuing route dispatch: #{incomingMessageLogLine message}|]
    pure message

-- | Record a logical self reply in the same chat as its triggering message.
recordSelfMessage :: ChatLog :> es => IncomingMessage -> Text -> Eff es ()
recordSelfMessage context body =
  send (RecordSelfMessage context body)

-- | Query recent messages from the current chat in chronological order.
queryChat :: ChatLog :> es => IncomingMessage -> Maybe Text -> Int -> Bool -> ChatLogTimeRange -> Eff es [ChatLogEntry]
queryChat message sender limit includeBotMessages timeRange =
  send (QueryChat message sender limit includeBotMessages timeRange)

-- | Look up one persisted message in the current platform/chat scope.
lookupMessage :: ChatLog :> es => IncomingMessage -> MessageId -> Eff es (Maybe ChatLogEntry)
lookupMessage message messageId =
  send (LookupMessage message messageId)

-- | Query current sender's messages in the requested scope, newest first.
queryCurrentSenderChatLog :: ChatLog :> es => IncomingMessage -> SenderChatLogScope -> [[Text]] -> Int -> ChatLogTimeRange -> Eff es [ChatLogEntry]
queryCurrentSenderChatLog message scope keywords limit timeRange =
  send (QueryCurrentSenderChatLog message scope keywords limit timeRange)

-- | Interpret chat logging through the storage capability.
runChatLog
  :: (IOE :> es, KatipE :> es, Storage.Storage :> es)
  => Eff (ChatLog : es) a
  -> Eff es a
runChatLog inner =
  interpret
    (\_ -> \case
      RecordMessage message ->
        ChatLogStorage.persistRecord (userRecord message)
      RecordSelfMessage context body ->
        ChatLogStorage.persistRecord (selfRecord context body)
      QueryChat message sender limit includeBotMessages timeRange ->
        ChatLogStorage.queryStored message sender limit includeBotMessages timeRange
      LookupMessage message messageId ->
        ChatLogStorage.lookupStoredMessage message messageId
      QueryCurrentSenderChatLog message scope keywords limit timeRange ->
        ChatLogStorage.queryCurrentSenderStored message scope keywords limit timeRange
    )
    inner
