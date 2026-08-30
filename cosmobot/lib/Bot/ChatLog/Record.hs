{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Bot.ChatLog.Record
Description : Chat log record construction and sanitization
Stability   : experimental
-}

module Bot.ChatLog.Record
  ( ChatLogRecord
  , userRecord
  , selfRecord
  , chatLogEntry
  , sanitizeChatLogEntry
  )
where

import Bot.ChatLog.Types
import qualified Bot.Effect.Chat as Chat
import Bot.Core.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Time (UTCTime)

data ChatLogRecord = ChatLogRecord
  { message :: !IncomingMessage
  , isBot :: !Bool
  }

userRecord :: IncomingMessage -> ChatLogRecord
userRecord message =
  ChatLogRecord message False

selfRecord :: IncomingMessage -> Text -> ChatLogRecord
selfRecord context body =
  ChatLogRecord
    (selfMessage context body)
    True

selfMessage :: IncomingMessage -> Text -> IncomingMessage
selfMessage context body =
  IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = context.platform
    , kind = context.kind
    , chatId = context.chatId
    , chatAliases = context.chatAliases
    , digest = emptyMessageDigest
    , senderId = Nothing
    , senderUsername = Nothing
    , messageId = Nothing
    , replyToMessageId = context.messageId
    , mentions = []
    , mentionUsernames = []
    , imageUrls = Chat.replyImageUrls body
    , files = []
    , text = Chat.renderReplyBody body
    , raw = Aeson.object
        [ "type" Aeson..= Aeson.String "bot_message"
        , "body" Aeson..= body
        ]
    }

chatLogEntry :: UTCTime -> ChatLogRecord -> ChatLogEntry
chatLogEntry recordedAt record =
  ChatLogEntry
    { recordedAt = Just recordedAt
    , platform = record.message.platform
    , kind = record.message.kind
    , chatId = record.message.chatId
    , senderId = record.message.senderId
    , senderUsername = record.message.senderUsername
    , messageId = record.message.messageId
    , replyToMessageId = record.message.replyToMessageId
    , isBot = record.isBot
    , mentions = record.message.mentions
    , mentionUsernames = record.message.mentionUsernames
    , imageUrls = record.message.imageUrls
    , text = record.message.text
    }

sanitizeChatLogEntry :: ChatLogEntry -> ChatLogEntry
sanitizeChatLogEntry ChatLogEntry{..} =
  ChatLogEntry
    { imageUrls = map sanitizeImageRef imageUrls
    , text = sanitizeImageText text
    , ..
    }

sanitizeImageRef :: Text -> Text
sanitizeImageRef ref
  | Chat.isBase64ImageRef ref = "[Picture]"
  | otherwise = ref

sanitizeImageText :: Text -> Text
sanitizeImageText text =
  Text.intercalate "\n" (map sanitizeImageLine (Text.lines text))

sanitizeImageLine :: Text -> Text
sanitizeImageLine line
  | Chat.isBase64ImageRef stripped = "[Picture]"
  | Just ref <- Text.stripPrefix "[image] " stripped
  , Chat.isBase64ImageRef ref = "[image] [Picture]"
  | isBase64ImageRefInfix stripped = "[Picture]"
  | otherwise = line
  where
    stripped = Text.strip line

isBase64ImageRefInfix :: Text -> Bool
isBase64ImageRefInfix text =
  "data:image/" `Text.isInfixOf` text &&
    ";base64," `Text.isInfixOf` text
