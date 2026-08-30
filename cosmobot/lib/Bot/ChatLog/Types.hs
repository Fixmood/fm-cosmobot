{-|
Module      : Bot.ChatLog.Types
Description : Chat log domain types
Stability   : experimental
-}

module Bot.ChatLog.Types
  ( ChatLogEntry (..)
  , SenderChatLogScope (..)
  , ChatLogTimeRange (..)
  , unboundedChatLogTimeRange
  )
where

import Bot.Core.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import Data.Time (UTCTime)

data SenderChatLogScope
  = SenderChatLogChat
  | SenderChatLogGlobal
  deriving (Eq, Show)

data ChatLogTimeRange = ChatLogTimeRange
  { since :: !(Maybe UTCTime)
  , before :: !(Maybe UTCTime)
  }
  deriving (Eq, Show)

unboundedChatLogTimeRange :: ChatLogTimeRange
unboundedChatLogTimeRange =
  ChatLogTimeRange Nothing Nothing

-- | Sanitized message record exposed to agent tools.
data ChatLogEntry = ChatLogEntry
  { recordedAt :: !(Maybe UTCTime)
  , platform :: !ChatPlatform
  , kind :: !ChatKind
  , chatId :: !(Maybe Integer)
  , senderId :: !(Maybe Text)
  , senderUsername :: !(Maybe Text)
  , messageId :: !(Maybe MessageId)
  , replyToMessageId :: !(Maybe MessageId)
  , isBot :: !Bool
  , mentions :: ![Text]
  , mentionUsernames :: ![Text]
  , imageUrls :: ![Text]
  , text :: !Text
  }
  deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON)
