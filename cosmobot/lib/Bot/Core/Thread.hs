{-|
Module      : Bot.Core.Thread
Description : Core platform thread values
Stability   : experimental
-}

module Bot.Core.Thread
  ( ThreadMessageKey (..)
  , threadMessageKey
  , ThreadNode (..)
  , ThreadTree (..)
  , emptyThreadTree
  , lookupThreadNode
  , insertThreadNode
  , threadTreeEntries
  )
where

import Bot.Core.Message
import Bot.Core.Transcript
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map

-- | Chat-scoped identity for a message that can anchor a thread node.
--
-- Platform message ids are not globally unique. Telegram message ids, for
-- example, are scoped to a chat, so the key must carry the normalized platform
-- and chat identity together with the initiating sender and message id.
data ThreadMessageKey = ThreadMessageKey
  { platform :: !ChatPlatform
  , chatId :: !(Maybe Integer)
  , senderId :: !(Maybe Text)
  , messageId :: !MessageId
  }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON ThreadMessageKey where
  toJSON key =
    Aeson.object
      [ "platform" Aeson..= key.platform
      , "chatId" Aeson..= key.chatId
      , "senderId" Aeson..= key.senderId
      , "messageId" Aeson..= key.messageId
      ]

instance Aeson.FromJSON ThreadMessageKey where
  parseJSON = Aeson.withObject "ThreadMessageKey" \o -> do
    camelSenderId <- o Aeson..:? "senderId"
    snakeSenderId <- o Aeson..:? "sender_id"
    ThreadMessageKey
      <$> o Aeson..: "platform"
      <*> o Aeson..:? "chatId"
      <*> pure (camelSenderId <|> snakeSenderId)
      <*> o Aeson..: "messageId"

threadMessageKey :: IncomingMessage -> MessageId -> ThreadMessageKey
threadMessageKey message messageId =
  ThreadMessageKey
    { platform = message.platform
    , chatId = message.chatId
    , senderId = message.senderId
    , messageId = messageId
    }

-- | One node in the platform reply tree.
--
-- The node keeps the accumulated LLM transcript for its message and an optional
-- parent key. It does not know how the node is cached or persisted.
data ThreadNode = ThreadNode
  { messageKey :: !ThreadMessageKey
  , parentMessageKey :: !(Maybe ThreadMessageKey)
  , transcript :: !Transcript
  }
  deriving (Show)

-- | Pure thread tree indexed by chat-scoped message keys.
--
-- The tree is represented as keyed nodes with parent links instead of a nested
-- child list because the main runtime operation is reply lookup by message id.
-- Storage modules can still derive branches by following 'parentMessageKey'.
newtype ThreadTree = ThreadTree
  { nodes :: Map.Map ThreadMessageKey ThreadNode
  }
  deriving (Show)

emptyThreadTree :: ThreadTree
emptyThreadTree =
  ThreadTree Map.empty

lookupThreadNode :: ThreadMessageKey -> ThreadTree -> Maybe ThreadNode
lookupThreadNode messageKey tree =
  Map.lookup messageKey tree.nodes

insertThreadNode :: ThreadNode -> ThreadTree -> ThreadTree
insertThreadNode node tree =
  ThreadTree (Map.insert node.messageKey node tree.nodes)

threadTreeEntries :: ThreadTree -> [(ThreadMessageKey, ThreadNode)]
threadTreeEntries =
  Map.toList . (.nodes)
