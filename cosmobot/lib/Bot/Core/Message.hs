{-|
Module      : Bot.Core.Message
Description : Unified incoming message types
Stability   : experimental
-}

module Bot.Core.Message
  ( -- * Message identity
    MessageId (..)
  , messageIdText
  , textMessageId
  , integerMessageId
  , messageIdInteger

    -- * Chat identity
  , ChatPlatform (..)
  , chatPlatformKey
  , ChatKind (..)
  , MessageDigest (..)
  , emptyMessageDigest

    -- * Incoming messages
  , IncomingMessage (..)
  , IncomingMessageEventKind (..)
  , incomingMessageLogLine
  , MessageFile (..)
  , MessageInput (..)
  , MessageInputAttachment (..)
  , inputWithAttachments
  , inputWithImages
  , messageInputImageUrls
  , messageInputFiles

    -- * Referenced messages
  , ReferencedMessage (..)
  )
where

import Bot.Prelude
import Bot.Util.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text

-- | Message id scoped by its source platform/chat context.
newtype MessageId = MessageId Text
  deriving (Eq, Ord, Show, Generic)

instance IsString MessageId where
  fromString =
    MessageId . Text.pack

instance Aeson.ToJSON MessageId where
  toJSON =
    Aeson.String . messageIdText

instance Aeson.FromJSON MessageId where
  parseJSON value =
    (textMessageId <$> (Aeson.parseJSON value :: AesonTypes.Parser Text))
      <|> (integerMessageId <$> (Aeson.parseJSON value :: AesonTypes.Parser Integer))

messageIdText :: MessageId -> Text
messageIdText (MessageId value) =
  value

textMessageId :: Text -> MessageId
textMessageId =
  MessageId

integerMessageId :: Integer -> MessageId
integerMessageId =
  MessageId . show

messageIdInteger :: MessageId -> Maybe Integer
messageIdInteger =
  readMaybe . Text.unpack . messageIdText

-- | Chat platform backends supported by the unified message layer.
data ChatPlatform
  = PlatformQQ
  -- ^ Tencent QQ via a OneBot-compatible gateway.
  | PlatformTelegram
  -- ^ Telegram Bot API.
  | PlatformMatrix
  -- ^ Matrix Client-Server API.
  | PlatformDiscord
  -- ^ Discord Gateway and REST APIs.
  | PlatformRPC
  -- ^ Local WebSocket RPC virtual chat sessions.
  | PlatformACP
  -- ^ Agent Client Protocol virtual chat sessions.
  deriving (Eq, Ord, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

chatPlatformKey :: ChatPlatform -> Text
chatPlatformKey = \case
  PlatformQQ ->
    "qq"
  PlatformTelegram ->
    "telegram"
  PlatformMatrix ->
    "matrix"
  PlatformDiscord ->
    "discord"
  PlatformRPC ->
    "rpc"
  PlatformACP ->
    "acp"

-- | Coarse chat shape shared across platforms.
data ChatKind
  = ChatPrivate
  | ChatGroup
  | ChatChannel
  | ChatUnknown Text
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

-- | Driver-provided facts that are not inherent in the raw message payload.
data MessageDigest = MessageDigest
  { chatIsAllowed :: !Bool
  , senderIsAllowed :: !Bool
  , senderIsSuperuser :: !Bool
  , mentionsBot :: !Bool
  , botId :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)
    deriving (Aeson.ToJSON, Aeson.FromJSON) via (JSON MessageDigest)

emptyMessageDigest :: MessageDigest
emptyMessageDigest =
  MessageDigest
    { chatIsAllowed = False
    , senderIsAllowed = False
    , senderIsSuperuser = False
    , mentionsBot = False
    , botId = Nothing
    }

-- | Platform-normalized message consumed by handlers.
data IncomingMessageEventKind
  = IncomingMessageCreated
  | IncomingMessageDeleted
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data IncomingMessage = IncomingMessage
  { eventKind :: !IncomingMessageEventKind
  , platform  :: !ChatPlatform
  , kind      :: !ChatKind
  , chatId    :: !(Maybe Integer)
  , chatAliases :: ![Text]
  , digest    :: !MessageDigest
  , senderId  :: !(Maybe Text)
  , senderUsername :: !(Maybe Text)
  , messageId :: !(Maybe MessageId)
  , replyToMessageId :: !(Maybe MessageId)
  , mentions  :: ![Text]
  , mentionUsernames :: ![Text]
  , imageUrls :: ![Text]
  , files     :: ![MessageFile]
  , text      :: !Text
  , raw       :: !Aeson.Value
  }
  deriving (Show, Generic, Aeson.ToJSON)

instance Aeson.FromJSON IncomingMessage where
  parseJSON = Aeson.withObject "IncomingMessage" \o ->
    IncomingMessage
      <$> o Aeson..:? "eventKind" Aeson..!= IncomingMessageCreated
      <*> o Aeson..: "platform"
      <*> o Aeson..: "kind"
      <*> o Aeson..:? "chatId"
      <*> o Aeson..:? "chatAliases" Aeson..!= []
      <*> o Aeson..: "digest"
      <*> o Aeson..:? "senderId"
      <*> o Aeson..:? "senderUsername"
      <*> o Aeson..:? "messageId"
      <*> o Aeson..:? "replyToMessageId"
      <*> o Aeson..:? "mentions" Aeson..!= []
      <*> o Aeson..:? "mentionUsernames" Aeson..!= []
      <*> o Aeson..:? "imageUrls" Aeson..!= []
      <*> o Aeson..:? "files" Aeson..!= []
      <*> o Aeson..:? "text" Aeson..!= ""
      <*> o Aeson..:? "raw" Aeson..!= Aeson.Null

-- | Normalized user-provided input for one handler/agent turn.
--
-- The attachment type is intentionally algebraic so non-image inputs such as
-- documents can be added without threading another parallel field through the
-- handler and agent layers.
data MessageFile = MessageFile
  { name :: !Text
  , ref :: !Text
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data MessageInput = MessageInput
  { text :: !Text
  , attachments :: ![MessageInputAttachment]
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data MessageInputAttachment
  = MessageInputImageUrl !Text
  | MessageInputFile !MessageFile
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

inputWithImages :: Text -> [Text] -> MessageInput
inputWithImages text imageUrls =
  inputWithAttachments text imageUrls []

inputWithAttachments :: Text -> [Text] -> [MessageFile] -> MessageInput
inputWithAttachments text imageUrls files =
  MessageInput
    { text = text
    , attachments = map MessageInputImageUrl imageUrls <> map MessageInputFile files
    }

messageInputImageUrls :: MessageInput -> [Text]
messageInputImageUrls MessageInput{attachments} =
  [ url
  | MessageInputImageUrl rawUrl <- attachments
  , let url = Text.strip rawUrl
  , not (Text.null url)
  ]

messageInputFiles :: MessageInput -> [MessageFile]
messageInputFiles MessageInput{attachments} =
  [ file
  | MessageInputFile file <- attachments
  ]

-- | Compact one-line representation for info-level logs.
incomingMessageLogLine :: IncomingMessage -> Text
incomingMessageLogLine message =
  Text.unwords
    [ "platform=" <> show message.platform
    , "event=" <> show message.eventKind
    , "kind=" <> show message.kind
    , "chat=" <> showMaybe message.chatId
    , "chat_allowed=" <> show message.digest.chatIsAllowed
    , "sender_allowed=" <> show message.digest.senderIsAllowed
    , "sender=" <> showMaybe message.senderId
    , "username=" <> fromMaybe "-" message.senderUsername
    , "superuser=" <> show message.digest.senderIsSuperuser
    , "bot=" <> showMaybe message.digest.botId
    , "message=" <> showMaybeMessageId message.messageId
    , "reply_to=" <> showMaybeMessageId message.replyToMessageId
    , "mentions=" <> show (length message.mentions + length message.mentionUsernames)
    , "mentions_bot=" <> show message.digest.mentionsBot
    , "images=" <> show (length message.imageUrls)
    , "files=" <> show (length message.files)
    , "text=" <> previewText 80 message.text
    ]

previewText :: Int -> Text -> Text
previewText maxChars text =
  let oneLine = Text.unwords (Text.words text)
      shortened = Text.take maxChars oneLine
  in if Text.length oneLine > maxChars
    then shortened <> "..."
    else shortened

showMaybe :: Show a => Maybe a -> Text
showMaybe =
  maybe "-" show

showMaybeMessageId :: Maybe MessageId -> Text
showMaybeMessageId =
  maybe "-" messageIdText

-- | Minimal content fetched for a message referenced by reply.
data ReferencedMessage = ReferencedMessage
  { messageId :: !(Maybe MessageId)
  , senderDisplayName :: !(Maybe Text)
  , senderIdentifier :: !(Maybe Text)
  , senderIsBot :: !Bool
  , text      :: !Text
  , imageUrls :: ![Text]
  , files     :: ![MessageFile]
  }
  deriving (Show, Generic, Aeson.ToJSON)
