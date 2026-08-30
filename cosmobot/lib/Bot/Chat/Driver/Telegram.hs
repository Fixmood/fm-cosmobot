{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.Chat.Driver.Telegram
Description : Telegram chat driver
Stability   : experimental
-}

module Bot.Chat.Driver.Telegram
  ( TelegramDriver
  , newTelegramDriver
  , Config (..)
  , User (..)
  , Update (..)
  , Chat (..)
  , ChatType (..)
  , ChatMember (..)
  , ChatMemberStatus (..)
  , Message (..)
  , MessageEntity (..)
  , PhotoSize (..)
  , TelegramMedia (..)
  , ParseMode (..)
  , InputRichMessage (..)
  , InputRichMessageMedia (..)
  , InputRichMedia (..)
  , ReplyParameters (..)
  , SendRichMessageRequest (..)
  , SendMessageRequest (..)
  , EditMessageTextRequest (..)
  , SendPhotoRequest (..)
  , SendDocumentRequest (..)
  , SendVoiceRequest (..)
  , TelegramException (..)
  , TelegramResult
  , parseTelegramResult
  , formatTelegramRichHtml
  , telegramFailureReplyText
  , richMessageFallbackAllowed
  , incomingMessages
  , updateToIncomingMessage
  , updateToIncomingMessageWith
  )
where

import qualified Bot.Chat.Driver.Types as Driver
import qualified Bot.Effect.Chat as ChatEffect
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.Media as Media
import qualified Bot.Media.Mime as Mime
import Bot.Util.Multipart
import Bot.Util.Aeson
import Bot.Core.Message
import Commonmark hiding (escapeHtml)
import Commonmark.Blocks
  ( BlockData (..)
  , BlockSpec (..)
  , BlockStartResult (..)
  , addNodeToStack
  , defBlockData
  , defaultFinalizer
  , getBlockText
  )
import Commonmark.Inlines (InlineParser, withAttributes)
import Commonmark.TokParsers (anyTok, nonindentSpaces, symbol)
import Commonmark.Extensions
  ( HasFootnote (..)
  , HasMath (..)
  , HasPipeTable (..)
  , HasSubscript (..)
  , HasSuperscript (..)
  , HasStrikethrough (..)
  , HasTaskList (..)
  , ColAlignment (..)
  , autolinkSpec
  , footnoteSpec
  , mathSpec
  , pipeTableSpec
  , subscriptSpec
  , superscriptSpec
  , strikethroughSpec
  , taskListSpec
  )
import Data.List (lookup, maximum)
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Lazy as LazyText
import Data.Tree (Tree (..))
import qualified Network.HTTP.Client as Client
import Network.HTTP.Req
import qualified Network.HTTP.Client.MultipartFormData as Multipart
import qualified Network.HTTP.Types.Header as HTTPHeader
import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified Text.Parsec as Parsec
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified Effectful.Temporary as Temporary
import qualified Data.Text as Text
import System.FilePath ((</>), (<.>))

-- ---------------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------------

-- | Telegram Bot API credentials.
data Config = Config
  { botToken :: !Text
  , botIds :: ![Integer]
  , botUsernames :: ![Text]
  , allowedChatIds :: ![Integer]
  , allowedChatAliases :: ![Text]
  , superusers :: ![Text]
  }
  deriving (Show)

newtype TelegramDriver = TelegramDriver
  { config :: Config
  }

newTelegramDriver :: Config -> TelegramDriver
newTelegramDriver config =
  TelegramDriver{config}

instance Driver.ChatDriver TelegramDriver where
  type ChatDriverEffects TelegramDriver es = (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es)

  driverPlatform _ =
    PlatformTelegram

  sendReplyMessage =
    replyToTelegram

  replyAudio =
    replyAudioTelegram

  uploadFile =
    uploadFileTelegram

  editMessage driver message messageId body =
    editMessageTelegram driver message messageId body

  deleteMessage =
    deleteMessageForTelegram

  messageOutPolicy _ _ =
    pure (ChatEffect.EditableMessage telegramEditChunkChars telegramMessageTextLimit)

  getMessageContent =
    getMessageContentTelegram

  getSenderMemberInfo driver message =
      case (message.kind, message.chatId, message.senderId) of
        (ChatGroup, Just chatId, Just rawUserId)
          | Just userId <- parseIntegerUserId rawUserId ->
          Just . Aeson.toJSON <$> getChatMember driver chatId userId
        _ ->
          pure Nothing

  getMemberInfo driver message userId =
      case (message.kind, message.chatId) of
        (ChatGroup, Just chatId)
          | Just numericUserId <- parseIntegerUserId userId ->
            Just . Aeson.toJSON <$> getChatMember driver chatId numericUserId
        _ ->
          pure Nothing

  getUserAvatar driver _ userId =
      maybe (pure Nothing) (getUserAvatar driver) (parseIntegerUserId userId)

  mentionUser =
    mentionUserTelegram

  setTyping driver message _timeoutMillis =
      case message.chatId of
        Just chatId -> do
          _ <- callTelegram driver (SendChatActionRequest chatId ChatActionTyping Nothing Nothing)
          pure ()
        _ ->
          pure ()

telegramEditChunkChars :: Int
telegramEditChunkChars = 512

telegramMessageTextLimit :: Int
telegramMessageTextLimit = 30000

telegramLegacyMessageTextLimit :: Int
telegramLegacyMessageTextLimit = 4096

-- ---------------------------------------------------------------------------
-- Typeclass
-- ---------------------------------------------------------------------------

class (Aeson.ToJSON req, Aeson.FromJSON (TelegramResponse req)) => TelegramRequest req where
  type TelegramResponse req
  telegramMethod :: req -> Text

callTelegram
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, TelegramRequest req)
  => TelegramDriver
  -> req
  -> Eff es (TelegramResponse req)
callTelegram driver request =
  apiCall driver.config (telegramMethod request) request

fileUrl
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
  => TelegramDriver
  -> Text
  -> Eff es Text
fileUrl driver fileId = do
  file :: File <- apiCall driver.config (telegramMethod (GetFileRequest fileId)) (GetFileRequest fileId)
  pure (telegramFileUrl driver.config file.filePath)

-- ---------------------------------------------------------------------------
-- Streaming
-- ---------------------------------------------------------------------------

updatesStream'
  :: (HTTP.HTTP :> es, KatipE :> es, IOE :> es)
  => TelegramDriver
  -> Int
  -> Stream (Of Update) (Eff es) ()
updatesStream' driver offset = do
  batches <- S.lift (getUpdates driver offset)
  S.lift $ logDebug [i|Got a batch of #{length batches} messages|]
  S.lift $ logInfo [i|Telegram update batch: #{length batches}|]
  S.each batches
  let nextOffset = case batches of
        [] -> offset
        _  -> 1 + maximum (map (fromInteger . (.updateId)) batches)
  updatesStream' driver nextOffset

updatesStream :: (HTTP.HTTP :> es, KatipE :> es, IOE :> es) => TelegramDriver -> Stream (Of Update) (Eff es) ()
updatesStream driver = updatesStream' driver 0

-- | Poll Telegram updates and yield platform-independent messages.
incomingMessages :: (HTTP.HTTP :> es, KatipE :> es, IOE :> es) => TelegramDriver -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessages driver = S.for (updatesStream driver) $ \update -> do
  case updateToIncomingMessageWith driver.config update of
    Nothing -> do
      S.lift $ logDebug [i|Ignoring Telegram event|]
      S.lift $ logInfo "Ignoring Telegram event"
    Just parsedMessage -> do
      message <- S.lift $
        resolveIncomingMessageMedia driver parsedMessage `catchSync` \err -> do
          logError [i|Telegram media resolution failed: #{show err :: String}|]
          pure parsedMessage
      S.lift $ logDebug [i|incoming Telegram message: #{show message :: String}|]
      S.lift $ logInfo [i|incoming Telegram message: #{incomingMessageLogLine message}|]
      S.yield message

resolveIncomingMessageMedia :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> IncomingMessage -> Eff es IncomingMessage
resolveIncomingMessageMedia driver message = do
  imageUrls <- traverse (fileUrl driver) message.imageUrls
  files <- traverse (traverseMessageFileRef (fileUrl driver)) message.files
  pure IncomingMessage
    { eventKind = message.eventKind
    , platform = message.platform
    , kind = message.kind
    , chatId = message.chatId
    , chatAliases = message.chatAliases
    , digest = message.digest
    , senderId = message.senderId
    , senderUsername = message.senderUsername
    , messageId = message.messageId
    , replyToMessageId = message.replyToMessageId
    , mentions = message.mentions
    , mentionUsernames = message.mentionUsernames
    , imageUrls = imageUrls
    , files
    , text = message.text
    , raw = message.raw
    }

updateToIncomingMessage :: Update -> Maybe IncomingMessage
updateToIncomingMessage =
  updateToIncomingMessageWith defaultMessageConfig

updateToIncomingMessageWith :: Config -> Update -> Maybe IncomingMessage
updateToIncomingMessageWith cfg Update{message = telegramMessage} = do
  message <- telegramMessage
  guard (not (isBotMessage message))
  pure IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform  = PlatformTelegram
    , kind      = telegramChatKind message.chat.type_
    , chatId    = Just message.chat.id
    , chatAliases = telegramChatAliases message.chat
    , digest = telegramMessageDigest cfg message
    , senderId  = Text.pack . show . (.id) <$> message.from
    , senderUsername = message.from >>= (.username)
    , messageId = Just (integerMessageId message.messageId)
    , replyToMessageId = integerMessageId . (.messageId) <$> message.replyToMessage
    , mentions  = messageMentionIds message
    , mentionUsernames = messageMentionUsernames message
    , imageUrls = messageImageFileIds message
    , files = telegramMessageFiles message
    , text      = messageText message
    , raw       = Aeson.toJSON message
    }

defaultMessageConfig :: Config
defaultMessageConfig =
  Config
    { botToken = ""
    , botIds = []
    , botUsernames = []
    , allowedChatIds = []
    , allowedChatAliases = []
    , superusers = []
    }

telegramMessageDigest :: Config -> Message -> MessageDigest
telegramMessageDigest cfg message =
  MessageDigest
    { chatIsAllowed = chatAllowed
    , senderIsAllowed = telegramChatKind message.chat.type_ == ChatPrivate && (chatAllowed || senderSuperuser)
    , senderIsSuperuser = senderSuperuser
    , mentionsBot =
        any (`elem` map show cfg.botIds) (messageMentionIds message) ||
        any (`elem` cfg.botUsernames) (messageMentionUsernames message)
    , botId = listToMaybe (map (Text.pack . show) cfg.botIds <> cfg.botUsernames)
    }
  where
    chatAllowed =
      message.chat.id `elem` cfg.allowedChatIds ||
        any (`elem` cfg.allowedChatAliases) (telegramChatAliases message.chat)
    senderSuperuser =
      maybe False (`elem` cfg.superusers) (normalizeUsername <$> (message.from >>= (.username)))

telegramChatAliases :: Chat -> [Text]
telegramChatAliases chat =
  map normalizeUsername (catMaybes [chat.username, chat.title])

isBotMessage :: Message -> Bool
isBotMessage message =
  maybe False (.isBot) message.from

telegramChatKind :: ChatType -> ChatKind
telegramChatKind = \case
  ChatTypePrivate    -> ChatPrivate
  ChatTypeGroup      -> ChatGroup
  ChatTypeSuperGroup -> ChatGroup
  ChatTypeChannel    -> ChatChannel

messageMentionIds :: Message -> [Text]
messageMentionIds message =
  mapMaybe entityMentionUserId (messageEntities message)

entityMentionUserId :: MessageEntity -> Maybe Text
entityMentionUserId messageEntity =
  show . (.id) <$> messageEntity.user

messageMentionUsernames :: Message -> [Text]
messageMentionUsernames message =
  mapMaybe (entityMentionUsername (messageText message)) (messageEntities message)

messageText :: Message -> Text
messageText message =
  Text.strip (fromMaybe "" (message.text <|> message.caption))

messageEntities :: Message -> [MessageEntity]
messageEntities message =
  fromMaybe [] (message.entities <|> message.captionEntities)

messageImageFileIds :: Message -> [Text]
messageImageFileIds message =
  maybe [] (maybeToList . largestPhotoFileId) message.photo

telegramMessageFiles :: Message -> [MessageFile]
telegramMessageFiles message =
  catMaybes
    [ telegramMessageFile "document" <$> message.document
    , telegramMessageFile "audio" <$> message.audio
    , telegramMessageFile "voice" <$> message.voice
    ]

telegramMessageFile :: Text -> TelegramMedia -> MessageFile
telegramMessageFile fallback media =
  MessageFile
    { name = fromMaybe (fallback <> maybe "" Mime.extensionFromMime media.mimeType) media.fileName
    , ref = media.fileId
    }

traverseMessageFileRef :: Functor f => (Text -> f Text) -> MessageFile -> f MessageFile
traverseMessageFileRef resolve file =
  (\ref -> MessageFile{name = file.name, ref}) <$> resolve file.ref

largestPhotoFileId :: [PhotoSize] -> Maybe Text
largestPhotoFileId =
  fmap (.fileId) . largestPhoto

largestPhoto :: [PhotoSize] -> Maybe PhotoSize
largestPhoto =
  viaNonEmpty largest
  where
    largest photos =
      let MaxPhotoSize photo = sconcat (fmap MaxPhotoSize photos)
      in photo

newtype MaxPhotoSize = MaxPhotoSize PhotoSize

instance Semigroup MaxPhotoSize where
  MaxPhotoSize a <> MaxPhotoSize b =
    MaxPhotoSize (if photoArea a >= photoArea b then a else b)

photoArea :: PhotoSize -> Integer
photoArea photo =
  photo.width * photo.height

-- | Resolve the content of a replied-to Telegram message when available locally.
getMessageContentTelegram
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
  => TelegramDriver
  -> IncomingMessage
  -> MessageId
  -> Eff es (Maybe ReferencedMessage)
getMessageContentTelegram driver message messageId =
  case messageIdInteger messageId of
    Nothing ->
      pure Nothing
    Just rawMessageId ->
      case Aeson.fromJSON message.raw :: Aeson.Result Message of
        Aeson.Success telegramMessage ->
          case telegramMessage.replyToMessage of
            Just referenced
              | referenced.messageId == rawMessageId -> do
                  imageUrls <- traverse (fileUrl driver) (messageImageFileIds referenced)
                  files <- traverse (traverseMessageFileRef (fileUrl driver)) (telegramMessageFiles referenced)
                  pure $ Just ReferencedMessage
                    { messageId = Just (integerMessageId referenced.messageId)
                    , senderDisplayName = telegramMessageSenderDisplayName referenced
                    , senderIdentifier = telegramMessageSenderIdentifier referenced
                    , senderIsBot = maybe False (.isBot) referenced.from
                    , text = messageText referenced
                    , imageUrls = imageUrls
                    , files
                    }
            _ -> pure Nothing
        Aeson.Error _ ->
          pure Nothing

entityMentionUsername :: Text -> MessageEntity -> Maybe Text
entityMentionUsername text messageEntity
  | Just username <- messageEntity.user >>= (.username) =
      Just (normalizeUsername username)
  | messageEntity.type_ == "mention" =
      normalizeUsername <$> entityText text messageEntity
  | otherwise =
      Nothing

telegramMessageSenderDisplayName :: Message -> Maybe Text
telegramMessageSenderDisplayName message =
  telegramUserFullName <$> message.from

telegramMessageSenderIdentifier :: Message -> Maybe Text
telegramMessageSenderIdentifier message =
  message.from <&> \user ->
    maybe (show user.id) ("@" <>) user.username

telegramUserFullName :: User -> Text
telegramUserFullName user =
  Text.unwords (filter (not . Text.null) [user.firstName, fromMaybe "" user.lastName])

entityText :: Text -> MessageEntity -> Maybe Text
entityText text messageEntity =
  let piece = Text.take (fromInteger messageEntity.length) (Text.drop (fromInteger messageEntity.offset) text)
  in if Text.null piece
    then Nothing
    else Just piece

normalizeUsername :: Text -> Text
normalizeUsername =
  Text.toLower . Text.dropWhile (== '@') . Text.strip

-- ---------------------------------------------------------------------------
-- Telegram API
-- ---------------------------------------------------------------------------

apiUrl :: Config -> Text -> Url 'Https
apiUrl cfg method =
  https "api.telegram.org"
    /: ("bot" <> cfg.botToken)
    /: method

telegramFileUrl :: Config -> Text -> Text
telegramFileUrl cfg path =
  [i|https://api.telegram.org/file/bot#{token}/#{path}|]
  where
    token = cfg.botToken

apiCall
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Aeson.ToJSON body, Aeson.FromJSON result)
  => Config
  -> Text
  -> body
  -> Eff es result
apiCall cfg method body = do
  logTelegramApiRequest method
  resp :: TelegramResult <-
    ( HTTP.runReq $
        req POST (apiUrl cfg method) (ReqBodyJson body) jsonResponse (telegramRequestOptions method)
          <&> responseBody
    ) `catch` \(err :: HttpException) ->
      throwIO (TelegramException (telegramExceptionMessage cfg err))
  logTelegramApiResponse method
  parseTelegramResult resp

apiMultipartCall
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Aeson.FromJSON result)
  => Config
  -> Text
  -> [Multipart.Part]
  -> Eff es result
apiMultipartCall cfg method parts = do
  logTelegramApiRequest method
  resp :: TelegramResult <-
    ( HTTP.runReq do
        body <- reqBodyMultipart parts
        req POST (apiUrl cfg method) body jsonResponse (telegramRequestOptions method)
          <&> responseBody
    ) `catch` \(err :: HttpException) ->
      throwIO (TelegramException (telegramExceptionMessage cfg err))
  logTelegramApiResponse method
  parseTelegramResult resp

telegramExceptionMessage :: Config -> HttpException -> Text
telegramExceptionMessage cfg err =
  case err of
    VanillaHttpException (Client.HttpExceptionRequest _ (Client.StatusCodeException _ body)) ->
      case Aeson.eitherDecodeStrict body of
        Right result -> telegramResultError result
        Left _ -> sanitizeTelegramException cfg err
    _ ->
      sanitizeTelegramException cfg err

sanitizeTelegramException :: Show err => Config -> err -> Text
sanitizeTelegramException cfg err =
  Text.replace cfg.botToken "<telegram-token>" (show err)

logTelegramApiRequest :: KatipE :> es => Text -> Eff es ()
logTelegramApiRequest method =
  unless (method == "getUpdates") $
    logInfo [i|Telegram API request: #{method}|]

logTelegramApiResponse :: KatipE :> es => Text -> Eff es ()
logTelegramApiResponse method =
  unless (method == "getUpdates") $
    logInfo [i|Telegram API response: #{method}|]

parseTelegramResult
  :: (IOE :> es, Aeson.FromJSON result)
  => TelegramResult
  -> Eff es result
parseTelegramResult resp =
  case resp of
    Err desc -> throwIO (TelegramException desc)
    Ok value -> case Aeson.fromJSON value of
      Aeson.Success x  -> pure x
      Aeson.Error  err -> throwIO (TelegramException (Text.pack err))

newtype TelegramException = TelegramException Text
  deriving (Show)
instance Exception TelegramException where
  displayException (TelegramException message) = Text.unpack message

withRichMessageFallback :: IOE :> es => Text -> Eff es a -> Eff es a -> Eff es a
withRichMessageFallback text action fallback =
  action `catchSync` \err ->
    case fromException err :: Maybe TelegramException of
      Just telegramError
        | richMessageFallbackAllowed text telegramError -> fallback
      _ -> throwIO err

richMessageFallbackAllowed :: Text -> TelegramException -> Bool
richMessageFallbackAllowed text (TelegramException message) =
  Text.length text <= telegramLegacyMessageTextLimit
    && "Bad Request:" `Text.isPrefixOf` message

data TelegramResult
  = Ok  Aeson.Value
  | Err Text
  deriving (Show, Generic)

instance Aeson.FromJSON TelegramResult where
  parseJSON = Aeson.withObject "TelegramResult" $ \o -> do
    ok <- o Aeson..: "ok"
    if ok
      then Ok <$> o Aeson..: "result"
      else Err <$> o Aeson..: "description"

telegramResultError :: TelegramResult -> Text
telegramResultError = \case
  Ok _ -> "Telegram API returned ok result in an HTTP error response."
  Err desc -> desc

telegramLongPollTimeoutSeconds :: Int
telegramLongPollTimeoutSeconds = 30

telegramLongPollResponseTimeoutMicroseconds :: Int
telegramLongPollResponseTimeoutMicroseconds =
  (telegramLongPollTimeoutSeconds + 10) * 1000000

telegramApiResponseTimeoutMicroseconds :: Int
telegramApiResponseTimeoutMicroseconds =
  10 * 1000000

telegramRequestOptions :: Text -> Option 'Https
telegramRequestOptions method =
  responseTimeout $
    if method == "getUpdates"
      then telegramLongPollResponseTimeoutMicroseconds
      else telegramApiResponseTimeoutMicroseconds

sendPhotoParts :: SendPhotoRequest -> FilePath -> Maybe Text -> [Multipart.Part]
sendPhotoParts SendPhotoRequest{..} path fileName =
  [ textPart "chat_id" (show chatId)
  , telegramFilePart "photo" path fileName
  ]
    <> maybePart "message_thread_id" (show <$> messageThreadId)
    <> maybePart "caption" caption
    <> maybePart "parse_mode" (parseModeText <$> parseMode)
    <> maybePart "caption_entities" (jsonText <$> captionEntities)
    <> maybePart "disable_notification" (boolText <$> disableNotification)
    <> maybePart "reply_to_message_id" (show <$> replyToMessageId)

sendDocumentParts :: SendDocumentRequest -> FilePath -> Maybe Text -> [Multipart.Part]
sendDocumentParts SendDocumentRequest{..} path fileName =
  [ textPart "chat_id" (show chatId)
  , telegramFilePart "document" path fileName
  ]
    <> maybePart "message_thread_id" (show <$> messageThreadId)
    <> maybePart "caption" caption
    <> maybePart "parse_mode" (parseModeText <$> parseMode)
    <> maybePart "caption_entities" (jsonText <$> captionEntities)
    <> maybePart "disable_notification" (boolText <$> disableNotification)
    <> maybePart "reply_to_message_id" (show <$> replyToMessageId)

sendAudioParts :: SendAudioRequest -> FilePath -> Maybe Text -> [Multipart.Part]
sendAudioParts SendAudioRequest{..} path fileName =
  [ textPart "chat_id" (show chatId)
  , telegramFilePart "audio" path fileName
  ]
    <> maybePart "message_thread_id" (show <$> messageThreadId)
    <> maybePart "caption" caption
    <> maybePart "parse_mode" (parseModeText <$> parseMode)
    <> maybePart "caption_entities" (jsonText <$> captionEntities)
    <> maybePart "disable_notification" (boolText <$> disableNotification)
    <> maybePart "reply_to_message_id" (show <$> replyToMessageId)

sendVideoParts :: SendVideoRequest -> FilePath -> Maybe Text -> [Multipart.Part]
sendVideoParts SendVideoRequest{..} path fileName =
  [ textPart "chat_id" (show chatId)
  , telegramFilePart "video" path fileName
  ]
    <> maybePart "message_thread_id" (show <$> messageThreadId)
    <> maybePart "caption" caption
    <> maybePart "parse_mode" (parseModeText <$> parseMode)
    <> maybePart "caption_entities" (jsonText <$> captionEntities)
    <> maybePart "disable_notification" (boolText <$> disableNotification)
    <> maybePart "reply_to_message_id" (show <$> replyToMessageId)

sendVoiceParts :: SendVoiceRequest -> FilePath -> Maybe Text -> [Multipart.Part]
sendVoiceParts SendVoiceRequest{..} path fileName =
  [ textPart "chat_id" (show chatId)
  , telegramFilePart "voice" path fileName
  ]
    <> maybePart "message_thread_id" (show <$> messageThreadId)
    <> maybePart "caption" caption
    <> maybePart "parse_mode" (parseModeText <$> parseMode)
    <> maybePart "caption_entities" (jsonText <$> captionEntities)
    <> maybePart "disable_notification" (boolText <$> disableNotification)
    <> maybePart "reply_to_message_id" (show <$> replyToMessageId)

telegramFilePart :: Text -> FilePath -> Maybe Text -> Multipart.Part
telegramFilePart fieldName path fileName =
  Multipart.addPartHeaders
    (Multipart.partFileRequestBodyM fieldName (Text.unpack (Driver.uploadFileName path fileName)) (Client.streamFile path))
    [(HTTPHeader.hContentType, TextEncoding.encodeUtf8 (Mime.mimeFromName (Text.pack path)))]

jsonText :: Aeson.ToJSON a => a -> Text
jsonText =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

boolText :: Bool -> Text
boolText True  = "true"
boolText False = "false"

parseModeText :: ParseMode -> Text
parseModeText ParseModeMarkdown   = "Markdown"
parseModeText ParseModeMarkdownV2 = "MarkdownV2"
parseModeText ParseModeHTML       = "HTML"

-- ---------------------------------------------------------------------------
-- Formatting
-- ---------------------------------------------------------------------------

data TelegramRichHtml = TelegramRichHtml
  { value :: !(Html ())
  , containsBlock :: !Bool
  }
  deriving (Show)

instance Semigroup TelegramRichHtml where
  left <> right = TelegramRichHtml
    { value = left.value <> right.value
    , containsBlock = left.containsBlock || right.containsBlock
    }

instance Monoid TelegramRichHtml where
  mempty = TelegramRichHtml mempty False

instance Rangeable TelegramRichHtml where
  ranged _ = id

instance HasAttributes TelegramRichHtml where
  addAttributes attrs html =
    html{value = addAttributes attrs html.value}

instance ToPlainText TelegramRichHtml where
  toPlainText = toPlainText . (.value)

instance IsInline TelegramRichHtml where
  lineBreak = inlineHtml (lineBreak :: Html ())
  softBreak = inlineHtml (softBreak :: Html ())
  str = inlineHtml . str
  entity = inlineHtml . entity
  escapedChar = inlineHtml . escapedChar
  emph = mapRichHtml emph
  strong = mapRichHtml strong
  link target title = mapRichHtml (link target title)
  image = telegramRichMedia
  code = inlineHtml . code
  rawInline format = inlineHtml . rawInline format

instance IsBlock TelegramRichHtml TelegramRichHtml where
  paragraph html
    | html.containsBlock = html
    | otherwise = blockHtml (paragraph html.value)
  plain html
    | html.containsBlock = html
    | otherwise = inlineHtml (plain html.value)
  thematicBreak = blockHtml (thematicBreak :: Html ())
  blockQuote = blockHtml . blockQuote . (.value)
  codeBlock info = blockHtml . codeBlock info
  heading level = blockHtml . heading level . (.value)
  rawBlock format = blockHtml . rawBlock format
  referenceLinkDefinition _ _ = mempty
  list listType spacing = blockHtml . list listType spacing . map (.value)

instance HasStrikethrough TelegramRichHtml where
  strikethrough = mapRichHtml strikethrough

instance HasSubscript TelegramRichHtml where
  subscript = mapRichHtml subscript

instance HasSuperscript TelegramRichHtml where
  superscript = mapRichHtml superscript

instance HasMath TelegramRichHtml where
  inlineMath = inlineHtml . htmlInline "tg-math" . Just . htmlText
  displayMath = blockHtml . htmlBlock "tg-math-block" . Just . htmlText

instance HasTaskList TelegramRichHtml TelegramRichHtml where
  taskList listType spacing items =
    blockHtml $ list listType spacing (map taskItem items)
    where
      taskItem (checked, item) =
        checkbox checked <> item.value
      checkbox checked =
        addAttribute ("type", "checkbox")
        . (if checked then addAttribute ("checked", "") else id)
        $ htmlInline "input" Nothing

instance HasPipeTable TelegramRichHtml TelegramRichHtml where
  pipeTable alignments headers rows =
    blockHtml $ htmlBlock "table" $ Just $
      tableRow "th" alignments headers <> mconcat (map (tableRow "td" alignments) rows)

instance HasFootnote TelegramRichHtml TelegramRichHtml where
  footnote _ label body =
    blockHtml $ addAttribute ("name", "note-" <> label) $
      htmlBlock "tg-reference" (Just body.value)
  footnoteList =
    mconcat
  footnoteRef number label _ =
    inlineHtml $ addAttribute ("href", "#note-" <> label) $
      htmlInline "a" (Just (htmlText ("[" <> number <> "]")))

inlineHtml :: Html () -> TelegramRichHtml
inlineHtml value =
  TelegramRichHtml{value, containsBlock = False}

blockHtml :: Html () -> TelegramRichHtml
blockHtml value =
  TelegramRichHtml{value, containsBlock = True}

mapRichHtml :: (Html () -> Html ()) -> TelegramRichHtml -> TelegramRichHtml
mapRichHtml action html =
  html{value = action html.value}

telegramRichMedia :: Text -> Text -> TelegramRichHtml -> TelegramRichHtml
telegramRichMedia target title description
  | Just emojiId <- Text.stripPrefix "tg://emoji?id=" target =
      inlineHtml $
        htmlRaw ("<tg-emoji emoji-id=\"" <> escapeHtml emojiId <> "\">")
          <> description.value
          <> htmlRaw "</tg-emoji>"
  | Just (unix, format) <- telegramTime target =
      inlineHtml $
        htmlRaw ("<tg-time unix=\"" <> escapeHtml unix <> "\"" <> maybe "" (\value -> " format=\"" <> escapeHtml value <> "\"") format <> ">")
          <> description.value
          <> htmlRaw "</tg-time>"
  | isHttpUrl target =
      blockHtml $ case nonEmptyText (Text.strip title) of
        Nothing -> media
        Just caption -> htmlBlock "figure" (Just (media <> htmlBlock "figcaption" (Just (htmlText caption))))
  | otherwise =
      description
  where
    tag = telegramMediaTag target
    media =
      addAttribute ("src", target) $
        if tag == "img"
          then htmlBlock tag Nothing
          else htmlBlock tag (Just mempty)

telegramTime :: Text -> Maybe (Text, Maybe Text)
telegramTime target = do
  query <- Text.stripPrefix "tg://time?" target
  let parameters = map (Text.breakOn "=") (Text.splitOn "&" query)
      parameter name = Text.drop 1 <$> lookup name parameters
  unix <- parameter "unix"
  pure (unix, parameter "format")

telegramMediaTag :: Text -> Text
telegramMediaTag target
  | mime == "image/gif" = "video"
  | "video/" `Text.isPrefixOf` mime = "video"
  | "audio/" `Text.isPrefixOf` mime = "audio"
  | otherwise = "img"
  where
    mime = Text.toLower (Mime.mimeFromName (Text.takeWhile (`notElem` ['?', '#']) target))

isHttpUrl :: Text -> Bool
isHttpUrl target =
  let lower = Text.toLower (Text.strip target)
  in "http://" `Text.isPrefixOf` lower || "https://" `Text.isPrefixOf` lower

tableRow :: Text -> [ColAlignment] -> [TelegramRichHtml] -> Html ()
tableRow cellTag alignments cells =
  htmlBlock "tr" . Just . mconcat $
    zipWith renderCell (alignments <> repeat DefaultAlignedCol) cells
  where
    renderCell alignment cell =
      htmlRaw ("<" <> cellTag <> alignmentAttribute alignment <> ">")
        <> cell.value
        <> htmlRaw ("</" <> cellTag <> ">")
    alignmentAttribute LeftAlignedCol = " align=\"left\""
    alignmentAttribute CenterAlignedCol = " align=\"center\""
    alignmentAttribute RightAlignedCol = " align=\"right\""
    alignmentAttribute DefaultAlignedCol = ""

telegramRichHtmlSyntax :: SyntaxSpec Identity TelegramRichHtml TelegramRichHtml
telegramRichHtmlSyntax =
  latexMathSpec
    <> mathSpec
    <> subscriptSpec
    <> superscriptSpec
    <> strikethroughSpec
    <> taskListSpec
    <> footnoteSpec
    <> autolinkSpec
    <> defaultSyntaxSpec
    <> pipeTableSpec

latexMathSpec :: (Monad m, IsBlock il bl, IsInline il, HasMath il, HasMath bl) => SyntaxSpec m il bl
latexMathSpec =
  mempty
    { syntaxBlockSpecs = [latexDisplayMathBlockSpec]
    , syntaxInlineParsers = [withAttributes parseLatexMath]
    }

parseLatexMath :: (Monad m, HasMath il) => InlineParser m il
parseLatexMath = Parsec.try do
  void (symbol '\\')
  void (symbol '(')
  inlineMath . untokenize <$> latexMathContents ')'

latexMathContents :: Monad m => Char -> InlineParser m [Tok]
latexMathContents closing = do
  token <- anyTok
  case token.tokType of
    Symbol '\\' -> do
      next <- anyTok
      case next.tokType of
        Symbol char | char == closing -> pure []
        _ -> (token :) . (next :) <$> latexMathContents closing
    _ -> (token :) <$> latexMathContents closing

latexDisplayMathBlockSpec :: (Monad m, IsBlock il bl, HasMath bl) => BlockSpec m il bl
latexDisplayMathBlockSpec = BlockSpec
  { blockType = "LatexDisplayMath"
  , blockStart = Parsec.try do
      nonindentSpaces
      position <- Parsec.getPosition
      void (symbol '\\')
      void (symbol '[')
      contents <- Parsec.manyTill anyTok (Parsec.try (symbol '\\' *> symbol ']'))
      addNodeToStack $ Node
        (defBlockData latexDisplayMathBlockSpec)
          { blockLines = [contents]
          , blockStartPos = [position]
          }
        []
      pure BlockStartMatch
  , blockCanContain = const False
  , blockContainsLines = False
  , blockParagraph = False
  , blockContinue = const empty
  , blockConstructor = pure . displayMath . untokenize . getBlockText
  , blockFinalize = defaultFinalizer
  }

formatTelegramRichHtml :: Text -> Text
formatTelegramRichHtml input =
  case runIdentity (commonmarkWith telegramRichHtmlSyntax "telegram-message" input) of
    Left _ -> escapeHtml input
    Right html -> Text.strip (LazyText.toStrict (renderHtml html.value))

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Telegram update envelope returned by long polling.
data Update = Update
  { updateId          :: Integer
  , message           :: Maybe Message
  , editedMessage     :: Maybe Message
  , channelPost       :: Maybe Message
  , editedChannelPost :: Maybe Message
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing Update)

-- | Telegram user object fields used by the bot.
data User = User
  { id        :: !Integer
  , isBot     :: !Bool
  , firstName :: !Text
  , lastName  :: !(Maybe Text)
  , username  :: !(Maybe Text)
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing User)

-- | Telegram message object fields consumed by the unified parser.
data Message = Message
  { messageId       :: !Integer
  , messageThreadId :: !(Maybe Integer)
  , from            :: !(Maybe User)
  , senderChat      :: !(Maybe Chat)
  , chat            :: !Chat
  , replyToMessage  :: !(Maybe Message)
  , text            :: !(Maybe Text)
  , entities        :: !(Maybe [MessageEntity])
  , caption         :: !(Maybe Text)
  , captionEntities :: !(Maybe [MessageEntity])
  , photo           :: !(Maybe [PhotoSize])
  , document        :: !(Maybe TelegramMedia)
  , audio           :: !(Maybe TelegramMedia)
  , voice           :: !(Maybe TelegramMedia)
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing Message)

-- | Telegram photo variant metadata.
data PhotoSize = PhotoSize
  { fileId       :: !Text
  , fileUniqueId :: !Text
  , width        :: !Integer
  , height       :: !Integer
  , fileSize     :: !(Maybe Integer)
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing PhotoSize)

data TelegramMedia = TelegramMedia
  { fileId       :: !Text
  , fileUniqueId :: !Text
  , fileName     :: !(Maybe Text)
  , mimeType     :: !(Maybe Text)
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing TelegramMedia)

-- | Telegram message entity metadata used for mention extraction.
data MessageEntity = MessageEntity
  { type_  :: !Text
  , offset :: !Integer
  , length :: !Integer
  , url    :: !(Maybe Text)
  , language :: !(Maybe Text)
  , user   :: !(Maybe User)
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing MessageEntity)

-- | Telegram chat kind.
data ChatType
  = ChatTypePrivate
  | ChatTypeGroup
  | ChatTypeSuperGroup
  | ChatTypeChannel
  deriving (Show, Generic)

instance Aeson.ToJSON ChatType where
  toJSON ChatTypePrivate    = Aeson.String "private"
  toJSON ChatTypeGroup      = Aeson.String "group"
  toJSON ChatTypeSuperGroup = Aeson.String "supergroup"
  toJSON ChatTypeChannel    = Aeson.String "channel"

instance Aeson.FromJSON ChatType where
  parseJSON = Aeson.withText "ChatType" $ \case
    "private"    -> pure ChatTypePrivate
    "group"      -> pure ChatTypeGroup
    "supergroup" -> pure ChatTypeSuperGroup
    "channel"    -> pure ChatTypeChannel
    other        -> fail $ "Unknown ChatType: " <> show other

-- | Telegram chat object fields used for routing and metadata.
data Chat = Chat
  { id        :: !Integer
  , type_     :: !ChatType
  , title     :: !(Maybe Text)
  , username  :: !(Maybe Text)
  , firstName :: !(Maybe Text)
  , lastName  :: !(Maybe Text)
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing Chat)

-- | Telegram membership status.
data ChatMemberStatus
  = ChatMemberCreator
  | ChatMemberAdministrator
  | ChatMemberMember
  | ChatMemberRestricted
  | ChatMemberLeft
  | ChatMemberKicked
  deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (PrefixedEnumJSON "ChatMember" ChatMemberStatus)

-- | Telegram membership information returned by @getChatMember@.
data ChatMember = ChatMember
  { status :: !ChatMemberStatus
  , user   :: !User
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSON ChatMember)

-- | Telegram parse mode for outbound formatted text.
data ParseMode
  = ParseModeMarkdown
  | ParseModeMarkdownV2
  | ParseModeHTML
  deriving (Show, Generic)

instance Aeson.ToJSON ParseMode where
  toJSON ParseModeMarkdown   = Aeson.String "Markdown"
  toJSON ParseModeMarkdownV2 = Aeson.String "MarkdownV2"
  toJSON ParseModeHTML       = Aeson.String "HTML"

instance Aeson.FromJSON ParseMode where
  parseJSON = Aeson.withText "ParseMode" $ \case
    "Markdown"   -> pure ParseModeMarkdown
    "MarkdownV2" -> pure ParseModeMarkdownV2
    "HTML"       -> pure ParseModeHTML
    other        -> fail $ "Unknown ParseMode: " <> show other

-- ---------------------------------------------------------------------------
-- Requests
-- ---------------------------------------------------------------------------

data GetMeRequest = GetMeRequest
  deriving (Show)

instance Aeson.ToJSON GetMeRequest where
  toJSON _ = Aeson.object []

instance TelegramRequest GetMeRequest where
  type TelegramResponse GetMeRequest = User
  telegramMethod _ = "getMe"

newtype GetFileRequest = GetFileRequest
  { fileId :: Text
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON GetFileRequest)

instance TelegramRequest GetFileRequest where
  type TelegramResponse GetFileRequest = File
  telegramMethod _ = "getFile"

data GetUserProfilePhotosRequest = GetUserProfilePhotosRequest
  { userId :: !Integer
  , offset :: !(Maybe Int)
  , limit  :: !(Maybe Int)
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSONOmitNothing GetUserProfilePhotosRequest)

instance TelegramRequest GetUserProfilePhotosRequest where
  type TelegramResponse GetUserProfilePhotosRequest = UserProfilePhotos
  telegramMethod _ = "getUserProfilePhotos"

data UserProfilePhotos = UserProfilePhotos
  { totalCount :: !Integer
  , photos     :: ![[PhotoSize]]
  } deriving (Show, Generic)
    deriving Aeson.FromJSON via (SnakeJSON UserProfilePhotos)

data File = File
  { fileId       :: !Text
  , fileUniqueId :: !Text
  , fileSize     :: !(Maybe Integer)
  , filePath     :: !Text
  } deriving (Show, Generic)
    deriving Aeson.FromJSON via (SnakeJSON File)

data GetUpdatesRequest = GetUpdatesRequest
  { offset  :: !Int
  , timeout :: !Int
  , limit   :: !Int
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON GetUpdatesRequest)

instance TelegramRequest GetUpdatesRequest where
  type TelegramResponse GetUpdatesRequest = [Update]
  telegramMethod _ = "getUpdates"

-- | Request payload for Telegram @sendMessage@.
data SendMessageRequest = SendMessageRequest
  { chatId              :: !Integer
  , messageThreadId     :: !(Maybe Integer)
  , text                :: !Text
  , parseMode           :: !(Maybe ParseMode)
  , entities            :: !(Maybe [MessageEntity])
  , disableNotification :: !(Maybe Bool)
  , replyToMessageId    :: !(Maybe Integer)
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing SendMessageRequest)

instance TelegramRequest SendMessageRequest where
  type TelegramResponse SendMessageRequest = Message
  telegramMethod _ = "sendMessage"

data InputRichMedia = InputRichMedia
  { type_ :: !Text
  , media :: !Text
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSON InputRichMedia)

data InputRichMessageMedia = InputRichMessageMedia
  { id :: !Text
  , media :: !InputRichMedia
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSON InputRichMessageMedia)

data InputRichMessage = InputRichMessage
  { html :: !Text
  , media :: !(Maybe [InputRichMessageMedia])
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing InputRichMessage)

-- | Minimal reply target used by outgoing Telegram messages.
newtype ReplyParameters = ReplyParameters
  { messageId :: Integer
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSON ReplyParameters)

-- | Request payload for Telegram @sendRichMessage@.
data SendRichMessageRequest = SendRichMessageRequest
  { chatId              :: !Integer
  , messageThreadId     :: !(Maybe Integer)
  , richMessage         :: !InputRichMessage
  , disableNotification :: !(Maybe Bool)
  , replyParameters     :: !(Maybe ReplyParameters)
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing SendRichMessageRequest)

instance TelegramRequest SendRichMessageRequest where
  type TelegramResponse SendRichMessageRequest = Message
  telegramMethod _ = "sendRichMessage"

-- | Request payload for Telegram @editMessageText@.
data EditMessageTextRequest = EditMessageTextRequest
  { chatId      :: !Integer
  , messageId   :: !Integer
  , richMessage :: !InputRichMessage
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing EditMessageTextRequest)

instance TelegramRequest EditMessageTextRequest where
  type TelegramResponse EditMessageTextRequest = Message
  telegramMethod _ = "editMessageText"

data EditPlainMessageTextRequest = EditPlainMessageTextRequest
  { chatId    :: !Integer
  , messageId :: !Integer
  , text      :: !Text
  , parseMode :: !(Maybe ParseMode)
  , entities  :: !(Maybe [MessageEntity])
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing EditPlainMessageTextRequest)

instance TelegramRequest EditPlainMessageTextRequest where
  type TelegramResponse EditPlainMessageTextRequest = Message
  telegramMethod _ = "editMessageText"

-- | Request payload for Telegram @sendPhoto@.
data SendPhotoRequest = SendPhotoRequest
  { chatId              :: !Integer
  , messageThreadId     :: !(Maybe Integer)
  , photo               :: !Text
  , caption             :: !(Maybe Text)
  , parseMode           :: !(Maybe ParseMode)
  , captionEntities     :: !(Maybe [MessageEntity])
  , disableNotification :: !(Maybe Bool)
  , replyToMessageId    :: !(Maybe Integer)
  } deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing SendPhotoRequest)

instance TelegramRequest SendPhotoRequest where
  type TelegramResponse SendPhotoRequest = Message
  telegramMethod _ = "sendPhoto"

-- | Request payload for Telegram @sendVoice@.
data SendVoiceRequest = SendVoiceRequest
  { chatId              :: !Integer
  , messageThreadId     :: !(Maybe Integer)
  , voice               :: !Text
  , caption             :: !(Maybe Text)
  , parseMode           :: !(Maybe ParseMode)
  , captionEntities     :: !(Maybe [MessageEntity])
  , disableNotification :: !(Maybe Bool)
  , replyToMessageId    :: !(Maybe Integer)
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSONOmitNothing SendVoiceRequest)

instance TelegramRequest SendVoiceRequest where
  type TelegramResponse SendVoiceRequest = Message
  telegramMethod _ = "sendVoice"

data SendAudioRequest = SendAudioRequest
  { chatId              :: !Integer
  , messageThreadId     :: !(Maybe Integer)
  , caption             :: !(Maybe Text)
  , parseMode           :: !(Maybe ParseMode)
  , captionEntities     :: !(Maybe [MessageEntity])
  , disableNotification :: !(Maybe Bool)
  , replyToMessageId    :: !(Maybe Integer)
  } deriving (Show, Generic)

data SendVideoRequest = SendVideoRequest
  { chatId              :: !Integer
  , messageThreadId     :: !(Maybe Integer)
  , caption             :: !(Maybe Text)
  , parseMode           :: !(Maybe ParseMode)
  , captionEntities     :: !(Maybe [MessageEntity])
  , disableNotification :: !(Maybe Bool)
  , replyToMessageId    :: !(Maybe Integer)
  } deriving (Show, Generic)

-- | Request payload for Telegram @sendDocument@ multipart uploads.
data SendDocumentRequest = SendDocumentRequest
  { chatId              :: !Integer
  , messageThreadId     :: !(Maybe Integer)
  , caption             :: !(Maybe Text)
  , parseMode           :: !(Maybe ParseMode)
  , captionEntities     :: !(Maybe [MessageEntity])
  , disableNotification :: !(Maybe Bool)
  , replyToMessageId    :: !(Maybe Integer)
  } deriving (Show, Generic)

data ForwardMessageRequest = ForwardMessageRequest
  { chatId     :: !Integer
  , fromChatId :: !Integer
  , messageId  :: !Integer
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON ForwardMessageRequest)

instance TelegramRequest ForwardMessageRequest where
  type TelegramResponse ForwardMessageRequest = Message
  telegramMethod _ = "forwardMessage"

data DeleteMessageRequest = DeleteMessageRequest
  { chatId    :: !Integer
  , messageId :: !Integer
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON DeleteMessageRequest)

instance TelegramRequest DeleteMessageRequest where
  type TelegramResponse DeleteMessageRequest = Bool
  telegramMethod _ = "deleteMessage"

data PinMessageRequest = PinMessageRequest
  { chatId              :: !Integer
  , messageId           :: !Integer
  , disableNotification :: !Bool
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON PinMessageRequest)

instance TelegramRequest PinMessageRequest where
  type TelegramResponse PinMessageRequest = Bool
  telegramMethod _ = "pinChatMessage"

data UnpinMessageRequest = UnpinMessageRequest
  { chatId    :: !Integer
  , messageId :: !Integer
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON UnpinMessageRequest)

instance TelegramRequest UnpinMessageRequest where
  type TelegramResponse UnpinMessageRequest = Bool
  telegramMethod _ = "unpinChatMessage"

newtype GetChatRequest = GetChatRequest
  { chatId :: Integer
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON GetChatRequest)

instance TelegramRequest GetChatRequest where
  type TelegramResponse GetChatRequest = Chat
  telegramMethod _ = "getChat"

data GetChatMemberRequest = GetChatMemberRequest
  { chatId :: !Integer
  , userId :: !Integer
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON GetChatMemberRequest)

instance TelegramRequest GetChatMemberRequest where
  type TelegramResponse GetChatMemberRequest = ChatMember
  telegramMethod _ = "getChatMember"

data BanChatMemberRequest = BanChatMemberRequest
  { chatId    :: !Integer
  , userId    :: !Integer
  , untilDate :: !(Maybe Integer)
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSONOmitNothing BanChatMemberRequest)

instance TelegramRequest BanChatMemberRequest where
  type TelegramResponse BanChatMemberRequest = Bool
  telegramMethod _ = "banChatMember"

data UnbanChatMemberRequest = UnbanChatMemberRequest
  { chatId      :: !Integer
  , userId      :: !Integer
  , onlyIfBanned :: !Bool
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON UnbanChatMemberRequest)

instance TelegramRequest UnbanChatMemberRequest where
  type TelegramResponse UnbanChatMemberRequest = Bool
  telegramMethod _ = "unbanChatMember"

newtype LeaveChatRequest = LeaveChatRequest
  { chatId :: Integer
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON LeaveChatRequest)

instance TelegramRequest LeaveChatRequest where
  type TelegramResponse LeaveChatRequest = Bool
  telegramMethod _ = "leaveChat"

data SendChatActionRequest = SendChatActionRequest
  { chatId :: !Integer
  , action :: !ChatAction
  , messageThreadId :: !(Maybe Integer)
  , businessConnectionId :: !(Maybe Text)
  } deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSONOmitNothing SendChatActionRequest)

instance TelegramRequest SendChatActionRequest where
  type TelegramResponse SendChatActionRequest = Bool
  telegramMethod _ = "sendChatAction"

data ChatAction
  = ChatActionTyping
  | ChatActionUploadPhoto
  | ChatActionUploadVideo
  | ChatActionUploadVoice
  | ChatActionUploadDocument
  | ChatActionChooseSticker
  | ChatActionFindLocation
  | ChatActionUploadVideoNote
  deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (PrefixedEnumJSON "ChatAction" ChatAction)

getUpdates :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> Int -> Eff es [Update]
getUpdates driver offset = callTelegram driver GetUpdatesRequest
  { offset  = offset
  , timeout = telegramLongPollTimeoutSeconds
  , limit   = 100
  }

sendPhoto :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> SendPhotoRequest -> Eff es Message
sendPhoto =
  callTelegram

uploadPhoto :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> SendPhotoRequest -> FilePath -> Eff es Message
uploadPhoto driver request path =
  apiMultipartCall driver.config "sendPhoto" (sendPhotoParts request path Nothing)

uploadVoice :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> SendVoiceRequest -> FilePath -> Eff es Message
uploadVoice driver request path =
  apiMultipartCall driver.config "sendVoice" (sendVoiceParts request path Nothing)

uploadAudio :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> SendAudioRequest -> FilePath -> Maybe Text -> Eff es Message
uploadAudio driver request path fileName =
  apiMultipartCall driver.config "sendAudio" (sendAudioParts request path fileName)

uploadVideo :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> SendVideoRequest -> FilePath -> Maybe Text -> Eff es Message
uploadVideo driver request path fileName =
  apiMultipartCall driver.config "sendVideo" (sendVideoParts request path fileName)

uploadDocument :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> SendDocumentRequest -> FilePath -> Maybe Text -> Eff es Message
uploadDocument driver request path fileName =
  apiMultipartCall driver.config "sendDocument" (sendDocumentParts request path fileName)

-- | Reply to a Telegram chat, including image directives in the body.
replyToTelegram
  :: (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es)
  => TelegramDriver
  -> IncomingMessage
  -> Text
  -> Eff es (Either Text MessageId)
replyToTelegram driver message body =
  case message.chatId of
    Just chatId -> do
      let replyToMessageId = messageIdInteger =<< message.messageId
      sent <- replyTextAndImages driver chatId replyToMessageId body `catch` \(err :: TelegramException) ->
        sendTelegramFailureReply driver chatId replyToMessageId err
      pure (Right (integerMessageId sent.messageId))
    _ ->
      pure (Left "Telegram reply requires a Telegram chat id.")

sendTelegramFailureReply :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> Integer -> Maybe Integer -> TelegramException -> Eff es Message
sendTelegramFailureReply driver chatId replyToMessageId err =
  callTelegram driver SendMessageRequest
    { chatId = chatId
    , messageThreadId = Nothing
    , text = telegramFailureReplyText err
    , parseMode = Nothing
    , entities = Nothing
    , disableNotification = Nothing
    , replyToMessageId = replyToMessageId
    }

telegramFailureReplyText :: TelegramException -> Text
telegramFailureReplyText (TelegramException message) =
  "Telegram request failed: " <> message

-- | Edit a Telegram text message previously sent by this bot.
editMessageTelegram
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
  => TelegramDriver
  -> IncomingMessage
  -> MessageId
  -> Text
  -> Eff es Bool
editMessageTelegram driver message messageId body =
  case (message.chatId, messageIdInteger messageId) of
    (Just chatId, Just rawMessageId) -> do
      let text = ChatEffect.renderReplyBody body
      void $
        withRichMessageFallback text
          (callTelegram driver EditMessageTextRequest
            { chatId = chatId
            , messageId = rawMessageId
            , richMessage = InputRichMessage
                { html = formatTelegramRichHtml text
                , media = Nothing
                }
            })
          (callTelegram driver EditPlainMessageTextRequest
            { chatId = chatId
            , messageId = rawMessageId
            , text = text
            , parseMode = Nothing
            , entities = Nothing
            })
      pure True
    _ ->
      pure False

-- | Delete a Telegram message in the current chat when the bot has permission.
deleteMessageForTelegram
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
  => TelegramDriver
  -> IncomingMessage
  -> MessageId
  -> Eff es Bool
deleteMessageForTelegram driver message messageId =
  case (message.chatId, messageIdInteger messageId) of
    (Just chatId, Just rawMessageId) ->
      deleteMessage driver chatId rawMessageId
    _ ->
      pure False

-- | Reply with an HTML mention for a Telegram user id.
mentionUserTelegram
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
  => TelegramDriver
  -> IncomingMessage
  -> Text
  -> Text
  -> Eff es (Either Text MessageId)
mentionUserTelegram driver message userId body =
  case message.chatId of
    Just chatId
      | Just numericUserId <- parseIntegerUserId userId -> do
      let replyToMessageId = messageIdInteger =<< message.messageId
      sent <- callTelegram driver SendMessageRequest
        { chatId = chatId
        , messageThreadId = Nothing
        , text = telegramMentionHtml numericUserId body
        , parseMode = Just ParseModeHTML
        , entities = Nothing
        , disableNotification = Nothing
        , replyToMessageId = replyToMessageId
        }
      pure (Right (integerMessageId sent.messageId))
    _ ->
      pure (Left "Telegram mention reply requires a Telegram chat id and numeric user id.")

-- | Reply with audio as a Telegram voice message.
replyAudioTelegram
  :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es)
  => TelegramDriver
  -> IncomingMessage
  -> Text
  -> Maybe Text
  -> Eff es (Either Text MessageId)
replyAudioTelegram driver message audioRef caption =
  case message.chatId of
    Just chatId -> do
      let replyToMessageId = messageIdInteger =<< message.messageId
      sent <- sendVoiceRequest driver SendVoiceRequest
        { chatId = chatId
        , messageThreadId = Nothing
        , voice = audioRef
        , caption = nonEmptyText . ChatEffect.renderReplyBody =<< caption
        , parseMode = Nothing
        , captionEntities = Nothing
        , disableNotification = Nothing
        , replyToMessageId = replyToMessageId
        }
      pure (Right (integerMessageId sent.messageId))
    _ ->
      pure (Left "Telegram audio reply requires a Telegram chat id.")

-- | Send a file to a Telegram chat as a document.
uploadFileTelegram
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
  => TelegramDriver
  -> IncomingMessage
  -> FilePath
  -> Maybe Text
  -> Eff es (Either Text MessageId)
uploadFileTelegram driver message path fileName =
  case message.chatId of
    Just chatId -> do
      let replyToMessageId = messageIdInteger =<< message.messageId
          baseRequest =
            TelegramUploadRequest
              { chatId = chatId
              , messageThreadId = Nothing
              , caption = Nothing
              , parseMode = Nothing
              , captionEntities = Nothing
              , disableNotification = Nothing
              , replyToMessageId = replyToMessageId
              }
      sent <- uploadTelegramFileByMime driver baseRequest path fileName
      pure (Right (integerMessageId sent.messageId))
    _ ->
      pure (Left "Telegram file upload requires a Telegram chat id.")

data TelegramUploadRequest = TelegramUploadRequest
  { chatId :: !Integer
  , messageThreadId :: !(Maybe Integer)
  , caption :: !(Maybe Text)
  , parseMode :: !(Maybe ParseMode)
  , captionEntities :: !(Maybe [MessageEntity])
  , disableNotification :: !(Maybe Bool)
  , replyToMessageId :: !(Maybe Integer)
  }

uploadTelegramFileByMime :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> TelegramUploadRequest -> FilePath -> Maybe Text -> Eff es Message
uploadTelegramFileByMime driver request path fileName =
  case telegramFileKind (Mime.mimeFromName (Text.pack path)) of
    TelegramImageFile ->
      apiMultipartCall driver.config "sendPhoto" (sendPhotoParts (photoRequest request) path fileName)
    TelegramAudioFile ->
      uploadAudio driver (audioRequest request) path fileName
    TelegramVideoFile ->
      uploadVideo driver (videoRequest request) path fileName
    TelegramDocumentFile ->
      uploadDocument driver (documentRequest request) path fileName

data TelegramFileKind
  = TelegramImageFile
  | TelegramAudioFile
  | TelegramVideoFile
  | TelegramDocumentFile

telegramFileKind :: Text -> TelegramFileKind
telegramFileKind mime
  | "image/" `Text.isPrefixOf` clean = TelegramImageFile
  | "audio/" `Text.isPrefixOf` clean = TelegramAudioFile
  | "video/" `Text.isPrefixOf` clean = TelegramVideoFile
  | otherwise = TelegramDocumentFile
  where
    clean = Text.toLower (Text.takeWhile (/= ';') mime)

photoRequest :: TelegramUploadRequest -> SendPhotoRequest
photoRequest TelegramUploadRequest{..} =
  SendPhotoRequest
    { photo = "attach://photo"
    , ..
    }

audioRequest :: TelegramUploadRequest -> SendAudioRequest
audioRequest TelegramUploadRequest{..} =
  SendAudioRequest{..}

videoRequest :: TelegramUploadRequest -> SendVideoRequest
videoRequest TelegramUploadRequest{..} =
  SendVideoRequest{..}

documentRequest :: TelegramUploadRequest -> SendDocumentRequest
documentRequest TelegramUploadRequest{..} =
  SendDocumentRequest{..}

telegramMentionHtml :: Integer -> Text -> Text
telegramMentionHtml userId body =
  [i|<a href="tg://user?id=#{userId}">user</a> #{escapeHtml body}|]

escapeHtml :: Text -> Text
escapeHtml =
  Text.concatMap \case
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    '"' -> "&quot;"
    c   -> Text.singleton c

replyTextAndImages :: (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es) => TelegramDriver -> Integer -> Maybe Integer -> Text -> Eff es Message
replyTextAndImages driver chatId replyToMessageId body = do
  let text = ChatEffect.renderReplyBody body
      images = ChatEffect.replyImageUrls body
  withRichMessageFallback text
    (do
      (richMessage, mediaParts) <- prepareRichMessage driver text images
      sendPreparedRichMessage driver SendRichMessageRequest
        { chatId = chatId
        , messageThreadId = Nothing
        , richMessage = richMessage
        , disableNotification = Nothing
        , replyParameters = ReplyParameters <$> replyToMessageId
        } mediaParts)
    (legacyReply text images)
  where
    legacyReply text = \case
      [] ->
        callTelegram driver SendMessageRequest
            { chatId = chatId
            , messageThreadId = Nothing
            , text = text
            , parseMode = Nothing
            , entities = Nothing
            , disableNotification = Nothing
            , replyToMessageId = replyToMessageId
            }
      firstImage : restImages -> do
        firstSent <- sendImageRequest driver SendPhotoRequest
          { chatId = chatId
          , messageThreadId = Nothing
          , photo = firstImage
          , caption = nonEmptyText text
          , parseMode = Nothing
          , captionEntities = Nothing
          , disableNotification = Nothing
          , replyToMessageId = replyToMessageId
          }
        traverse_ (sendImage Nothing) restImages
        pure firstSent
    sendImage caption photo = void $ sendImageRequest driver SendPhotoRequest
        { chatId = chatId
        , messageThreadId = Nothing
        , photo = photo
        , caption = caption
        , parseMode = Nothing
        , captionEntities = Nothing
        , disableNotification = Nothing
        , replyToMessageId = Nothing
        }

data PreparedRichMedia = PreparedRichMedia
  { block :: !Text
  , input :: !(Maybe InputRichMessageMedia)
  , part :: !(Maybe Multipart.Part)
  }

prepareRichMessage
  :: (Media.Media :> es)
  => TelegramDriver
  -> Text
  -> [Text]
  -> Eff es (InputRichMessage, [Multipart.Part])
prepareRichMessage driver text refs = do
  prepared <- zipWithM (prepareRichPhoto driver) [(1 :: Int)..] refs
  let html = Text.intercalate "\n" $ filter (not . Text.null) (formatTelegramRichHtml text : map (.block) prepared)
      media = viaNonEmpty toList (mapMaybe (.input) prepared)
  pure
    ( InputRichMessage{html, media}
    , mapMaybe (.part) prepared
    )

prepareRichPhoto :: Media.Media :> es => TelegramDriver -> Int -> Text -> Eff es PreparedRichMedia
prepareRichPhoto driver index ref =
  Media.platformMediaRef "telegram" (telegramMediaScope driver) ref >>= \case
    Just telegramFileId ->
      pure (referenced telegramFileId Nothing)
    Nothing ->
      Media.localMediaPath ref >>= \case
        Just path ->
          let attachment = "rich-media-" <> show index
          in pure (referenced ("attach://" <> attachment) (Just (telegramFilePart attachment path Nothing)))
        Nothing ->
          pure PreparedRichMedia
            { block = [i|<img src="#{escapeHtml ref}"/>|]
            , input = Nothing
            , part = Nothing
            }
  where
    mediaId = "media-" <> show index
    referenced mediaRef part = PreparedRichMedia
      { block = [i|<img src="tg://photo?id=#{mediaId}"/>|]
      , input = Just InputRichMessageMedia
          { id = mediaId
          , media = InputRichMedia
              { type_ = "photo"
              , media = mediaRef
              }
          }
      , part = part
      }

sendPreparedRichMessage
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
  => TelegramDriver
  -> SendRichMessageRequest
  -> [Multipart.Part]
  -> Eff es Message
sendPreparedRichMessage driver request mediaParts
  | null mediaParts =
      callTelegram driver request
  | otherwise =
      apiMultipartCall driver.config "sendRichMessage" (richMessageParts request <> mediaParts)

richMessageParts :: SendRichMessageRequest -> [Multipart.Part]
richMessageParts SendRichMessageRequest{..} =
  [ textPart "chat_id" (show chatId)
  , textPart "rich_message" (jsonText richMessage)
  ]
    <> maybePart "message_thread_id" (show <$> messageThreadId)
    <> maybePart "disable_notification" (boolText <$> disableNotification)
    <> maybePart "reply_parameters" (jsonText <$> replyParameters)

sendImageRequest :: (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es) => TelegramDriver -> SendPhotoRequest -> Eff es Message
sendImageRequest driver request =
  Media.platformMediaRef "telegram" (telegramMediaScope driver) originalPhoto >>= \case
    Just telegramFileId ->
      sendPhoto driver (replacePhoto telegramFileId request)
    Nothing ->
      case localFilePhoto originalPhoto of
        Just path ->
          uploadAndRemember path
        Nothing ->
          Media.localMediaPath originalPhoto >>= \case
            Just path ->
              uploadAndRemember path
            Nothing ->
              case dataImagePhoto originalPhoto of
                Just bytes -> uploadTemporaryPhoto driver request bytes
                Nothing -> do
                  sent <- sendPhoto driver request
                  rememberTelegramPhotoRef originalPhoto sent
                  pure sent
  where
    originalPhoto =
      request.photo

    uploadAndRemember path = do
      sent <- uploadPhoto driver request path
      rememberTelegramPhotoRef originalPhoto sent
      pure sent

    rememberTelegramPhotoRef ref sent =
      when (telegramCacheablePhotoRef ref) do
        for_ (sent.photo >>= largestPhotoFileId) \telegramFileId ->
          Media.storePlatformMediaRef "telegram" (telegramMediaScope driver) ref telegramFileId

telegramMediaScope :: TelegramDriver -> Text
telegramMediaScope driver =
  fromMaybe "default" $
    (("id:" <>) . show <$> viaNonEmpty head driver.config.botIds) <|>
      (("username:" <>) <$> viaNonEmpty head driver.config.botUsernames)

telegramCacheablePhotoRef :: Text -> Bool
telegramCacheablePhotoRef ref =
  let stripped = Text.strip ref
      lower = Text.toLower stripped
  in not (Text.null stripped) && not ("data:image/" `Text.isPrefixOf` lower)

replacePhoto :: Text -> SendPhotoRequest -> SendPhotoRequest
replacePhoto newPhoto SendPhotoRequest{..} =
  SendPhotoRequest{photo = newPhoto, ..}

sendVoiceRequest :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es) => TelegramDriver -> SendVoiceRequest -> Eff es Message
sendVoiceRequest driver request =
  case localFileRef request.voice of
    Just path -> uploadVoice driver request path
    Nothing ->
      case dataAudioBytes request.voice of
        Just bytes -> uploadTemporaryVoice driver request bytes
        Nothing    -> callTelegram driver request

localFilePhoto :: Text -> Maybe FilePath
localFilePhoto photo =
  localFileRef photo

localFileRef :: Text -> Maybe FilePath
localFileRef ref =
  let stripped = Text.strip ref
  in case Text.stripPrefix "file://" stripped of
    Just path ->
      Just (Text.unpack path)
    Nothing
      | isLocalPathRef stripped ->
          Just (Text.unpack stripped)
      | otherwise ->
          Nothing

isLocalPathRef :: Text -> Bool
isLocalPathRef ref =
  "/" `Text.isPrefixOf` ref || "./" `Text.isPrefixOf` ref || "../" `Text.isPrefixOf` ref

dataImagePhoto :: Text -> Maybe ByteString.ByteString
dataImagePhoto photo = do
  rest <- Text.stripPrefix "data:image/" (Text.strip photo)
  let (_, encodedWithMarker) = Text.breakOn ";base64," rest
  encoded <- Text.stripPrefix ";base64," encodedWithMarker
  either (const Nothing) Just (Base64.decode (TextEncoding.encodeUtf8 encoded))

dataAudioBytes :: Text -> Maybe ByteString.ByteString
dataAudioBytes ref = do
  rest <- Text.stripPrefix "data:audio/" (Text.strip ref)
  let (_, encodedWithMarker) = Text.breakOn ";base64," rest
  encoded <- Text.stripPrefix ";base64," encodedWithMarker
  either (const Nothing) Just (Base64.decode (TextEncoding.encodeUtf8 encoded))

uploadTemporaryPhoto :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es) => TelegramDriver -> SendPhotoRequest -> ByteString.ByteString -> Eff es Message
uploadTemporaryPhoto driver request bytes = do
  withTemporaryTelegramFile "telegram-photo" "png" bytes (uploadPhoto driver request)

uploadTemporaryVoice :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es) => TelegramDriver -> SendVoiceRequest -> ByteString.ByteString -> Eff es Message
uploadTemporaryVoice driver request bytes = do
  withTemporaryTelegramFile "telegram-voice" "ogg" bytes (uploadVoice driver request)

withTemporaryTelegramFile :: (FileSystem :> es, IOE :> es) => FilePath -> FilePath -> ByteString.ByteString -> (FilePath -> Eff es a) -> Eff es a
withTemporaryTelegramFile prefix extension bytes action =
  Temporary.runTemporary $
    Temporary.withSystemTempDirectory "cosmobot-telegram-" \dir -> do
      let path = dir </> (prefix <.> extension)
      FileSystemByteString.writeFile path bytes
      raise (action path)

nonEmptyText :: Text -> Maybe Text
nonEmptyText text
  | Text.null text = Nothing
  | otherwise      = Just text

deleteMessage :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> Integer -> Integer -> Eff es Bool
deleteMessage driver chatId messageId =
  callTelegram driver DeleteMessageRequest{..}

getChatMember :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> Integer -> Integer -> Eff es ChatMember
getChatMember driver chatId userId =
  callTelegram driver GetChatMemberRequest{..}

getUserAvatar :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => TelegramDriver -> Integer -> Eff es (Maybe Aeson.Value)
getUserAvatar driver userId = do
  profilePhotos <- callTelegram driver GetUserProfilePhotosRequest
    { userId = userId
    , offset = Nothing
    , limit = Just 1
    }
  case profilePhotos.photos >>= maybeToList . largestPhoto of
    [] ->
      pure Nothing
    photo : _ -> do
      avatarUrl <- fileUrl driver photo.fileId
      pure $ Just $ Aeson.object
        [ "platform" Aeson..= ("telegram" :: Text)
        , "user_id" Aeson..= userId
        , "avatar_url" Aeson..= avatarUrl
        , "file_id" Aeson..= photo.fileId
        , "width" Aeson..= photo.width
        , "height" Aeson..= photo.height
        ]

parseIntegerUserId :: Text -> Maybe Integer
parseIntegerUserId raw =
  case reads (Text.unpack (Text.strip raw)) of
    [(userId, "")] ->
      Just userId
    _ ->
      Nothing
