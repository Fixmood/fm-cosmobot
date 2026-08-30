{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.Chat.Driver.Discord
Description : Discord Gateway and REST chat driver
Stability   : experimental
-}

module Bot.Chat.Driver.Discord
  ( DiscordDriver
  , newDiscordDriver
  , runDiscordDriver
  , Config (..)
  , GatewayEnvelope (..)
  , GatewayHello (..)
  , Message (..)
  , DeletedMessage (..)
  , User (..)
  , Attachment (..)
  , Embed (..)
  , EmbedImage (..)
  , Member (..)
  , Reference (..)
  , CreateMessageRequest (..)
  , incomingMessages
  , eventToIncomingMessage
  , eventToIncomingMessageWith
  , deletedEventToIncomingMessageWith
  , formatDiscordMarkdown
  , discordUserAvatarValue
  )
where

import qualified Bot.Chat.Driver.Types as Driver
import qualified Bot.Effect.Chat as Chat
import Bot.Core.Message
import Bot.Prelude
import Bot.Util.Aeson
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Media as Media
import Commonmark
import qualified Commonmark.Entity as Commonmark
import Commonmark.Extensions
import qualified Control.Concurrent.Chan as Chan
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Bits (shiftR)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.MVar as MVar
import Effectful.FileSystem (FileSystem)
import qualified Network.Connection as Connection
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.MultipartFormData as Multipart
import Network.HTTP.Req
import qualified Network.TLS as TLS
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WSStream
import qualified Streaming as S
import qualified Streaming.Prelude as S
import System.IO.Error (userError)

data Config = Config
  { botToken :: !Text
  , botId :: !(Maybe Text)
  , applicationId :: !(Maybe Text)
  , allowedGuilds :: ![Integer]
  , allowedChannels :: ![Integer]
  , allowedUsers :: ![Text]
  , superusers :: ![Text]
  , gatewayHost :: !String
  , gatewayPath :: !String
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig = Config
  { botToken = ""
  , botId = Nothing
  , applicationId = Nothing
  , allowedGuilds = []
  , allowedChannels = []
  , allowedUsers = []
  , superusers = []
  , gatewayHost = "gateway.discord.gg"
  , gatewayPath = "/?v=10&encoding=json"
  }

data DiscordDriver = DiscordDriver
  { config :: !Config
  , eventChan :: !(Chan.Chan GatewayEvent)
  }

data GatewayEvent
  = GatewayMessageCreated !Message
  | GatewayMessageDeleted !DeletedMessage

newDiscordDriver :: IOE :> es => Config -> Eff es DiscordDriver
newDiscordDriver config = do
  eventChan <- liftIO Chan.newChan
  pure DiscordDriver{config, eventChan}

instance Driver.ChatDriver DiscordDriver where
  type ChatDriverEffects DiscordDriver es = (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrency.Concurrency :> es, Media.Media :> es)

  driverPlatform _ =
    PlatformDiscord

  sendReplyMessage =
    replyToDiscord

  replyAudio =
    replyAudioDiscord

  uploadFile =
    uploadFileDiscord

  editMessage driver message messageId body =
    editMessageDiscord driver message messageId body

  deleteMessage =
    deleteMessageDiscord

  messageOutPolicy _ _ =
    pure (Chat.EditableMessage discordEditChunkChars discordMessageTextLimit)

  getMessageContent =
    getMessageContentDiscord

  getSenderMemberInfo driver message =
      case (discordMessageGuildId message.raw, message.senderId) of
        (Just guildId, Just userId) ->
          Just <$> getGuildMember driver guildId userId
        _ ->
          pure Nothing

  getMemberInfo driver message userId =
      case discordMessageGuildId message.raw of
        Just guildId ->
          Just <$> getGuildMember driver guildId userId
        _ ->
          pure Nothing

  getUserAvatar driver _ userId = do
    value <- getUser driver userId
    pure (discordUserAvatarValue =<< Aeson.parseMaybe Aeson.parseJSON value)

  listGroupMembers driver message =
      case discordMessageGuildId message.raw of
        Just guildId ->
          Just <$> listGuildMembers driver guildId
        _ ->
          pure Nothing

  mentionUser =
    mentionUserDiscord

  setTyping driver message _timeoutMillis =
      case discordChannelId message of
        Just channelId ->
          triggerTyping driver channelId
        _ ->
          pure ()

discordEditChunkChars :: Int
discordEditChunkChars = 512

discordMessageTextLimit :: Int
discordMessageTextLimit = 2000

receiveEvent :: IOE :> es => DiscordDriver -> Eff es GatewayEvent
receiveEvent driver =
  liftIO (Chan.readChan driver.eventChan)

createMessage :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> Text -> CreateMessageRequest -> Eff es Message
createMessage driver channelId request =
  discordJsonRequest driver.config POST ["channels", channelId, "messages"] request

editDiscordMessage :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> Text -> Text -> CreateMessageRequest -> Eff es Message
editDiscordMessage driver channelId messageId request =
  discordJsonRequest driver.config PATCH ["channels", channelId, "messages", messageId] request

deleteDiscordMessage :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> Text -> Text -> Eff es ()
deleteDiscordMessage driver channelId messageId =
  discordNoResponseRequest driver.config DELETE ["channels", channelId, "messages", messageId]

fetchMessage :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> Text -> Text -> Eff es Message
fetchMessage driver channelId messageId =
  discordGetRequest driver.config ["channels", channelId, "messages", messageId]

getUser :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> Text -> Eff es Aeson.Value
getUser driver userId =
  discordGetRequest driver.config ["users", userId]

getGuildMember :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> Text -> Text -> Eff es Aeson.Value
getGuildMember driver guildId userId =
  discordGetRequest driver.config ["guilds", guildId, "members", userId]

listGuildMembers :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> Text -> Eff es Aeson.Value
listGuildMembers driver guildId =
  discordGetRequest driver.config ["guilds", guildId, "members"]

uploadDiscordFile :: (HTTP.HTTP :> es, KatipE :> es, IOE :> es) => DiscordDriver -> Text -> Maybe Text -> FilePath -> Maybe Text -> Eff es Message
uploadDiscordFile driver channelId content path fileName =
  discordUploadFile driver.config channelId content path fileName

triggerTyping :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> Text -> Eff es ()
triggerTyping driver channelId =
  discordNoResponseRequest driver.config POST ["channels", channelId, "typing"]

runDiscordDriver
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Concurrency.Concurrency :> es)
  => DiscordDriver
  -> Eff es a
  -> Eff es a
runDiscordDriver driver inner = do
  let cfg = driver.config
      eventChan = driver.eventChan
  if discordEnabled cfg
    then do
      Concurrency.withWorker "discord.gateway" (discordConnectionLoop cfg eventChan) inner
    else inner

discordEnabled :: Config -> Bool
discordEnabled cfg =
  not (Text.null (Text.strip cfg.botToken))

discordConnectionLoop
  :: (IOE :> es, KatipE :> es, Concurrent :> es, Concurrency.Concurrency :> es)
  => Config
  -> Chan.Chan GatewayEvent
  -> Eff es ()
discordConnectionLoop cfg eventChan =
  forever do
    result <- runDiscordConnectionOnce cfg eventChan
    case result of
      Right () ->
        logInfo "Discord gateway disconnected; reconnecting"
      Left err ->
        logInfo [i|Discord gateway failed; reconnecting: #{err}|]
    threadDelay discordReconnectDelayMicroseconds

runDiscordConnectionOnce
  :: (IOE :> es, KatipE :> es, Concurrent :> es, Concurrency.Concurrency :> es)
  => Config
  -> Chan.Chan GatewayEvent
  -> Eff es (Either String ())
runDiscordConnectionOnce cfg eventChan =
  (Right <$> withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
    liftIO $ runSecureWebSocketClient cfg.gatewayHost cfg.gatewayPath \conn ->
      runInIO (runGatewayConnection cfg eventChan conn))
    `catch` \(connectionErr :: WS.ConnectionException) ->
      pure (Left (show connectionErr))
    `catch` \(handshakeErr :: WS.HandshakeException) ->
      pure (Left (show handshakeErr))
    `catch` \(ioErr :: IOException) ->
      pure (Left (show ioErr))
    `catchSync` \err ->
      pure (Left (displayException err))

runSecureWebSocketClient :: String -> String -> WS.ClientApp a -> IO a
runSecureWebSocketClient host path app = do
  context <- Connection.initConnectionContext
  conn <- Connection.connectTo context Connection.ConnectionParams
    { Connection.connectionHostname = host
    , Connection.connectionPort = 443
    , Connection.connectionUseSecure = Just discordTlsSettings
    , Connection.connectionUseSocks = Nothing
    }
  stream <- WSStream.makeStream
    (Just <$> Connection.connectionGetChunk conn)
    (maybe (Connection.connectionClose conn) (Connection.connectionPut conn . LazyByteString.toStrict))
  result <- WS.runClientWithStream stream host path WS.defaultConnectionOptions [] app
  Connection.connectionClose conn
  pure result

discordTlsSettings :: Connection.TLSSettings
discordTlsSettings =
  Connection.TLSSettingsSimple
    { Connection.settingDisableCertificateValidation = False
    , Connection.settingDisableSession = False
    , Connection.settingUseServerName = True
    , Connection.settingClientSupported =
        TLS.defaultSupported
          { TLS.supportedExtendedMainSecret = TLS.AllowEMS
          }
    }

runGatewayConnection
  :: (IOE :> es, KatipE :> es, Concurrent :> es, Concurrency.Concurrency :> es)
  => Config
  -> Chan.Chan GatewayEvent
  -> WS.Connection
  -> Eff es ()
runGatewayConnection cfg eventChan conn = do
  firstEnvelope <- readGatewayEnvelope conn
  case firstEnvelope.op of
    10 -> do
      hello :: GatewayHello <- parseGatewayData "Discord hello" firstEnvelope.d
      logInfo "Discord gateway connected"
      lastSequence <- MVar.newMVar firstEnvelope.s
      heartbeatAck <- MVar.newMVar True
      identifyGateway cfg conn
      runDiscordGatewaySession eventChan lastSequence heartbeatAck hello.heartbeatInterval conn
    op ->
      logInfo [i|Discord gateway expected HELLO, got op=#{op}|]

runDiscordGatewaySession
  :: (IOE :> es, KatipE :> es, Concurrent :> es, Concurrency.Concurrency :> es)
  => Chan.Chan GatewayEvent
  -> MVar.MVar (Maybe Int)
  -> MVar.MVar Bool
  -> Int
  -> WS.Connection
  -> Eff es ()
runDiscordGatewaySession eventChan lastSequence heartbeatAck heartbeatInterval conn = do
  done <- MVar.newEmptyMVar
  heartbeat <- forkGatewayThread "heartbeat" done (heartbeatLoop conn lastSequence heartbeatAck heartbeatInterval)
  eventReader <- forkGatewayThread "reader" done (readGatewayEvents eventChan lastSequence heartbeatAck conn)
  reason <- MVar.takeMVar done
  logInfo [i|Discord gateway connection ending: #{displayException reason}|]
  closeDiscordGatewayForReconnect conn
  void $ Concurrency.cancel heartbeat.handleId
  void $ Concurrency.cancel eventReader.handleId
  throwIO reason

forkGatewayThread
  :: (Concurrency.Concurrency :> es, Concurrent :> es)
  => Text
  -> MVar.MVar SomeException
  -> Eff es ()
  -> Eff es Concurrency.Handle
forkGatewayThread label done action = Concurrency.fork [i|discord.gateway.#{label}|] do
  result <- try action
  case result of
    Left err ->
      void (MVar.tryPutMVar done err)
    Right () ->
      void (MVar.tryPutMVar done (toException ThreadKilled))

closeDiscordGatewayForReconnect :: (IOE :> es, KatipE :> es) => WS.Connection -> Eff es ()
closeDiscordGatewayForReconnect conn =
  trySync (liftIO $ WS.sendClose conn ("reconnect" :: Text)) >>= \case
    Left err ->
      logDebug [i|Discord gateway close during reconnect failed: #{show err :: String}|]
    Right () ->
      pure ()

heartbeatLoop
  :: (IOE :> es, KatipE :> es, Concurrent :> es)
  => WS.Connection
  -> MVar.MVar (Maybe Int)
  -> MVar.MVar Bool
  -> Int
  -> Eff es ()
heartbeatLoop conn lastSequence heartbeatAck intervalMs = forever do
  threadDelay (intervalMs * 1000)
  acked <- MVar.swapMVar heartbeatAck False
  if acked
    then do
      sequenceNumber <- MVar.readMVar lastSequence
      liftIO $ WS.sendTextData conn (Aeson.encode (heartbeatPayload sequenceNumber))
    else do
      logInfo "Discord gateway heartbeat ACK timed out; closing connection"
      liftIO $ WS.sendClose conn ("heartbeat ACK timeout" :: Text)
      throwIO (userError "Discord gateway heartbeat ACK timed out")

readGatewayEvents
  :: (IOE :> es, KatipE :> es, Concurrent :> es)
  => Chan.Chan GatewayEvent
  -> MVar.MVar (Maybe Int)
  -> MVar.MVar Bool
  -> WS.Connection
  -> Eff es ()
readGatewayEvents eventChan lastSequence heartbeatAck conn = forever do
  envelope <- readGatewayEnvelope conn
  updateLastSequence lastSequence envelope.s
  case (envelope.op, envelope.t) of
    (0, Just "MESSAGE_CREATE") -> do
      message :: Message <- parseGatewayData "Discord message create" envelope.d
      liftIO $ Chan.writeChan eventChan (GatewayMessageCreated message)
    (0, Just "MESSAGE_DELETE") -> do
      deleted :: DeletedMessage <- parseGatewayData "Discord message delete" envelope.d
      liftIO $ Chan.writeChan eventChan (GatewayMessageDeleted deleted)
    (1, _) -> do
      sequenceNumber <- MVar.readMVar lastSequence
      liftIO $ WS.sendTextData conn (Aeson.encode (heartbeatPayload sequenceNumber))
    (7, _) ->
      throwIO (userError "Discord gateway requested reconnect")
    (9, _) ->
      throwIO (userError "Discord gateway invalid session")
    (11, _) ->
      void $ MVar.swapMVar heartbeatAck True
    _ ->
      pure ()

updateLastSequence :: Concurrent :> es => MVar.MVar (Maybe Int) -> Maybe Int -> Eff es ()
updateLastSequence lastSequence =
  traverse_ \sequenceNumber ->
    void $ MVar.swapMVar lastSequence (Just sequenceNumber)

readGatewayEnvelope :: (IOE :> es, KatipE :> es) => WS.Connection -> Eff es GatewayEnvelope
readGatewayEnvelope conn = do
  bytes <- liftIO (WS.receiveData conn :: IO ByteString.ByteString)
  case Aeson.eitherDecodeStrict bytes of
    Right envelope ->
      pure envelope
    Left err -> do
      logInfo [i|Ignoring malformed Discord gateway frame: #{Text.pack err}|]
      readGatewayEnvelope conn

identifyGateway :: IOE :> es => Config -> WS.Connection -> Eff es ()
identifyGateway cfg conn =
  liftIO $ WS.sendTextData conn (Aeson.encode (identifyPayload cfg))

heartbeatPayload :: Maybe Int -> Aeson.Value
heartbeatPayload sequenceNumber =
  Aeson.object
    [ "op" Aeson..= (1 :: Int)
    , "d" Aeson..= sequenceNumber
    ]

identifyPayload :: Config -> Aeson.Value
identifyPayload cfg =
  Aeson.object
    [ "op" Aeson..= (2 :: Int)
    , "d" Aeson..= Aeson.object
        [ "token" Aeson..= cfg.botToken
        , "intents" Aeson..= discordGatewayIntents
        , "properties" Aeson..= Aeson.object
            [ "os" Aeson..= ("linux" :: Text)
            , "browser" Aeson..= ("cosmobot" :: Text)
            , "device" Aeson..= ("cosmobot" :: Text)
            ]
        ]
    ]

discordGatewayIntents :: Int
discordGatewayIntents =
  512 + 4096 + 32768

discordReconnectDelayMicroseconds :: Int
discordReconnectDelayMicroseconds =
  5 * 1000000

incomingMessages :: (IOE :> es, KatipE :> es) => DiscordDriver -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessages driver = do
  if discordEnabled driver.config
    then incomingMessagesLoop driver
    else S.lift $ logInfo "Discord driver disabled: no bot token configured"

incomingMessagesLoop :: (IOE :> es, KatipE :> es) => DiscordDriver -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessagesLoop driver = do
  event <- S.lift (receiveEvent driver)
  let incoming = case event of
        GatewayMessageCreated message -> eventToIncomingMessageWith driver.config message
        GatewayMessageDeleted deleted -> Just (deletedEventToIncomingMessageWith driver.config deleted)
  case incoming of
    Nothing -> do
      S.lift $ logDebug "Ignoring Discord event"
      S.lift $ logInfo "Ignoring Discord event"
    Just message -> do
      S.lift $ logDebug [i|incoming Discord message: #{show message :: String}|]
      S.lift $ logInfo [i|incoming Discord message: #{incomingMessageLogLine message}|]
      S.yield message
  incomingMessagesLoop driver

eventToIncomingMessage :: Message -> Maybe IncomingMessage
eventToIncomingMessage =
  eventToIncomingMessageWith defaultConfig

eventToIncomingMessageWith :: Config -> Message -> Maybe IncomingMessage
eventToIncomingMessageWith cfg message = do
  guard (not (isOwnMessage cfg message))
  guard (not message.author.bot)
  guard (not (Text.null (Text.strip message.content)) || not (null message.attachments))
  pure IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = PlatformDiscord
    , kind = if isJust message.guildId then ChatGroup else ChatPrivate
    , chatId = Just (discordSnowflakeNumber message.channelId)
    , chatAliases = catMaybes [Just message.channelId, message.guildId]
    , digest = discordMessageDigest cfg message
    , senderId = Just message.author.id
    , senderUsername = message.author.globalName <|> message.author.username
    , messageId = Just (textMessageId message.id)
    , replyToMessageId = message.referencedMessage <&> (.id) <&> textMessageId
    , mentions = map (.id) message.mentions
    , mentionUsernames = mapMaybe (.username) message.mentions
    , imageUrls = messageImageUrls message
    , files = messageFiles message
    , text = Text.strip message.content
    , raw = message.raw
    }

deletedEventToIncomingMessageWith :: Config -> DeletedMessage -> IncomingMessage
deletedEventToIncomingMessageWith cfg deleted =
  IncomingMessage
    { eventKind = IncomingMessageDeleted
    , platform = PlatformDiscord
    , kind = if isJust deleted.guildId then ChatGroup else ChatPrivate
    , chatId = Just (discordSnowflakeNumber deleted.channelId)
    , chatAliases = catMaybes [Just deleted.channelId, deleted.guildId]
    , digest = emptyMessageDigest
        { chatIsAllowed =
            discordSnowflakeNumber deleted.channelId `elem` cfg.allowedChannels
              || maybe False ((`elem` cfg.allowedGuilds) . discordSnowflakeNumber) deleted.guildId
        , botId = cfg.botId
        }
    , senderId = Nothing
    , senderUsername = Nothing
    , messageId = Just (textMessageId deleted.id)
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , files = []
    , text = ""
    , raw = deleted.raw
    }

discordMessageDigest :: Config -> Message -> MessageDigest
discordMessageDigest cfg message =
  MessageDigest
    { chatIsAllowed = chatAllowed
    , senderIsAllowed = if isNothing message.guildId then (chatAllowed || senderAllowed || senderSuperuser) else senderSuperuser
    , senderIsSuperuser = senderSuperuser
    , mentionsBot = maybe False (`elem` map (.id) message.mentions) cfg.botId
    , botId = cfg.botId
    }
  where
    chatAllowed =
      discordSnowflakeNumber message.channelId `elem` cfg.allowedChannels ||
        maybe False ((`elem` cfg.allowedGuilds) . discordSnowflakeNumber) message.guildId
    senderAllowed =
      message.author.id `elem` cfg.allowedUsers
    senderSuperuser =
      message.author.id `elem` cfg.superusers

replyToDiscord
  :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es, Media.Media :> es)
  => DiscordDriver
  -> IncomingMessage
  -> Text
  -> Eff es (Either Text MessageId)
replyToDiscord driver message body =
  case discordChannelId message of
    Just channelId -> do
      let text = formatDiscordMarkdown (Chat.renderReplyBody body)
          imageRefs = Chat.replyImageUrls body
          request = createMessageRequest text (discordReplyReference message)
      sentText <- if Text.null (Text.strip text)
        then pure Nothing
        else Just <$> createMessage driver channelId request
      sentImages <- traverse (sendDiscordImage driver channelId (discordReplyReference message)) imageRefs
      pure case textMessageId . (.id) <$> sentText <|> (textMessageId . (.id) <$> viaNonEmpty head sentImages) of
        Just messageId ->
          Right messageId
        Nothing ->
          Left "Discord reply did not send any message."
    _ ->
      pure (Left "Discord reply requires a Discord channel id.")

sendDiscordImage :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es, Media.Media :> es) => DiscordDriver -> Text -> Maybe Reference -> Text -> Eff es Message
sendDiscordImage driver channelId replyReference imageRef = do
  resolvedRef <- discordImageRef imageRef
  case discordRemoteImageRef resolvedRef of
    Just url ->
      createMessage driver channelId (createMessageRequest url replyReference)
    Nothing ->
      withDiscordImageFile resolvedRef \path ->
        uploadDiscordFile driver channelId Nothing path Nothing

editMessageDiscord :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> IncomingMessage -> MessageId -> Text -> Eff es Bool
editMessageDiscord driver message messageId body =
  case discordChannelId message of
    Just channelId -> do
      void $ editDiscordMessage driver channelId (messageIdText messageId) (createMessageRequest (formatDiscordMarkdown (Chat.renderReplyBody body)) Nothing)
      pure True
    _ ->
      pure False

deleteMessageDiscord :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> IncomingMessage -> MessageId -> Eff es Bool
deleteMessageDiscord driver message messageId =
  case discordChannelId message of
    Just channelId -> do
      deleteDiscordMessage driver channelId (messageIdText messageId)
      pure True
    _ ->
      pure False

getMessageContentDiscord :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> IncomingMessage -> MessageId -> Eff es (Maybe ReferencedMessage)
getMessageContentDiscord driver message messageId =
  case discordChannelId message of
    Just channelId -> do
      fetched <- fetchMessage driver channelId (messageIdText messageId)
      pure (Just ReferencedMessage
        { messageId = Just (textMessageId fetched.id)
        , senderDisplayName = fetched.author.globalName <|> fetched.author.username
        , senderIdentifier = Just fetched.author.id
        , senderIsBot = fetched.author.bot
        , text = fetched.content
        , imageUrls = messageImageUrls fetched
        , files = messageFiles fetched
        })
    _ ->
      pure Nothing

mentionUserDiscord :: (HTTP.HTTP :> es, KatipE :> es) => DiscordDriver -> IncomingMessage -> Text -> Text -> Eff es (Either Text MessageId)
mentionUserDiscord driver message userId body =
  case discordChannelId message of
    Just channelId -> do
      sent <- createMessage driver channelId (createMessageRequest ([i|<@#{userId}> #{formatDiscordMarkdown body}|]) (discordReplyReference message))
      pure (Right (textMessageId sent.id))
    _ ->
      pure (Left "Discord mention reply requires a Discord channel id.")

replyAudioDiscord :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es) => DiscordDriver -> IncomingMessage -> Text -> Maybe Text -> Eff es (Either Text MessageId)
replyAudioDiscord driver message audioRef caption =
  case discordChannelId message of
    Just channelId -> do
      sent <- withDiscordImageFile audioRef \path ->
        uploadDiscordFile driver channelId (formatDiscordMarkdown . Chat.renderReplyBody <$> caption) path Nothing
      pure (Right (textMessageId sent.id))
    _ ->
      pure (Left "Discord audio reply requires a Discord channel id.")

uploadFileDiscord :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => DiscordDriver -> IncomingMessage -> FilePath -> Maybe Text -> Eff es (Either Text MessageId)
uploadFileDiscord driver message path fileName =
  case discordChannelId message of
    Just channelId -> do
      sent <- uploadDiscordFile driver channelId Nothing path fileName
      pure (Right (textMessageId sent.id))
    _ ->
      pure (Left "Discord file upload requires a Discord channel id.")

discordChannelId :: IncomingMessage -> Maybe Text
discordChannelId message =
  viaNonEmpty head message.chatAliases

discordReplyReference :: IncomingMessage -> Maybe Reference
discordReplyReference message =
  Reference <$> message.messageId <*> discordChannelId message <*> Just (discordMessageGuildId message.raw)

createMessageRequest :: Text -> Maybe Reference -> CreateMessageRequest
createMessageRequest content messageReference =
  CreateMessageRequest
    { content = nonEmptyDiscordContent content
    , messageReference
    , allowedMentions = AllowedMentions
        { parse = []
        , users = []
        , repliedUser = Just False
        }
    }

nonEmptyDiscordContent :: Text -> Text
nonEmptyDiscordContent content
  | Text.null (Text.strip content) = " "
  | otherwise = Text.take discordMessageTextLimit content

newtype DiscordMarkdown = DiscordMarkdown
  { discordMarkdownText :: Text
  }
  deriving (Show, Eq, Typeable)

instance Semigroup DiscordMarkdown where
  DiscordMarkdown left <> DiscordMarkdown right =
    DiscordMarkdown (left <> right)

instance Monoid DiscordMarkdown where
  mempty =
    DiscordMarkdown ""

instance Rangeable DiscordMarkdown where
  ranged _ = id

instance HasAttributes DiscordMarkdown where
  addAttributes _ = id

instance ToPlainText DiscordMarkdown where
  toPlainText =
    (.discordMarkdownText)

instance IsInline DiscordMarkdown where
  lineBreak = discordMarkdownTextOnly "\n"
  softBreak = discordMarkdownTextOnly "\n"
  str = discordMarkdownTextOnly . escapeDiscordMarkdown
  entity raw =
    discordMarkdownTextOnly (escapeDiscordMarkdown (fromMaybe raw (Commonmark.lookupEntity (Text.drop 1 raw))))
  escapedChar = discordMarkdownTextOnly . escapeDiscordMarkdown . Text.singleton
  emph body = discordMarkdownWrap "*" "*" body
  strong body = discordMarkdownWrap "**" "**" body
  link target _title body
    | Text.null body.discordMarkdownText = discordMarkdownTextOnly target
    | otherwise = DiscordMarkdown ("[" <> body.discordMarkdownText <> "](" <> escapeDiscordLinkTarget target <> ")")
  image target _title description
    | Text.null description.discordMarkdownText = discordMarkdownTextOnly target
    | otherwise = description <> discordMarkdownTextOnly (" " <> target)
  code text = discordMarkdownTextOnly ("`" <> escapeDiscordInlineCode text <> "`")
  rawInline _ text = discordMarkdownTextOnly text

instance IsBlock DiscordMarkdown DiscordMarkdown where
  paragraph body = body <> discordMarkdownTextOnly "\n\n"
  plain body = body <> discordMarkdownTextOnly "\n"
  thematicBreak = discordMarkdownTextOnly "--------\n\n"
  blockQuote body =
    discordMarkdownTextOnly (quoteDiscordBlock (Text.stripEnd body.discordMarkdownText) <> "\n\n")
  codeBlock info text =
    discordMarkdownTextOnly ("```" <> Text.takeWhile (not . Char.isSpace) info <> "\n" <> escapeDiscordCodeBlock text <> "\n```\n\n")
  heading level body =
    discordMarkdownTextOnly (Text.replicate (max 1 (min 3 level)) "#" <> " ")
      <> body
      <> discordMarkdownTextOnly "\n\n"
  rawBlock _ text = discordMarkdownTextOnly text
  referenceLinkDefinition _ _ = mempty
  list listType _ items =
    mconcat (zipWith renderItem [(1 :: Int)..] items) <> discordMarkdownTextOnly "\n"
    where
      renderItem index item =
        discordMarkdownTextOnly (discordListItemPrefix listType index)
          <> indentDiscordContinuation "  " (trimDiscordMarkdownEnd item)
          <> discordMarkdownTextOnly "\n"

instance HasEmoji DiscordMarkdown where
  emoji _keyword value =
    discordMarkdownTextOnly value

instance HasStrikethrough DiscordMarkdown where
  strikethrough =
    discordMarkdownWrap "~~" "~~"

instance HasMath DiscordMarkdown where
  inlineMath text =
    discordMarkdownTextOnly ("`" <> escapeDiscordInlineCode text <> "`")
  displayMath text =
    discordMarkdownTextOnly ("```\n" <> escapeDiscordCodeBlock text <> "\n```")

instance HasTaskList DiscordMarkdown DiscordMarkdown where
  taskList _ _ items =
    mconcat (map renderItem items) <> discordMarkdownTextOnly "\n"
    where
      renderItem (checked, item) =
        discordMarkdownTextOnly (if checked then "- [x] " else "- [ ] ")
          <> indentDiscordContinuation "  " (trimDiscordMarkdownEnd item)
          <> discordMarkdownTextOnly "\n"

instance HasFootnote DiscordMarkdown DiscordMarkdown where
  footnote number _label body =
    discordMarkdownTextOnly ("[" <> show number <> "]: ")
      <> indentDiscordContinuation "    " (trimDiscordMarkdownEnd body)
      <> discordMarkdownTextOnly "\n"
  footnoteList =
    mconcat
  footnoteRef number _label _body =
    discordMarkdownTextOnly ("[" <> number <> "]")

instance HasPipeTable DiscordMarkdown DiscordMarkdown where
  pipeTable _alignments headerCells rows =
    discordMarkdownTextOnly "```\n"
      <> discordMarkdownTextOnly (Text.unlines (map pipeRow (headerCells : rows)))
      <> discordMarkdownTextOnly "```\n\n"
    where
      pipeRow cells =
        Text.intercalate " | " (map (Text.strip . (.discordMarkdownText)) cells)

instance HasAlerts DiscordMarkdown DiscordMarkdown where
  alert alertType body =
    discordMarkdownTextOnly ("> **" <> alertName alertType <> "**\n")
      <> DiscordMarkdown (quoteDiscordBlock (Text.stripEnd body.discordMarkdownText))
      <> discordMarkdownTextOnly "\n\n"

formatDiscordMarkdown :: Text -> Text
formatDiscordMarkdown input =
  case runIdentity (commonmarkWith discordMarkdownSyntax "discord-message" (input <> "\n")) of
    Left _ ->
      input
    Right formatted ->
      Text.stripEnd formatted.discordMarkdownText

discordMarkdownSyntax :: SyntaxSpec Identity DiscordMarkdown DiscordMarkdown
discordMarkdownSyntax =
  gfmExtensions
    <> mathSpec
    <> defaultSyntaxSpec

discordMarkdownTextOnly :: Text -> DiscordMarkdown
discordMarkdownTextOnly =
  DiscordMarkdown

discordMarkdownWrap :: Text -> Text -> DiscordMarkdown -> DiscordMarkdown
discordMarkdownWrap left right body
  | Text.null body.discordMarkdownText = body
  | otherwise = DiscordMarkdown (left <> body.discordMarkdownText <> right)

trimDiscordMarkdownEnd :: DiscordMarkdown -> DiscordMarkdown
trimDiscordMarkdownEnd =
  DiscordMarkdown . Text.dropWhileEnd Char.isSpace . (.discordMarkdownText)

indentDiscordContinuation :: Text -> DiscordMarkdown -> DiscordMarkdown
indentDiscordContinuation indent formatted =
  DiscordMarkdown (Text.intercalate "\n" (indentLines (Text.lines formatted.discordMarkdownText)))
  where
    indentLines [] = []
    indentLines (firstLine : rest) =
      firstLine : map (indent <>) rest

discordListItemPrefix :: ListType -> Int -> Text
discordListItemPrefix (BulletList _) _ =
  "- "
discordListItemPrefix (OrderedList start _ delimiter) index =
  discordOrderedItemPrefix delimiter (start + index - 1)

discordOrderedItemPrefix :: DelimiterType -> Int -> Text
discordOrderedItemPrefix Period number =
  show number <> ". "
discordOrderedItemPrefix OneParen number =
  show number <> ") "
discordOrderedItemPrefix TwoParens number =
  "(" <> show number <> ") "

quoteDiscordBlock :: Text -> Text
quoteDiscordBlock text =
  Text.unlines (map ("> " <>) (Text.lines text))

escapeDiscordMarkdown :: Text -> Text
escapeDiscordMarkdown =
  Text.concatMap \case
    '\\' -> "\\\\"
    '`' -> "\\`"
    '*' -> "\\*"
    '_' -> "\\_"
    '~' -> "\\~"
    '|' -> "\\|"
    '[' -> "\\["
    ']' -> "\\]"
    '(' -> "\\("
    ')' -> "\\)"
    c -> Text.singleton c

escapeDiscordInlineCode :: Text -> Text
escapeDiscordInlineCode =
  Text.replace "`" "\\`"

escapeDiscordCodeBlock :: Text -> Text
escapeDiscordCodeBlock =
  Text.replace "```" "`\8203``"

escapeDiscordLinkTarget :: Text -> Text
escapeDiscordLinkTarget =
  Text.replace ")" "%29"

discordUserAvatarValue :: User -> Maybe Aeson.Value
discordUserAvatarValue user = do
  avatarUrl <- discordUserAvatarUrl user
  pure $ Aeson.object
    [ "platform" Aeson..= ("discord" :: Text)
    , "user_id" Aeson..= user.id
    , "username" Aeson..= user.username
    , "global_name" Aeson..= user.globalName
    , "avatar" Aeson..= user.avatar
    , "avatar_url" Aeson..= avatarUrl
    ]

discordUserAvatarUrl :: User -> Maybe Text
discordUserAvatarUrl user =
  case user.avatar of
    Just avatarHash | not (Text.null avatarHash) ->
      let userId = user.id
          extension = discordAvatarExtension avatarHash
      in Just [i|https://cdn.discordapp.com/avatars/#{userId}/#{avatarHash}.#{extension}?size=512|]
    _ ->
      discordDefaultAvatarUrl user.id

discordAvatarExtension :: Text -> Text
discordAvatarExtension avatarHash
  | "a_" `Text.isPrefixOf` avatarHash = "gif"
  | otherwise = "png"

discordDefaultAvatarUrl :: Text -> Maybe Text
discordDefaultAvatarUrl userId = do
  numericUserId <- parseDiscordSnowflake userId
  let index = (numericUserId `shiftR` 22) `mod` 6
  pure [i|https://cdn.discordapp.com/embed/avatars/#{index}.png|]

parseDiscordSnowflake :: Text -> Maybe Integer
parseDiscordSnowflake raw =
  case reads (Text.unpack (Text.strip raw)) of
    [(value, "")] ->
      Just value
    _ ->
      Nothing

discordRemoteImageRef :: Text -> Maybe Text
discordRemoteImageRef ref
  | "http://" `Text.isPrefixOf` ref || "https://" `Text.isPrefixOf` ref = Just ref
  | otherwise = Nothing

discordImageRef :: Media.Media :> es => Text -> Eff es Text
discordImageRef ref
  | "media:" `Text.isPrefixOf` Text.strip ref = do
      publicRef <- Media.publicMediaRef ref
      if "media:" `Text.isPrefixOf` Text.strip publicRef
        then maybe ref (("file://" <>) . Text.pack) <$> Media.localMediaPath ref
        else pure publicRef
  | otherwise =
      pure ref

withDiscordImageFile :: (FileSystem :> es, IOE :> es) => Text -> (FilePath -> Eff es a) -> Eff es a
withDiscordImageFile ref action =
  case Text.stripPrefix "file://" ref of
    Just path -> action (Text.unpack path)
    Nothing -> action (Text.unpack ref)

messageImageUrls :: Message -> [Text]
messageImageUrls message =
  [ attachment.url
  | attachment <- message.attachments
  , attachmentIsImage attachment
  ] <>
  [ embedImage.imageUrl
  | embed <- message.embeds
  , embedImage <- maybeToList embed.image <> maybeToList embed.thumbnail
  ] <>
  contentImageUrls message.content

messageFiles :: Message -> [MessageFile]
messageFiles message =
  [ MessageFile{name = attachment.filename, ref = attachment.url}
  | attachment <- message.attachments
  , not (attachmentIsImage attachment)
  ]

attachmentIsImage :: Attachment -> Bool
attachmentIsImage attachment =
  maybe (imageFileName attachment.filename || imageUrl attachment.url) ("image/" `Text.isPrefixOf`) attachment.contentType

contentImageUrls :: Text -> [Text]
contentImageUrls =
  filter imageUrl . Text.words

imageUrl :: Text -> Bool
imageUrl raw =
  ("http://" `Text.isPrefixOf` stripped || "https://" `Text.isPrefixOf` stripped) && imagePath (Text.toLower withoutQuery)
  where
    stripped =
      Text.dropWhileEnd (`elem` (".,;:!?)" :: String)) raw
    withoutQuery =
      Text.takeWhile (\c -> c /= '?' && c /= '#') stripped

imageFileName :: Text -> Bool
imageFileName =
  imagePath . Text.toLower

imagePath :: Text -> Bool
imagePath path =
  any (`Text.isSuffixOf` path)
    [ ".jpg"
    , ".jpeg"
    , ".png"
    , ".gif"
    , ".webp"
    , ".bmp"
    , ".avif"
    ]

isOwnMessage :: Config -> Message -> Bool
isOwnMessage cfg message =
  Just message.author.id == cfg.botId

discordMessageGuildId :: Aeson.Value -> Maybe Text
discordMessageGuildId =
  join . Aeson.parseMaybe (Aeson.withObject "Discord message" \o -> o Aeson..:? "guild_id")

discordSnowflakeNumber :: Text -> Integer
discordSnowflakeNumber raw =
  fromMaybe (stableTextId raw) (parseIntegerUserId raw)

parseIntegerUserId :: Text -> Maybe Integer
parseIntegerUserId raw =
  readMaybe (Text.unpack (Text.strip raw))

stableTextId :: Text -> Integer
stableTextId =
  Text.foldl' step 14695981039346656037
  where
    step acc char =
      fromIntegral ((fromIntegral acc `xor` fromIntegral (fromEnum char)) * fnvPrime :: Word64)
    fnvPrime :: Word64
    fnvPrime = 1099511628211

discordJsonRequest
  :: (HTTP.HTTP :> es, KatipE :> es, Aeson.ToJSON body, Aeson.FromJSON result, HttpMethod method, HttpBodyAllowed (AllowsBody method) 'CanHaveBody)
  => Config
  -> method
  -> [Text]
  -> body
  -> Eff es result
discordJsonRequest cfg method path body = do
  logDebug [i|Discord REST request: #{Text.intercalate "/" path}|]
  HTTP.runReq do
    req method (discordApiUrl path) (ReqBodyJson body) jsonResponse (discordRequestOptions cfg)
      <&> responseBody

discordNoResponseRequest
  :: (HTTP.HTTP :> es, KatipE :> es, HttpMethod method, HttpBodyAllowed (AllowsBody method) 'NoBody)
  => Config
  -> method
  -> [Text]
  -> Eff es ()
discordNoResponseRequest cfg method path = do
  logDebug [i|Discord REST request: #{Text.intercalate "/" path}|]
  void $ HTTP.runReq do
    req method (discordApiUrl path) NoReqBody ignoreResponse (discordRequestOptions cfg)

discordGetRequest
  :: (HTTP.HTTP :> es, KatipE :> es, Aeson.FromJSON result)
  => Config
  -> [Text]
  -> Eff es result
discordGetRequest cfg path = do
  logDebug [i|Discord REST request: #{Text.intercalate "/" path}|]
  HTTP.runReq do
    req GET (discordApiUrl path) NoReqBody jsonResponse (discordRequestOptions cfg)
      <&> responseBody

discordUploadFile
  :: (HTTP.HTTP :> es, IOE :> es)
  => Config
  -> Text
  -> Maybe Text
  -> FilePath
  -> Maybe Text
  -> Eff es Message
discordUploadFile cfg channelId content path fileName = do
  manager <- HTTP.manager
  base <- liftIO $ Client.parseRequest [i|https://discord.com/api/v10/channels/#{channelId}/messages|]
  let payload = Aeson.object
        [ "content" Aeson..= fromMaybe "" content
        ]
      request =
        base
          { Client.method = "POST"
          , Client.requestHeaders =
              [ ("Authorization", TextEncoding.encodeUtf8 ("Bot " <> cfg.botToken))
              , ("User-Agent", "cosmobot")
              ]
          }
      parts =
        [ Multipart.partLBS "payload_json" (Aeson.encode payload)
        , Multipart.partFileRequestBodyM "files[0]" (Text.unpack (Driver.uploadFileName path fileName)) (Client.streamFile path)
        ]
  multipartRequest <- liftIO $ Multipart.formDataBody parts request
  response <- liftIO $ Client.httpLbs multipartRequest manager
  case Aeson.eitherDecode (Client.responseBody response) of
    Right message ->
      pure message
    Left err ->
      throwIO (userError [i|Discord upload response decode failed: #{err}|])

discordApiUrl :: [Text] -> Url 'Https
discordApiUrl path =
  foldl' (/:) (https "discord.com" /: "api" /: "v10") path

discordRequestOptions :: Config -> Option 'Https
discordRequestOptions cfg =
  header "Authorization" (TextEncoding.encodeUtf8 ("Bot " <> cfg.botToken))
    <> header "User-Agent" "cosmobot"

data GatewayEnvelope = GatewayEnvelope
  { op :: !Int
  , d :: !Aeson.Value
  , s :: !(Maybe Int)
  , t :: !(Maybe Text)
  }
  deriving (Show, Generic)
    deriving Aeson.FromJSON via (SnakeJSON GatewayEnvelope)

data GatewayHello = GatewayHello
  { heartbeatInterval :: !Int
  }
  deriving (Show, Generic)
    deriving Aeson.FromJSON via (SnakeJSON GatewayHello)

parseGatewayData :: (IOE :> es, Aeson.FromJSON a) => String -> Aeson.Value -> Eff es a
parseGatewayData label value =
  case Aeson.parseEither Aeson.parseJSON value of
    Right parsed ->
      pure parsed
    Left err ->
      throwIO (userError [i|#{label} parse failed: #{err}|])

data Message = Message
  { id :: !Text
  , channelId :: !Text
  , guildId :: !(Maybe Text)
  , author :: !User
  , member :: !(Maybe Member)
  , content :: !Text
  , attachments :: ![Attachment]
  , embeds :: ![Embed]
  , mentions :: ![User]
  , referencedMessage :: !(Maybe Message)
  , messageReference :: !(Maybe Reference)
  , raw :: !Aeson.Value
  }
  deriving (Show, Generic)

instance Aeson.FromJSON Message where
  parseJSON value = Aeson.withObject "Message" (\o ->
    Message
      <$> o Aeson..: "id"
      <*> o Aeson..: "channel_id"
      <*> o Aeson..:? "guild_id"
      <*> o Aeson..: "author"
      <*> o Aeson..:? "member"
      <*> fmap (fromMaybe "") (o Aeson..:? "content")
      <*> fmap (fromMaybe []) (o Aeson..:? "attachments")
      <*> fmap (fromMaybe []) (o Aeson..:? "embeds")
      <*> fmap (fromMaybe []) (o Aeson..:? "mentions")
      <*> o Aeson..:? "referenced_message"
      <*> o Aeson..:? "message_reference"
      <*> pure value
    ) value

data DeletedMessage = DeletedMessage
  { id :: !Text
  , channelId :: !Text
  , guildId :: !(Maybe Text)
  , raw :: !Aeson.Value
  }
  deriving (Show, Generic)

instance Aeson.FromJSON DeletedMessage where
  parseJSON value = Aeson.withObject "DeletedMessage" (\o ->
    DeletedMessage
      <$> o Aeson..: "id"
      <*> o Aeson..: "channel_id"
      <*> o Aeson..:? "guild_id"
      <*> pure value
    ) value

data User = User
  { id :: !Text
  , username :: !(Maybe Text)
  , globalName :: !(Maybe Text)
  , bot :: !Bool
  , avatar :: !(Maybe Text)
  }
  deriving (Show, Generic, Aeson.ToJSON)

instance Aeson.FromJSON User where
  parseJSON = Aeson.withObject "User" \o ->
    User
      <$> o Aeson..: "id"
      <*> o Aeson..:? "username"
      <*> o Aeson..:? "global_name"
      <*> fmap (fromMaybe False) (o Aeson..:? "bot")
      <*> o Aeson..:? "avatar"

data Member = Member
  { nick :: !(Maybe Text)
  , user :: !(Maybe User)
  }
  deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data Attachment = Attachment
  { id :: !Text
  , filename :: !Text
  , url :: !Text
  , contentType :: !(Maybe Text)
  }
  deriving (Show, Generic)
    deriving Aeson.FromJSON via (SnakeJSON Attachment)

data Embed = Embed
  { image :: !(Maybe EmbedImage)
  , thumbnail :: !(Maybe EmbedImage)
  }
  deriving (Show, Generic, Aeson.FromJSON)

data EmbedImage = EmbedImage
  { imageUrl :: !Text
  }
  deriving (Show, Generic)
    deriving Aeson.FromJSON via (PrefixedSnakeJSON "image" EmbedImage)

data Reference = Reference
  { messageId :: !MessageId
  , channelId :: !Text
  , guildId :: !(Maybe Text)
  }
  deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (SnakeJSONOmitNothing Reference)

data AllowedMentions = AllowedMentions
  { parse :: ![Text]
  , users :: ![Text]
  , repliedUser :: !(Maybe Bool)
  }
  deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSONOmitNothing AllowedMentions)

data CreateMessageRequest = CreateMessageRequest
  { content :: !Text
  , messageReference :: !(Maybe Reference)
  , allowedMentions :: !AllowedMentions
  }
  deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSONOmitNothing CreateMessageRequest)
