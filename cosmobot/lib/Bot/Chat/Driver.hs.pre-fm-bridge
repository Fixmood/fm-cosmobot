{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Bot.Chat.Driver
Description : Platform-specific chat driver dispatch
Stability   : experimental
-}

module Bot.Chat.Driver
  ( runChatDrivers
  )
where

import qualified Bot.Chat.Driver.QQ as QQ
import qualified Bot.Chat.Driver.ACP as ACPDriver
import qualified Bot.Chat.Driver.Discord as Discord
import qualified Bot.Chat.Driver.Matrix as Matrix
import qualified Bot.Chat.Driver.RPC as RPCDriver
import qualified Bot.Chat.Driver.Telegram as Telegram
import qualified Bot.ACP.State as ACP
import Bot.Chat.Driver.Types
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatDriver as ChatDriverEffect
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Matrix as MatrixEffect
import qualified Bot.Effect.Storage as Storage
import Bot.Core.Message
import qualified Bot.Core.ReplyBody as ReplyBody
import Bot.Prelude
import qualified Bot.RPC.Config as RPCConfig
import qualified Bot.RPC.State as RPC
import qualified Bot.Util.Stream as StreamUtil
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Vector as Vector
import Effectful.FileSystem (FileSystem)
import Effectful.Timeout
import qualified Streaming as S
import System.IO.Error (userError)

data ChatDrivers = ChatDrivers
  { qq :: !(Maybe QQ.QQDriver)
  , telegram :: !(Maybe Telegram.TelegramDriver)
  , matrix :: !(Maybe Matrix.MatrixDriver)
  , discord :: !(Maybe Discord.DiscordDriver)
  , rpc :: !(Maybe RPCDriver.RpcChatDriver)
  , acp :: !(Maybe ACPDriver.AcpChatDriver)
  , rpcState :: !RPC.RpcState
  , acpState :: !ACP.AcpState
  }

newtype NormalizingChatDriver driver =
  NormalizingChatDriver driver

instance ChatDriver driver => ChatDriver (NormalizingChatDriver driver) where
  type ChatDriverEffects (NormalizingChatDriver driver) es =
    (ChatDriverEffects driver es, KatipE :> es, Media.Media :> es)

  driverPlatform (NormalizingChatDriver driver) =
    driverPlatform driver

  sendReplyMessage (NormalizingChatDriver driver) message body =
    withChatDriverEither message "Chat reply" do
      normalizedBody <- normalizeOutgoingReplyBody driver body
      sendReplyMessage driver message normalizedBody

  sendReplyMessages (NormalizingChatDriver driver) message body =
    withChatDriverResults message "Chat reply" do
      normalizedBody <- normalizeOutgoingReplyBody driver body
      sendReplyMessages driver message normalizedBody

  sendStreamingReplyMessage (NormalizingChatDriver driver) message body =
    withChatDriverEither message "Chat streaming reply" do
      normalizedBody <- normalizeOutgoingReplyBody driver body
      sendStreamingReplyMessage driver message normalizedBody

  replyAudio (NormalizingChatDriver driver) message audioRef caption =
    withChatDriverEither message "Audio send" do
      replyAudio driver message audioRef caption

  uploadFile (NormalizingChatDriver driver) message path fileName =
    withChatDriverEither message "File upload" do
      uploadFile driver message path fileName

  editMessage (NormalizingChatDriver driver) message messageId body =
    fromMaybe False <$> withChatDriverMaybe message "chat edit" do
      Just <$> editMessage driver message messageId body

  completeMessageEdit (NormalizingChatDriver driver) message messageId =
    fromMaybe False <$> withChatDriverMaybe message "chat completion edit" do
      Just <$> completeMessageEdit driver message messageId

  deleteMessage (NormalizingChatDriver driver) message messageId =
    fromMaybe False <$> withChatDriverMaybe message "chat delete" do
      Just <$> deleteMessage driver message messageId

  messageOutPolicy (NormalizingChatDriver driver) message =
    fromMaybe defaultMessageOutPolicy <$> withChatDriverMaybe message "message out policy" do
      Just <$> messageOutPolicy driver message

  getMessageContent (NormalizingChatDriver driver) message messageId =
    withChatDriverMaybe message "fetch referenced message" do
      traverse (normalizeReferencedMessageMedia driver) =<< getMessageContent driver message messageId

  getSenderMemberInfo (NormalizingChatDriver driver) message =
    withChatDriverMaybe message "fetch sender member info" do
      traverse (normalizeJsonMediaUrls (normalizeMediaRef driver)) =<< getSenderMemberInfo driver message

  getMemberInfo (NormalizingChatDriver driver) message userId =
    withChatDriverMaybe message "fetch member info" do
      traverse (normalizeJsonMediaUrls (normalizeMediaRef driver)) =<< getMemberInfo driver message userId

  getUserAvatar (NormalizingChatDriver driver) message userId =
    withChatDriverMaybe message "fetch user avatar" do
      traverse (normalizeJsonMediaUrls (normalizeMediaRef driver)) =<< getUserAvatar driver message userId

  listGroupMembers (NormalizingChatDriver driver) message =
    withChatDriverMaybe message "list group members" do
      traverse (normalizeJsonMediaUrls (normalizeMediaRef driver)) =<< listGroupMembers driver message

  normalizeMediaRef (NormalizingChatDriver driver) =
    normalizeMediaRef driver

  mentionUser (NormalizingChatDriver driver) message userId body =
    withChatDriverEither message "Chat mention" do
      normalizedBody <- normalizeOutgoingReplyBody driver body
      mentionUser driver message userId normalizedBody

  setMemberTitle (NormalizingChatDriver driver) message userId title =
    fromMaybe False <$> withChatDriverMaybe message "set member title" do
      Just <$> setMemberTitle driver message userId title

  setTyping (NormalizingChatDriver driver) message timeoutMillis =
    setTyping driver message timeoutMillis
      `catchSync` \err ->
        logError [i|Failed to set typing: #{show err :: String}|]

instance ChatDriver ChatDrivers where
  type ChatDriverEffects ChatDrivers es =
    ( ChatDriverEffects (NormalizingChatDriver QQ.QQDriver) es
    , ChatDriverEffects (NormalizingChatDriver Telegram.TelegramDriver) es
    , ChatDriverEffects (NormalizingChatDriver Matrix.MatrixDriver) es
    , ChatDriverEffects (NormalizingChatDriver Discord.DiscordDriver) es
    , ChatDriverEffects (NormalizingChatDriver RPCDriver.RpcChatDriver) es
    , ChatDriverEffects (NormalizingChatDriver ACPDriver.AcpChatDriver) es
    )

  driverPlatform _ =
    error "ChatDrivers does not have a single platform"

  sendReplyMessage drivers message body =
    withMessageDriver drivers message \driver ->
      sendReplyMessage driver message body

  sendReplyMessages drivers message body =
    withMessageDriver drivers message \driver ->
      sendReplyMessages driver message body

  sendStreamingReplyMessage drivers message body =
    withMessageDriver drivers message \driver ->
      sendStreamingReplyMessage driver message body

  replyAudio drivers message audioRef caption =
    withMessageDriver drivers message \driver ->
      replyAudio driver message audioRef caption

  uploadFile drivers message path fileName =
    withMessageDriver drivers message \driver ->
      uploadFile driver message path fileName

  editMessage drivers message messageId body =
    withMessageDriver drivers message \driver ->
      editMessage driver message messageId body

  completeMessageEdit drivers message messageId =
    withMessageDriver drivers message \driver ->
      completeMessageEdit driver message messageId

  deleteMessage drivers message messageId =
    withMessageDriver drivers message \driver ->
      deleteMessage driver message messageId

  messageOutPolicy drivers message =
    withMessageDriver drivers message \driver ->
      messageOutPolicy driver message

  getMessageContent drivers message messageId =
    withMessageDriver drivers message \driver ->
      getMessageContent driver message messageId

  getSenderMemberInfo drivers message =
    withMessageDriver drivers message \driver ->
      getSenderMemberInfo driver message

  getMemberInfo drivers message userId =
    withMessageDriver drivers message \driver ->
      getMemberInfo driver message userId

  getUserAvatar drivers message userId =
    withMessageDriver drivers message \driver ->
      getUserAvatar driver message userId

  listGroupMembers drivers message =
    withMessageDriver drivers message \driver ->
      listGroupMembers driver message

  normalizeMediaRefForMessage drivers message ref =
    withMessageDriver drivers message \driver ->
      normalizeMediaRef driver ref

  mentionUser drivers message userId body =
    withMessageDriver drivers message \driver ->
      mentionUser driver message userId body

  setMemberTitle drivers message userId title =
    withMessageDriver drivers message \driver ->
      setMemberTitle driver message userId title

  setTyping drivers message timeoutMillis =
    withMessageDriver drivers message \driver ->
      setTyping driver message timeoutMillis

withMessageDriver
  :: (ChatDriverEffects ChatDrivers es, IOE :> es)
  => ChatDrivers
  -> IncomingMessage
  -> (forall driver. (ChatDriver driver, ChatDriverEffects driver es) => driver -> Eff es a)
  -> Eff es a
withMessageDriver drivers message action =
  case message.platform of
    PlatformQQ ->
      maybe missingDriver (action . NormalizingChatDriver) drivers.qq
    PlatformTelegram ->
      maybe missingDriver (action . NormalizingChatDriver) drivers.telegram
    PlatformMatrix ->
      maybe missingDriver (action . NormalizingChatDriver) drivers.matrix
    PlatformDiscord ->
      maybe missingDriver (action . NormalizingChatDriver) drivers.discord
    PlatformRPC ->
      maybe missingDriver (action . NormalizingChatDriver) drivers.rpc
    PlatformACP ->
      maybe missingDriver (action . NormalizingChatDriver) drivers.acp
  where
    platformText = show message.platform :: String
    missingDriver =
      throwIO (userError [i|#{platformText} driver is not configured.|])

withChatDriverMaybe
  :: (KatipE :> es)
  => IncomingMessage
  -> Text
  -> Eff es (Maybe a)
  -> Eff es (Maybe a)
withChatDriverMaybe message label action =
  action `catchSync` \err -> do
    let platformText = show message.platform :: String
    logInfo [i|#{label} failed on #{platformText}: #{displayException err}|]
    pure Nothing

withChatDriverEither
  :: (KatipE :> es)
  => IncomingMessage
  -> Text
  -> Eff es (Either Text MessageId)
  -> Eff es (Either Text MessageId)
withChatDriverEither message label action =
  action `catchSync` \err -> do
    let platformText = show message.platform :: String
        messageText = [i|#{label} failed on #{platformText}: #{displayException err}|]
    logInfo messageText
    pure (Left messageText)

withChatDriverResults
  :: KatipE :> es
  => IncomingMessage
  -> Text
  -> Eff es [Either Text MessageId]
  -> Eff es [Either Text MessageId]
withChatDriverResults message label action =
  action `catchSync` \err -> do
    let platformText = show message.platform :: String
        messageText = [i|#{label} failed on #{platformText}: #{displayException err}|]
    logInfo messageText
    pure [Left messageText]

defaultMessageOutPolicy :: Chat.MessageOutPolicy
defaultMessageOutPolicy =
  Chat.ChunkedMessage defaultChunkedMessageLimit

defaultChunkedMessageLimit :: Int
defaultChunkedMessageLimit = 4000

normalizeReferencedMessageMedia
  :: (Media.Media :> es, ChatDriver driver, ChatDriverEffects driver es)
  => driver
  -> ReferencedMessage
  -> Eff es ReferencedMessage
normalizeReferencedMessageMedia driver message = do
  imageUrls <- traverse (normalizeMediaRef driver >=> Media.normalizeMediaRef) message.imageUrls
  files <- traverse normalizeFile message.files
  pure ReferencedMessage
    { messageId = message.messageId
    , senderDisplayName = message.senderDisplayName
    , senderIdentifier = message.senderIdentifier
    , senderIsBot = message.senderIsBot
    , text = message.text
    , imageUrls
    , files
    }
  where
    normalizeFile file = do
      ref <- normalizeMediaRef driver file.ref >>= Media.normalizeMediaRef
      pure MessageFile{name = file.name, ref}

normalizeOutgoingReplyBody
  :: (Media.Media :> es, ChatDriver driver, ChatDriverEffects driver es)
  => driver
  -> Text
  -> Eff es Text
normalizeOutgoingReplyBody driver body =
  Media.normalizeReplyBody body >>= ReplyBody.traverseReplyImageUrls (normalizeMediaRef driver)

runChatDrivers
  :: (KatipE :> es, Concurrency.Concurrency :> es, HTTP.HTTP :> es, Timeout :> es, Fail :> es, Concurrent :> es, Media.Media :> es, FileSystem :> es, Prim :> es, Storage.Storage :> es, IOE :> es)
  => Maybe QQ.Config
  -> Maybe Telegram.Config
  -> Maybe Matrix.Config
  -> Maybe Discord.Config
  -> RPCConfig.Config
  -> RPC.RpcState
  -> Bool
  -> ACP.AcpState
  -> Eff (MatrixEffect.Matrix : Chat.Chat : es) ()
  -> Eff es ()
runChatDrivers qqConfig telegramConfig matrixConfig discordConfig rpcConfig rpcState acpEnabled acpState action = do
  qq <- traverse QQ.newQQDriver qqConfig
  let telegram = Telegram.newTelegramDriver <$> telegramConfig
  matrix <- traverse Matrix.newMatrixDriver matrixConfig
  discord <- traverse Discord.newDiscordDriver discordConfig
  let drivers = ChatDrivers
        { qq
        , telegram
        , matrix
        , discord
        , rpc = RPCDriver.rpcChatDriver rpcConfig rpcState <$ guard rpcConfig.enabled
        , acp = ACPDriver.acpChatDriver acpState <$ guard acpEnabled
        , rpcState
        , acpState
        }
  if hasConfiguredChatDriver drivers
    then runChatDriversWith drivers action
    else logInfo "No chat drivers or RPC server are configured; exiting."

hasConfiguredChatDriver :: ChatDrivers -> Bool
hasConfiguredChatDriver drivers =
  any isJust
    [ void drivers.qq
    , void drivers.telegram
    , void drivers.matrix
    , void drivers.discord
    , void drivers.rpc
    , void drivers.acp
    ]

runChatDriversWith
  :: (KatipE :> es, Concurrency.Concurrency :> es, HTTP.HTTP :> es, Timeout :> es, Fail :> es, Concurrent :> es, Media.Media :> es, FileSystem :> es, Prim :> es, Storage.Storage :> es, IOE :> es)
  => ChatDrivers
  -> Eff (MatrixEffect.Matrix : Chat.Chat : es) ()
  -> Eff es ()
runChatDriversWith drivers inner = do
  runMaybeDiscordDriver drivers.discord $
    runMaybeQQDriver drivers.qq $
      Chat.runChatWithHandler (chatDriversHandler drivers) $
        Matrix.runMatrixClient drivers.matrix inner

runMaybeQQDriver
  :: (IOE :> es, KatipE :> es, Timeout :> es, Concurrent :> es, Concurrency.Concurrency :> es)
  => Maybe QQ.QQDriver
  -> Eff es a
  -> Eff es a
runMaybeQQDriver =
  maybe id QQ.runQQDriver

runMaybeDiscordDriver
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Concurrency.Concurrency :> es)
  => Maybe Discord.DiscordDriver
  -> Eff es a
  -> Eff es a
runMaybeDiscordDriver =
  maybe id Discord.runDiscordDriver

chatDriversHandler
  :: forall es.
     ( ChatDriverEffects ChatDrivers es
     , HTTP.HTTP :> es
     , Concurrency.Concurrency :> es
     , Media.Media :> es
     , Storage.Storage :> es
     , KatipE :> es
     , Concurrent :> es
     , Prim :> es
     , Fail :> es
     , IOE :> es
     )
  => ChatDrivers
  -> Chat.ChatHandler es
chatDriversHandler drivers localEnv = \case
  ChatDriverEffect.IncomingMessages -> do
    let stream :: Stream (Of IncomingMessage) (Eff es) ()
        stream = incomingMessages drivers
    localLift localEnv incomingMessageStreamUnlift \runLocal ->
      pure (S.hoist runLocal stream)
  op ->
    Chat.chatDriverHandler drivers localEnv op

incomingMessageStreamUnlift :: UnliftStrategy
incomingMessageStreamUnlift =
  ConcUnlift Persistent (Limited 1)

incomingMessages
  :: HTTP.HTTP :> es
  => Timeout :> es
  => Concurrency.Concurrency :> es
  => Media.Media :> es
  => Storage.Storage :> es
  => KatipE :> es
  => Concurrent :> es
  => Prim :> es
  => Fail :> es
  => IOE :> es
  => ChatDrivers
  -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessages drivers =
  Media.normalizeIncomingMessages $
    StreamUtil.mergeStreams
    (catMaybes
      [ QQ.incomingMessages <$> drivers.qq
      , Telegram.incomingMessages <$> drivers.telegram
      , Matrix.incomingMessages <$> drivers.matrix
      , Discord.incomingMessages <$> drivers.discord
      , RPC.incomingMessages drivers.rpcState <$ drivers.rpc
      , ACP.incomingMessages drivers.acpState <$ drivers.acp
      ])

normalizeJsonMediaUrls :: Media.Media :> es => (Text -> Eff es Text) -> Aeson.Value -> Eff es Aeson.Value
normalizeJsonMediaUrls normalizePlatformMediaRef = \case
  Aeson.Object jsonObject ->
    Aeson.Object . AesonKeyMap.fromList <$> traverse normalizePair (AesonKeyMap.toList jsonObject)
  Aeson.Array array ->
    Aeson.Array . Vector.fromList <$> traverse (normalizeJsonMediaUrls normalizePlatformMediaRef) (Vector.toList array)
  value ->
    pure value
  where
    normalizePair (key, Aeson.String ref)
      | AesonKey.toText key `elem` mediaUrlKeys = do
          platformNormalized <- normalizePlatformMediaRef ref
          normalized <- Media.normalizeMediaRef platformNormalized
          pure (key, Aeson.String normalized)
    normalizePair (key, value) =
      (key,) <$> normalizeJsonMediaUrls normalizePlatformMediaRef value

    mediaUrlKeys =
      ["avatar_url", "image_url", "url"]
