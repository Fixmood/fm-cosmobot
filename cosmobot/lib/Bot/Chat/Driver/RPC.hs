{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.Chat.Driver.RPC
Description : RPC chat driver implementation
Stability   : experimental
-}

module Bot.Chat.Driver.RPC
  ( RpcChatDriver
  , rpcChatDriver
  )
where

import qualified Bot.Chat.Types as Chat
import Bot.Chat.Driver.Types
import Bot.Core.Message
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Bot.Effect.Media as Media
import Bot.Effect.Media (MediaObject (..))
import qualified Bot.Effect.Storage as StorageEffect
import Bot.Prelude
import qualified Bot.RPC.Config as Config
import qualified Bot.JSONRPC as JSONRPC
import qualified Bot.RPC.State as RPC
import qualified Bot.Storage.Session as SessionStorage
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified Streaming.ByteString as Q
import System.FilePath (takeExtension, takeFileName)

data RpcChatDriver = RpcChatDriver
  { cfg :: !Config.Config
  , rpcState :: !RPC.RpcState
  }

rpcChatDriver :: Config.Config -> RPC.RpcState -> RpcChatDriver
rpcChatDriver cfg rpcState =
  RpcChatDriver{cfg, rpcState}

instance ChatDriver RpcChatDriver where
  type ChatDriverEffects RpcChatDriver es = (Concurrent :> es, IOE :> es, StorageEffect.Storage :> es, FileSystem.FileSystem :> es, Media.Media :> es)

  driverPlatform _ =
    PlatformRPC

  sendReplyMessage driver message body = do
    let sessionId = RPC.sessionIdFromMessage message
        parentMessageId = message.messageId
    reply <- rpcReplyContent driver.cfg body
    stored <- SessionStorage.appendMessage
      (RPC.unRpcSessionId sessionId)
      "assistant"
      reply.text
      reply.imageUrls
      reply.attachments
      parentMessageId
      parentMessageId
    case stored of
      Left err ->
        pure (Left err)
      Right Nothing ->
        pure (Left "RPC reply did not produce a message id.")
      Right (Just storedReply) -> do
        RPC.rememberMessageNumber driver.rpcState storedReply.messageId
        RPC.broadcast driver.rpcState (Aeson.toJSON (JSONRPC.notification "chat.message" (RPC.storedMessageToRpc storedReply)))
        pure (Right storedReply.messageId)

  replyAudio driver message audioRef caption = do
    let body = maybe audioRef (\c -> c <> "\n" <> audioRef) caption
    sendReplyMessage driver message body

  uploadFile driver message path fileName =
    sendReplyMessage driver message ("Uploaded file: " <> uploadFileName path fileName)

  editMessage driver message messageId body = do
    let sessionId = RPC.sessionIdFromMessage message
        text = ReplyBody.renderReplyBody body
        payload = RPC.RpcOutbound sessionId (Just messageId) text
    updated <- SessionStorage.updateMessageText (RPC.unRpcSessionId sessionId) messageId text
    RPC.broadcast driver.rpcState (Aeson.toJSON (JSONRPC.notification "chat.message_update" payload))
    pure updated

  messageOutPolicy _ _ =
    pure (Chat.EditableMessage 1200 4000)

data RpcReplyContent = RpcReplyContent
  { text :: !Text
  , imageUrls :: ![Text]
  , attachments :: ![SessionStorage.StoredMediaRef]
  }

rpcReplyContent
  :: (StorageEffect.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => Config.Config
  -> Text
  -> Eff es RpcReplyContent
rpcReplyContent cfg body = do
  converted <- traverse (rpcReplyImage cfg) (ReplyBody.replyImageUrls body)
  pure RpcReplyContent
    { text = ReplyBody.renderReplyBody body
    , imageUrls = [url | Left url <- converted]
    , attachments = [attachment | Right attachment <- converted]
    }

rpcReplyImage
  :: (StorageEffect.Storage :> es, FileSystem.FileSystem :> es, IOE :> es, Media.Media :> es)
  => Config.Config
  -> Text
  -> Eff es (Either Text SessionStorage.StoredMediaRef)
rpcReplyImage _cfg ref =
  case Text.stripPrefix "file://" (Text.strip ref) of
    Nothing ->
      pure (Left ref)
    Just pathText -> do
      let path = Text.unpack pathText
      exists <- FileSystem.doesFileExist path
      if exists
        then do
          bytes <- FileSystemByteString.readFile path
          mediaRef <- Media.storeMediaObject $
            MediaObject
              { bytes = Q.fromStrict bytes
              , mimeType = imageMediaType path
              , sourceName = Just (Text.pack (takeFileName path))
              }
          case mediaRef >>= RPC.parseMediaId of
            Nothing ->
              pure (Left ref)
            Just fileId ->
              Media.mediaFileInfo fileId >>= \case
                Nothing -> pure (Left ref)
                Just info -> do
                  url <- Media.publicMediaRef info.ref
                  pure (Right (RPC.storedMediaRef info url))
        else
          pure (Left ref)

imageMediaType :: FilePath -> Text
imageMediaType path =
  case Text.toLower (Text.pack (takeExtension path)) of
    ".avif" -> "image/avif"
    ".gif" -> "image/gif"
    ".jpeg" -> "image/jpeg"
    ".jpg" -> "image/jpeg"
    ".png" -> "image/png"
    ".webp" -> "image/webp"
    _ -> "application/octet-stream"
