{-|
Module      : Bot.Agent.Tools.Sticker
Description : Tools for the bot's own curated sticker gallery
Stability   : experimental

The gallery is a small, hand-picked set of images the owner selected from the
chats the bot already took part in, plus a catalog that says what each one
expresses. It lives next to the runtime config:

@
stickers/catalog.json
stickers/st_01.gif
@

Two constraints shaped this module:

* The bot and the OneBot gateway run in different containers and share no
  volume, so a file path means nothing to the gateway. Images are therefore
  inlined as an RFC 2397 @data:@ URL, which the gateway accepts and which is
  also what 'Bot.Core.ReplyBody.isBase64ImageRef' recognises.
* A sticker has to arrive as its own message, so the tool sends through
  'Chat.replyTo' rather than appending images to the final answer.
-}

module Bot.Agent.Tools.Sticker
  ( stickerListTool
  , stickerSendTool
  , Sticker (..)
  , StickerCatalog (..)
  , parseStickerCatalog
  , matchStickers
  ) where

import Bot.Agent.Tool
import Bot.Agent.Tools.Common (chatTag, jsonText, optionalText, requiredText)
import Bot.Agent.Types
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Media as Media
import qualified Bot.Media.Object as MediaObject
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import System.FilePath ((</>))

-- | One curated sticker.
--
-- The MIME type is deliberately absent: 'MediaObject.fileObject' (and the cache
-- behind it) sniffs the real type, which matters here because several of the
-- picked files carry a .webp name but JPEG content.
data Sticker = Sticker
  { stickerId :: !Text
  , stickerFile :: !Text
  , stickerEmotion :: !Text
  , stickerDescription :: !Text
  , stickerTags :: ![Text]
  }
  deriving (Eq, Show)

newtype StickerCatalog = StickerCatalog
  { stickers :: [Sticker]
  }
  deriving (Eq, Show)

instance Aeson.FromJSON Sticker where
  parseJSON = Aeson.withObject "sticker" \object ->
    Sticker
      <$> object Aeson..: "id"
      <*> object Aeson..: "file"
      <*> object Aeson..: "emotion"
      <*> object Aeson..: "description"
      <*> object Aeson..:? "tags" Aeson..!= []

instance Aeson.FromJSON StickerCatalog where
  parseJSON = Aeson.withObject "sticker catalog" \object ->
    StickerCatalog <$> object Aeson..:? "stickers" Aeson..!= []

parseStickerCatalog :: ByteString -> Maybe StickerCatalog
parseStickerCatalog = Aeson.decodeStrict

-- | Case-insensitive search over emotion, description and tags. An empty
-- keyword returns everything, which is what the model needs when it just wants
-- to know what is available.
matchStickers :: Text -> [Sticker] -> [Sticker]
matchStickers keyword stickers
  | Text.null needle = stickers
  | otherwise = filter matches stickers
  where
    needle = Text.toCaseFold (Text.strip keyword)
    matches sticker =
      any (Text.isInfixOf needle . Text.toCaseFold)
        (sticker.stickerEmotion : sticker.stickerDescription : sticker.stickerTags)

stickerCatalogPath :: FilePath
stickerCatalogPath = "stickers/catalog.json"

stickerDir :: FilePath
stickerDir = "stickers"

loadCatalog :: IOE :> es => FileSystem :> es => Eff es (Maybe StickerCatalog)
loadCatalog = do
  exists <- FileSystem.doesFileExist stickerCatalogPath
  if not exists
    then pure Nothing
    else do
      raw <- FileSystemByteString.readFile stickerCatalogPath
      pure (parseStickerCatalog raw)

-- | A rejected argument, not a transport failure: the model should fix the id.
argumentFailure :: Text -> ToolResult
argumentFailure err =
  toolFailure (permanentArgumentFailure err err)

stickerListTool :: (IOE :> es, FileSystem :> es) => Tool (Eff es)
stickerListTool =
  tagged [chatTag]
  . withDescription "List this bot's own curated stickers. Returns id, emotion, description and tags. Call it before sticker_send when you want to attach a picture to a reaction. Optional keyword filters across emotion, description and tags."
  $ tool "sticker_list" (optionalText "keyword" "Optional keyword to filter stickers, e.g. 开心 or 猫.") \maybeKeyword -> do
      catalog <- loadCatalog
      case catalog of
        Nothing -> pure (toolText "当前没有表情包库（stickers/catalog.json 不存在）。")
        Just StickerCatalog{stickers = available} -> do
          let found = matchStickers (fromMaybe "" maybeKeyword) available
          if null found
            then pure (toolText "没有匹配的表情包，可以换个关键词或者直接不发图。")
            else pure (toolText (jsonText (map renderSticker found)))
  where
    renderSticker sticker =
      Aeson.object
        [ "id" Aeson..= sticker.stickerId
        , "emotion" Aeson..= sticker.stickerEmotion
        , "description" Aeson..= sticker.stickerDescription
        , "tags" Aeson..= sticker.stickerTags
        ]

stickerSendTool :: (Chat.Chat :> es, Media.Media :> es, IOE :> es, FileSystem :> es) => Tool (Eff es)
stickerSendTool =
  tagged [chatTag]
  . withDescription "Send one sticker from this bot's own gallery to the current chat, as its own message. Get an id from sticker_list first. Do not describe the sticker in text as well; either send it or say something."
  $ tool "sticker_send" (requiredText "id" "Sticker id from sticker_list, for example st_06.") \rawId -> do
      context <- askToolContext
      catalog <- loadCatalog
      case catalog of
        Nothing -> pure (toolText "当前没有表情包库，无法发送。")
        Just StickerCatalog{stickers = available} ->
          case filter ((== Text.strip rawId) . (.stickerId)) available of
            [] -> pure (argumentFailure [i|未知的表情 id：#{Text.strip rawId}。先用 sticker_list 查可用的 id。|])
            (sticker : _) -> sendSticker context sticker

sendSticker :: (Chat.Chat :> es, Media.Media :> es, IOE :> es, FileSystem :> es) => Context -> Sticker -> Eff es ToolResult
sendSticker context sticker = do
  let path = stickerDir </> Text.unpack sticker.stickerFile
  exists <- FileSystem.doesFileExist path
  if not exists
    then pure (toolFailure Failure
      { category = ExternalServiceUnavailable
      , userMessage = [i|表情文件不存在：#{sticker.stickerFile}|]
      , detail = Text.pack path
      })
    else do
      mediaObject <- MediaObject.fileObject ("file://" <> Text.pack path)
      stored <- Media.storeMediaObject mediaObject
      case stored of
        Nothing ->
          pure (toolFailure Failure
            { category = ExternalServiceUnavailable
            , userMessage = "表情没能存进媒体库。"
            , detail = Text.pack path
            })
        Just ref -> do
          sent <- Chat.replyTo context.message (ReplyBody.imageDirective ref)
          case rights sent of
            (_ : _) -> pure (toolText [i|已发送表情 #{sticker.stickerId}（#{sticker.stickerEmotion}）。不要再用文字描述它。|])
            [] ->
              pure (toolFailure Failure
                { category = ExternalServiceUnavailable
                , userMessage = "表情发送失败。"
                , detail = Text.intercalate "\n" (lefts sent)
                })
