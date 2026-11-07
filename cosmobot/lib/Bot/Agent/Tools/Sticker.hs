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
  , loadStickerCatalog
  , pickStickerForRequest
  , sendStickerMessage
  ) where

import Bot.Agent.Tool
import Bot.Agent.Tools.Common (chatTag, jsonText, optionalText, requiredText)
import Bot.Agent.Types
import Bot.Core.Message (IncomingMessage)
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Media as Media
import qualified Bot.Media.Object as MediaObject
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.FilePath ((</>))
import Text.Read (readMaybe)

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

-- | 最近发送记录，用来限频。连发表情会触发 QQ 风控，所以窗口内超过上限就不发。
stickerSendLogPath :: FilePath
stickerSendLogPath = stickerDir </> "recent-sends.log"

stickerRateLimitWindowSeconds :: Integer
stickerRateLimitWindowSeconds = 60

stickerRateLimitMax :: Int
stickerRateLimitMax = 3

-- | 记一次发送；返回 False 表示这一分钟内已经发满，本次不要再发。
--
-- 用文件而不是进程内状态：重启后限额仍然有效，这正是风控关心的窗口。
reserveStickerSend :: (IOE :> es, FileSystem :> es) => Eff es Bool
reserveStickerSend = do
  now <- liftIO (round <$> getPOSIXTime)
  exists <- FileSystem.doesFileExist stickerSendLogPath
  raw <-
    if exists
      then TextEncoding.decodeUtf8 <$> FileSystemByteString.readFile stickerSendLogPath
      else pure ""
  let recent =
        filter (> now - stickerRateLimitWindowSeconds)
          (mapMaybe (readMaybe . Text.unpack) (Text.lines raw))
  if length recent >= stickerRateLimitMax
    then pure False
    else do
      FileSystem.createDirectoryIfMissing True stickerDir
      FileSystemByteString.writeFile stickerSendLogPath
        (TextEncoding.encodeUtf8 (Text.unlines (map (Text.pack . show) (now : recent))))
      pure True

loadStickerCatalog :: (IOE :> es, FileSystem :> es) => Eff es (Maybe StickerCatalog)
loadStickerCatalog = do
  exists <- FileSystem.doesFileExist stickerCatalogPath
  if not exists
    then pure Nothing
    else do
      raw <- FileSystemByteString.readFile stickerCatalogPath
      pure (parseStickerCatalog raw)

-- | Choose a sticker for an explicit request, deterministically.
--
-- The model was given these tools and a written rule and still preferred to
-- render a fresh picture (and, when that tool was withheld, to re-enable it
-- through tool_enable). So for an explicit "send me a sulky sticker" the pick
-- is made here from the message words; the model only gets to comment on it.
--
-- The seed only breaks ties inside the matched pool, so "再来一张" rotates
-- instead of returning the same picture again.
pickStickerForRequest :: Text -> Text -> [Sticker] -> Maybe Sticker
pickStickerForRequest seed request stickers =
  case matchingPool of
    [] -> pickFrom seed stickers
    pool -> pickFrom seed pool
  where
    compact = Text.toCaseFold request
    -- 先找消息里出现的情绪词，再按该词去目录里匹配；词表按具体到笼统排序。
    emotionWords =
      filter (`Text.isInfixOf` compact)
        [ "委屈", "难过", "伤心", "哭", "生气", "愤怒", "崩溃", "开心", "高兴"
        , "躺平", "累", "困", "震惊", "惊讶", "疑惑", "不懂", "调皮", "得意", "可爱"
        ]
    -- 整个匹配池，而不是第一个：同种子下要能轮换出不同的图。
    matchingPool :: [Sticker]
    matchingPool =
      case mapMaybe matchPool emotionWords of
        (pool : _) -> toList pool
        [] -> []
    matchPool word = viaNonEmpty id (matchStickers word stickers)
    pickFrom value pool =
      let offset = Text.foldl' (\acc char -> acc * 31 + fromEnum char) 0 value
      in case drop (offset `mod` max 1 (length pool)) pool of
          (sticker : _) -> Just sticker
          [] -> viaNonEmpty head pool

-- | Send one sticker to the chat without involving the model.
sendStickerMessage
  :: (Chat.Chat :> es, Media.Media :> es, IOE :> es, FileSystem :> es)
  => IncomingMessage
  -> Sticker
  -> Eff es Bool
sendStickerMessage message sticker = do
  let path = stickerDir </> Text.unpack sticker.stickerFile
  exists <- FileSystem.doesFileExist path
  if not exists
    then pure False
    else do
      allowed <- reserveStickerSend
      if not allowed
        then pure False
        else do
          mediaObject <- MediaObject.fileObject ("file://" <> Text.pack path)
          stored <- Media.storeMediaObject mediaObject
          case stored of
            Nothing -> pure False
            Just ref -> not . null . rights <$> Chat.replyTo message (ReplyBody.imageDirective ref)

-- | A rejected argument, not a transport failure: the model should fix the id.
argumentFailure :: Text -> ToolResult
argumentFailure err =
  toolFailure (permanentArgumentFailure err err)

stickerListTool :: (IOE :> es, FileSystem :> es) => Tool (Eff es)
stickerListTool =
  tagged [chatTag]
  . withDescription "List this bot's own curated stickers. Returns id, emotion, description and tags. Call it before sticker_send when you want to attach a picture to a reaction. Optional keyword filters across emotion, description and tags."
  $ tool "sticker_list" (optionalText "keyword" "Optional keyword to filter stickers, e.g. 开心 or 猫.") \maybeKeyword -> do
      catalog <- loadStickerCatalog
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
      catalog <- loadStickerCatalog
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
      allowed <- reserveStickerSend
      if not allowed
        then pure (toolText "表情发得太频繁了，这一分钟先不再发（避免触发风控）。")
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
