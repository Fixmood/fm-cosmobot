{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Bot.Trigger
Description : Per-chat trigger mode configuration
-}
module Bot.Trigger
  ( TriggerMode (..)
  , TriggerConfig (..)
  , loadTriggerConfig
  , loadTriggerConfigByKey
  , saveTriggerConfig
  , saveTriggerConfigByKey
  , clearTriggerConfig
  , clearTriggerConfigByKey
  , listTriggerConfigs
  , triggerModeName
  , triggerConfigHasMode
  , parseTriggerMode
  , renderTriggerConfig
  , triggerMatches
  )
where

import Bot.Core.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Encoding as TextEncoding
import System.Directory

data TriggerMode
  = TriggerPrefix
  | TriggerMention
  | TriggerReply
  | TriggerName
  | TriggerKeyword
  | TriggerImage
  | TriggerFile
  | TriggerAudio
  | TriggerSticker
  | TriggerAll
  deriving (Eq, Ord, Show, Read, Generic)

instance Aeson.ToJSON TriggerMode where
  toJSON = Aeson.String . triggerModeName

instance Aeson.FromJSON TriggerMode where
  parseJSON = Aeson.withText "trigger mode" \value ->
    maybe (fail "unknown trigger mode") pure (parseTriggerMode value)

data TriggerConfig = TriggerConfig
  { modes :: ![TriggerMode]
  , keywords :: ![Text]
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON TriggerConfig where
  toJSON TriggerConfig{modes, keywords} = Aeson.object ["modes" Aeson..= modes, "keywords" Aeson..= keywords]

instance Aeson.FromJSON TriggerConfig where
  parseJSON = Aeson.withObject "trigger config" \object ->
    TriggerConfig <$> object Aeson..:? "modes" Aeson..!= [] <*> object Aeson..:? "keywords" Aeson..!= []

triggerConfigPath :: FilePath
triggerConfigPath = "trigger-config.json"

loadTriggerConfig :: IncomingMessage -> IO (Maybe TriggerConfig)
loadTriggerConfig message = do
  loadTriggerConfigByKey (scopeKey message)

loadTriggerConfigByKey :: Text -> IO (Maybe TriggerConfig)
loadTriggerConfigByKey requestedKey = do
  exists <- doesFileExist triggerConfigPath
  if not exists then pure Nothing else do
    raw <- TextIO.readFile triggerConfigPath
    pure $ Aeson.decodeStrict (TextEncoding.encodeUtf8 raw) >>= lookupConfig requestedKey
  where
    lookupConfig requestedKey (Aeson.Object object) = KeyMap.lookup (Key.fromText requestedKey) object >>= AesonTypes.parseMaybe Aeson.parseJSON
    lookupConfig _ _ = Nothing

saveTriggerConfig :: IncomingMessage -> TriggerConfig -> IO ()
saveTriggerConfig message config = do
  saveTriggerConfigByKey (scopeKey message) config

saveTriggerConfigByKey :: Text -> TriggerConfig -> IO ()
saveTriggerConfigByKey key config = do
  existing <- loadAll
  let updated = (key, config) : filter ((/= key) . fst) existing
  TextIO.writeFile triggerConfigPath (TextEncoding.decodeUtf8 (LazyByteString.toStrict (Aeson.encode (Aeson.object [Key.fromText configKey Aeson..= value | (configKey, value) <- updated]))))

clearTriggerConfig :: IncomingMessage -> IO ()
clearTriggerConfig message = do
  clearTriggerConfigByKey (scopeKey message)

clearTriggerConfigByKey :: Text -> IO ()
clearTriggerConfigByKey key = do
  existing <- loadAll
  let remaining = filter ((/= key) . fst) existing
  TextIO.writeFile triggerConfigPath (TextEncoding.decodeUtf8 (LazyByteString.toStrict (Aeson.encode (Aeson.object [Key.fromText configKey Aeson..= value | (configKey, value) <- remaining]))))

listTriggerConfigs :: IO [(Text, TriggerConfig)]
listTriggerConfigs = loadAll

loadAll :: IO [(Text, TriggerConfig)]
loadAll = do
  exists <- doesFileExist triggerConfigPath
  if not exists then pure [] else do
    raw <- TextIO.readFile triggerConfigPath
    pure $ maybe [] (mapMaybe decodePair . objectPairs) (Aeson.decodeStrict (TextEncoding.encodeUtf8 raw) :: Maybe Aeson.Value)
  where
    decodePair (configKey, value) = (configKey,) <$> AesonTypes.parseMaybe Aeson.parseJSON value
    objectPairs (Aeson.Object object) = [(Key.toText configKey, value) | (configKey, value) <- KeyMap.toList object]
    objectPairs _ = []

scopeKey :: IncomingMessage -> Text
scopeKey message = chatPlatformKey message.platform <> ":" <> maybe (Text.intercalate "," message.chatAliases) (Text.pack . show) message.chatId

triggerModeName :: TriggerMode -> Text
triggerModeName = \case
  TriggerPrefix -> "prefix"
  TriggerMention -> "mention"
  TriggerReply -> "reply"
  TriggerName -> "name"
  TriggerKeyword -> "keyword"
  TriggerImage -> "image"
  TriggerFile -> "file"
  TriggerAudio -> "audio"
  TriggerSticker -> "sticker"
  TriggerAll -> "all"

triggerConfigHasMode :: TriggerMode -> TriggerConfig -> Bool
triggerConfigHasMode mode config = mode `elem` config.modes

parseTriggerMode :: Text -> Maybe TriggerMode
parseTriggerMode raw = case Text.toCaseFold (Text.strip raw) of
  "prefix" -> Just TriggerPrefix
  "前缀" -> Just TriggerPrefix
  "fm空格" -> Just TriggerPrefix
  "mention" -> Just TriggerMention
  "@" -> Just TriggerMention
  "@fm" -> Just TriggerMention
  "回复" -> Just TriggerReply
  "reply" -> Just TriggerReply
  "name" -> Just TriggerName
  "名字" -> Just TriggerName
  "叫名" -> Just TriggerName
  "keyword" -> Just TriggerKeyword
  "关键词" -> Just TriggerKeyword
  "image" -> Just TriggerImage
  "图片" -> Just TriggerImage
  "file" -> Just TriggerFile
  "文件" -> Just TriggerFile
  "audio" -> Just TriggerAudio
  "语音" -> Just TriggerAudio
  "sticker" -> Just TriggerSticker
  "贴纸" -> Just TriggerSticker
  "all" -> Just TriggerAll
  "全部" -> Just TriggerAll
  _ -> Nothing

renderTriggerConfig :: Maybe TriggerConfig -> Text
renderTriggerConfig Nothing = "当前使用默认触发规则。"
renderTriggerConfig (Just TriggerConfig{modes, keywords}) =
  "当前触发方式：" <> Text.intercalate "、" (map triggerModeName modes)
    <> if null keywords then "。" else "；关键词：" <> Text.intercalate "、" keywords <> "。"

triggerMatches :: Text -> TriggerConfig -> IncomingMessage -> Bool
triggerMatches botName TriggerConfig{modes, keywords} message = any matches modes
  where
    text = Text.strip message.text
    folded = Text.toCaseFold text
    name = Text.toCaseFold (Text.strip botName)
    matches TriggerPrefix = (name <> " ") `Text.isPrefixOf` folded || (name <> "　") `Text.isPrefixOf` folded
    matches TriggerMention = message.digest.mentionsBot
    matches TriggerReply = isJust message.replyToMessageId
    matches TriggerName = not (Text.null name) && name `Text.isPrefixOf` folded
    matches TriggerKeyword = any ((`Text.isInfixOf` folded) . Text.toCaseFold . Text.strip) keywords
    matches TriggerImage = not (null message.imageUrls)
    matches TriggerFile = not (null message.files)
    matches TriggerAudio = "[语音]" `Text.isInfixOf` folded
    matches TriggerSticker = "[贴纸]" `Text.isInfixOf` folded
    matches TriggerAll = message.kind == ChatGroup
