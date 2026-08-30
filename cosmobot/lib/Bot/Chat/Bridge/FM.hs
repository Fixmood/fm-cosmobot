{-# LANGUAGE FieldSelectors #-}

module Bot.Chat.Bridge.FM
  ( fmMatrixRoomId
  , fmQQGroupId
  , FMBridgeConfig (..)
  , readFMBridgeConfig
  , writeFMBridgeConfig
  , fmOwnerMatrixId
  , fmOwnerQQId
  , fmBotQQId
  , fmOwnerRelayBody
  , fmOwnerRelayBodyWithImages
  , fmReplyRelayBody
  , fmReplyBody
  , fmStandaloneMessage
  , FMTakeoverAddress (..)
  , FMTakeoverState (..)
  , takeoverAliases
  , takeoverEnabled
  , takeoverSource
  , takeoverDestination
  , readFMTakeoverState
  , writeFMTakeoverState
  , takeoverAddressFromMessage
  , takeoverAddressFromRequest
  , takeoverTargetMessage
  , takeoverMatchesSource
  , takeoverMatchesDestination
  , takeoverIsOwnerMessage
  , takeoverIsControlMessage
  , takeoverIsBotMessage
  , takeoverIncomingBody
  , takeoverIncomingBodyWithImages
  , takeoverIncomingBodyWithName
  , takeoverOwnerBody
  , takeoverOwnerBodyWithImages
  , markFMAgentReply
  , isFMAgentReply
  , matrixOwnerAsQQ
  , isMirroredQQMatrixMessage
  , isMatrixOwnerBridge
  , usesMatrixReplyPipeline
  , matrixReplyTarget
  , qqRelayTarget
  , bridgeDeliveryMessageId
  , bridgeDeliveryTargets
  ) where

import Bot.Core.Message
import qualified Bot.Core.ReplyBody as ReplyBody
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory (createDirectoryIfMissing, renameFile)
import System.IO.Error (catchIOError)
import System.IO.Unsafe (unsafePerformIO)

fmMatrixRoomId :: Text
fmMatrixRoomId = "!juVzWRcyQMbLRCTaNh:matrix.org"

fmQQGroupId :: Integer
fmQQGroupId = 906230260

fmOwnerMatrixId :: Text
fmOwnerMatrixId = "@fixmood:g24.at"

fmOwnerQQId :: Text
fmOwnerQQId = "2822751355"

fmBotQQId :: Text
fmBotQQId = "3471095459"

data FMBridgeConfig = FMBridgeConfig
  { enabled :: !Bool
  , matrixRoomId :: !Text
  , qqGroupId :: !Integer
  }
  deriving (Eq, Show)

bridgeConfigPath :: FilePath
bridgeConfigPath = "/data/fm-bridge-config.json"

defaultFMBridgeConfig :: FMBridgeConfig
defaultFMBridgeConfig = FMBridgeConfig
  { enabled = True
  , matrixRoomId = fmMatrixRoomId
  , qqGroupId = fmQQGroupId
  }

instance Aeson.ToJSON FMBridgeConfig where
  toJSON config = Aeson.object
    [ "enabled" Aeson..= config.enabled
    , "matrix_room_id" Aeson..= config.matrixRoomId
    , "qq_group_id" Aeson..= config.qqGroupId
    ]

instance Aeson.FromJSON FMBridgeConfig where
  parseJSON = Aeson.withObject "FM bridge config" \o -> FMBridgeConfig
    <$> o Aeson..:? "enabled" Aeson..!= True
    <*> o Aeson..:? "matrix_room_id" Aeson..!= fmMatrixRoomId
    <*> o Aeson..:? "qq_group_id" Aeson..!= fmQQGroupId

{-# NOINLINE bridgeConfigRef #-}
bridgeConfigRef :: IORef.IORef FMBridgeConfig
bridgeConfigRef = unsafePerformIO (IORef.newIORef =<< loadFMBridgeConfig)

loadFMBridgeConfig :: IO FMBridgeConfig
loadFMBridgeConfig =
  catchIOError
    (fromMaybe defaultFMBridgeConfig . Aeson.decode <$> LazyByteString.readFile bridgeConfigPath)
    (const (pure defaultFMBridgeConfig))

readFMBridgeConfig :: IO FMBridgeConfig
readFMBridgeConfig = IORef.readIORef bridgeConfigRef

writeFMBridgeConfig :: FMBridgeConfig -> IO ()
writeFMBridgeConfig config = do
  createDirectoryIfMissing True "/data"
  let temporary = bridgeConfigPath <> ".tmp"
  LazyByteString.writeFile temporary (Aeson.encode config)
  renameFile temporary bridgeConfigPath
  IORef.writeIORef bridgeConfigRef config

data FMTakeoverAddress = FMTakeoverAddress
  { takeoverPlatform :: !ChatPlatform
  , takeoverKind :: !ChatKind
  , takeoverChatId :: !(Maybe Integer)
  , takeoverPeerId :: !(Maybe Text)
  , takeoverAliases :: ![Text]
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON FMTakeoverAddress where
  toJSON address = Aeson.object
    [ "platform" Aeson..= address.takeoverPlatform
    , "kind" Aeson..= address.takeoverKind
    , "chat_id" Aeson..= address.takeoverChatId
    , "peer_id" Aeson..= address.takeoverPeerId
    , "aliases" Aeson..= address.takeoverAliases
    ]

instance Aeson.FromJSON FMTakeoverAddress where
  parseJSON = Aeson.withObject "FM takeover address" \o -> FMTakeoverAddress
    <$> o Aeson..: "platform"
    <*> o Aeson..: "kind"
    <*> o Aeson..:? "chat_id"
    <*> o Aeson..:? "peer_id"
    <*> o Aeson..:? "aliases" Aeson..!= []

data FMTakeoverState = FMTakeoverState
  { takeoverEnabled :: !Bool
  , takeoverSource :: !FMTakeoverAddress
  , takeoverDestination :: !FMTakeoverAddress
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON FMTakeoverState where
  toJSON state = Aeson.object
    [ "enabled" Aeson..= state.takeoverEnabled
    , "source" Aeson..= state.takeoverSource
    , "destination" Aeson..= state.takeoverDestination
    ]

instance Aeson.FromJSON FMTakeoverState where
  parseJSON = Aeson.withObject "FM takeover state" \o -> FMTakeoverState
    <$> o Aeson..:? "enabled" Aeson..!= False
    <*> o Aeson..: "source"
    <*> o Aeson..: "destination"

takeoverStatePath :: FilePath
takeoverStatePath = "/data/fm-takeover.json"

{-# NOINLINE takeoverStateRef #-}
takeoverStateRef :: IORef.IORef (Maybe FMTakeoverState)
takeoverStateRef = unsafePerformIO (IORef.newIORef =<< loadFMTakeoverState)

loadFMTakeoverState :: IO (Maybe FMTakeoverState)
loadFMTakeoverState =
  catchIOError
    (Aeson.decode <$> LazyByteString.readFile takeoverStatePath)
    (const (pure Nothing))

readFMTakeoverState :: IO (Maybe FMTakeoverState)
readFMTakeoverState = IORef.readIORef takeoverStateRef

writeFMTakeoverState :: Maybe FMTakeoverState -> IO ()
writeFMTakeoverState state = do
  createDirectoryIfMissing True "/data"
  let temporary = takeoverStatePath <> ".tmp"
  LazyByteString.writeFile temporary (Aeson.encode state)
  renameFile temporary takeoverStatePath
  IORef.writeIORef takeoverStateRef state

takeoverAddressFromMessage :: IncomingMessage -> FMTakeoverAddress
takeoverAddressFromMessage message = FMTakeoverAddress
  { takeoverPlatform = message.platform
  , takeoverKind = message.kind
  , takeoverChatId = message.chatId
  , takeoverPeerId = if message.kind == ChatPrivate then message.senderId else Nothing
  , takeoverAliases = message.chatAliases
  }

takeoverAddressFromRequest :: Text -> Text -> Text -> Either Text FMTakeoverAddress
takeoverAddressFromRequest platformText kindText targetText = do
  platform <- case Text.toLower (Text.strip platformText) of
    "qq" -> Right PlatformQQ
    "matrix" -> Right PlatformMatrix
    value -> Left ("接管目标平台只能是 qq 或 matrix，收到：" <> value)
  let target = Text.strip targetText
      targetKind = Text.toLower (Text.strip kindText)
  if Text.null target
    then Left "接管目标不能为空。"
    else case platform of
      PlatformMatrix
        | "!" `Text.isPrefixOf` target && ":" `Text.isInfixOf` target ->
            Right FMTakeoverAddress
              { takeoverPlatform = PlatformMatrix
              , takeoverKind = ChatGroup
              , takeoverChatId = Just (fromMaybe 0 (readMaybe (toString target)))
              , takeoverPeerId = Nothing
              , takeoverAliases = [target]
              }
        | otherwise -> Left "Matrix 接管目标必须是完整房间 ID，例如 !abc:g24.at。"
      PlatformQQ
        | targetKind `elem` ["private", "user", "person", "私聊"]
        , Just userId <- readMaybe (toString target), userId > 0 ->
            Right FMTakeoverAddress
              { takeoverPlatform = PlatformQQ
              , takeoverKind = ChatPrivate
              , takeoverChatId = Nothing
              , takeoverPeerId = Just target
              , takeoverAliases = []
              }
        | targetKind `notElem` ["", "group", "群"] ->
            Left "QQ target_kind 只能是 group 或 private。"
        | Just groupId <- readMaybe (toString target), groupId > 0 ->
            Right FMTakeoverAddress
              { takeoverPlatform = PlatformQQ
              , takeoverKind = ChatGroup
              , takeoverChatId = Just groupId
              , takeoverPeerId = Nothing
              , takeoverAliases = []
              }
        | otherwise -> Left "QQ 接管目标需要填写正整数群号或私聊账号。"
      _ -> Left "不支持的接管目标。"

takeoverTargetMessage :: FMTakeoverAddress -> IncomingMessage
takeoverTargetMessage address = IncomingMessage
  { eventKind = IncomingMessageCreated
  , platform = address.takeoverPlatform
  , kind = address.takeoverKind
  , chatId = address.takeoverChatId
  , chatAliases = address.takeoverAliases
  , digest = emptyMessageDigest
      { chatIsAllowed = True
      , senderIsAllowed = True
      , senderIsSuperuser = True
      }
  , senderId = address.takeoverPeerId
  , senderUsername = Nothing
  , messageId = Nothing
  , replyToMessageId = Nothing
  , mentions = []
  , mentionUsernames = []
  , imageUrls = []
  , files = []
  , text = ""
  , raw = Aeson.Null
  }

takeoverMatches :: FMTakeoverAddress -> IncomingMessage -> Bool
takeoverMatches address message =
  case (address.takeoverPlatform, address.takeoverKind, message.platform, message.kind) of
    (PlatformQQ, ChatPrivate, PlatformQQ, ChatPrivate) ->
      let expected = address.takeoverPeerId <|> (show <$> address.takeoverChatId)
          actual = message.senderId <|> (show <$> message.chatId)
      in isJust expected && expected == actual
    _ -> takeoverAddressFromMessage message == address

takeoverMatchesSource :: FMTakeoverState -> IncomingMessage -> Bool
takeoverMatchesSource state = takeoverMatches state.takeoverSource

takeoverMatchesDestination :: FMTakeoverState -> IncomingMessage -> Bool
takeoverMatchesDestination state = takeoverMatches state.takeoverDestination

takeoverIsOwnerMessage :: IncomingMessage -> Bool
takeoverIsOwnerMessage message =
  case message.platform of
    PlatformQQ -> message.senderId == Just fmOwnerQQId
    PlatformMatrix -> message.senderId == Just fmOwnerMatrixId
    _ -> False

takeoverIsBotMessage :: IncomingMessage -> Bool
takeoverIsBotMessage message =
  case message.platform of
    PlatformQQ -> message.senderId == Just fmBotQQId
    PlatformMatrix -> message.senderId == Just "@fm:g24.at"
    _ -> False

takeoverIsControlMessage :: Text -> Bool
takeoverIsControlMessage value =
  let normalized = Text.toLower (Text.filter (not . (`elem` [' ', '\t', '\x3000'])) value)
  in any (`Text.isInfixOf` normalized)
      [ "接管fm", "结束接管", "退出接管", "停止接管", "关闭接管"
      , "接管状态", "接管情况", "查看接管", "接管信息"
      ]

takeoverIncomingBody :: IncomingMessage -> Text
takeoverIncomingBody message =
  "😻" <> fromMaybe "未知用户" (message.senderUsername <|> message.senderId) <> "：" <> cleanImageFilenameBody message.imageUrls message.text

takeoverIncomingBodyWithImages :: IncomingMessage -> Text
takeoverIncomingBodyWithImages message =
  ReplyBody.replyContentToBody ReplyBody.ReplyContent
    { text = takeoverIncomingBody message
    , images = message.imageUrls
    }

takeoverIncomingBodyWithName :: Text -> IncomingMessage -> Text
takeoverIncomingBodyWithName displayName message =
  "😻" <> displayName <> "：" <> message.text

takeoverOwnerBody :: Text -> Text
takeoverOwnerBody body =
  "😼Fixmood：" <> cleanImageFilename body

takeoverOwnerBodyWithImages :: Text -> [Text] -> Text
takeoverOwnerBodyWithImages body images =
  ReplyBody.replyContentToBody ReplyBody.ReplyContent
    { text = takeoverOwnerBody body
    , images
    }

{-# NOINLINE bridgeConfigFor #-}
bridgeConfigFor :: IncomingMessage -> FMBridgeConfig
bridgeConfigFor _ = unsafePerformIO readFMBridgeConfig

bridgeMarker :: Text
bridgeMarker = "fm-bridge:matrix-owner"

agentReplyMarker :: Text
agentReplyMarker = "fm-internal:agent-reply"

bridgeDeliveryPrefix :: Text
bridgeDeliveryPrefix = "fm-bridge-delivery:"

bridgeDeliveryMessageId :: MessageId -> [MessageId] -> MessageId
bridgeDeliveryMessageId matrixMessageId qqMessageIds =
  textMessageId $
    bridgeDeliveryPrefix
      <> TextEncoding.decodeUtf8
        (LazyByteString.toStrict (Aeson.encode payload))
  where
    payload = Aeson.object
      [ "matrix" Aeson..= messageIdText matrixMessageId
      , "qq" Aeson..= map messageIdText qqMessageIds
      ]

bridgeDeliveryTargets :: MessageId -> Maybe (MessageId, [MessageId])
bridgeDeliveryTargets messageId = do
  encoded <- Text.stripPrefix bridgeDeliveryPrefix (messageIdText messageId)
  value <- Aeson.decodeStrict (TextEncoding.encodeUtf8 encoded)
  AesonTypes.parseMaybe parseTargets value
  where
    parseTargets = Aeson.withObject "FM bridge delivery" \o -> do
      matrixMessageId <- textMessageId <$> o Aeson..: "matrix"
      qqMessageIds <- map textMessageId <$> (o Aeson..:? "qq" Aeson..!= [])
      pure (matrixMessageId, qqMessageIds)

fmOwnerRelayBody :: Text -> Text
fmOwnerRelayBody body =
  "😼 Fixmood：" <> cleanImageFilename body

fmOwnerRelayBodyWithImages :: Text -> [Text] -> Text
fmOwnerRelayBodyWithImages body images =
  ReplyBody.replyContentToBody ReplyBody.ReplyContent
    { text = fmOwnerRelayBody (ReplyBody.renderReplyBody body)
    , images = ReplyBody.replyImageUrls body <> images
    }

fmReplyRelayBody :: Text -> Text
fmReplyRelayBody body =
  let ReplyBody.ReplyContent{text, images} = ReplyBody.replyContentFromBody body
      cleanText = stripReplyPrefix text
      prefixedText =
        if Text.null (Text.strip text)
          then ""
          else if isTypingPracticeBody cleanText then cleanText else "😻 FM：" <> cleanText
  in ReplyBody.replyContentToBody ReplyBody.ReplyContent{text = prefixedText, images}

fmReplyBody :: Text -> Text
fmReplyBody body =
  let cleanBody = stripReplyPrefix body
  in if isTypingPracticeBody cleanBody then cleanBody else "😻 FM：" <> cleanBody

isTypingPracticeBody :: Text -> Bool
isTypingPracticeBody body =
  "[FM/" `Text.isPrefixOf` Text.stripStart body
    && "-FM发文" `Text.isInfixOf` body

-- Matrix uses the uploaded image's generated filename as the message body.
-- Do not mirror that implementation detail as visible QQ text, while keeping
-- real captions and ordinary filenames intact.
cleanImageFilenameBody :: [Text] -> Text -> Text
cleanImageFilenameBody images body
  | not (null images) && isGeneratedImageFilename body = ""
  | otherwise = body

cleanImageFilename :: Text -> Text
cleanImageFilename = cleanImageFilenameBody ["bridge-image"]

isGeneratedImageFilename :: Text -> Bool
isGeneratedImageFilename value =
  let normalized = Text.toLower (Text.strip value)
  in normalized `elem` genericImageFilenames || isHexImageFilename normalized

genericImageFilenames :: [Text]
genericImageFilenames =
  [ "image.jpg", "image.jpeg", "image.png", "image.gif", "image.webp"
  , "image_1.jpg", "image_1.jpeg", "image_1.png", "image_1.gif", "image_1.webp"
  , "image-1.jpg", "image-1.jpeg", "image-1.png", "image-1.gif", "image-1.webp"
  ]

isHexImageFilename :: Text -> Bool
isHexImageFilename value =
  case Text.breakOnEnd "." value of
    (stemWithDot, extension)
      | not (Text.null stemWithDot)
      , not (Text.null extension)
      , Text.length (Text.dropEnd 1 stemWithDot) >= 16
      , Text.all isHexDigit (Text.dropEnd 1 stemWithDot) ->
          extension `elem` ["jpg", "jpeg", "png", "gif", "webp"]
    _ -> False
  where
    isHexDigit character =
      character `elem` ['0' .. '9'] || character `elem` ['a' .. 'f'] || character `elem` ['A' .. 'F']

-- Agent output on QQ is sent as a standalone FM message. The private target
-- remains in senderId while clearing messageId prevents OneBot reply quoting.
fmStandaloneMessage :: IncomingMessage -> IncomingMessage
fmStandaloneMessage message
  | message.platform == PlatformQQ && message.kind `elem` [ChatGroup, ChatPrivate] =
      message{messageId = Nothing, replyToMessageId = Nothing, raw = Aeson.Null}
  | otherwise = message

markFMAgentReply :: IncomingMessage -> IncomingMessage
markFMAgentReply message =
  message{chatAliases = filter (/= agentReplyMarker) message.chatAliases <> [agentReplyMarker]}

isFMAgentReply :: IncomingMessage -> Bool
isFMAgentReply message =
  agentReplyMarker `elem` message.chatAliases

stripReplyPrefix :: Text -> Text
stripReplyPrefix value =
  case firstJust (Text.stripPrefix <$> prefixes <*> pure (Text.stripStart value)) of
    Just rest -> stripReplyPrefix rest
    Nothing -> Text.stripStart value
  where
    prefixes =
      [ "😻 FM：", "😻 FM:"
      , "◆ FM：", "◆ FM:"
      , "😼 Fixmood：", "😼 Fixmood:"
      , "✦ Fixmood：", "✦ Fixmood:"
      , "FM：", "FM:"
      ]

matrixOwnerAsQQ :: IncomingMessage -> Maybe IncomingMessage
matrixOwnerAsQQ message = do
  let config = bridgeConfigFor message
  guard config.enabled
  guard (isFMMatrixRoom message)
  guard (message.eventKind == IncomingMessageCreated)
  guard (message.senderId == Just fmOwnerMatrixId)
  pure message
    { platform = PlatformQQ
    , kind = ChatGroup
    , chatId = Just config.qqGroupId
    , chatAliases = [config.matrixRoomId, bridgeMarker]
    , digest = MessageDigest
        { chatIsAllowed = True
        , senderIsAllowed = True
        , senderIsSuperuser = True
        , mentionsBot = message.digest.mentionsBot
        , botId = Just fmBotQQId
        }
    , senderId = Just fmOwnerQQId
    , senderUsername = Just "Fixmood"
    }

isMirroredQQMatrixMessage :: IncomingMessage -> Bool
isMirroredQQMatrixMessage message =
  (bridgeConfigFor message).enabled
    && isFMMatrixRoom message
    && maybe False isQQBridgeUser message.senderId

isMatrixOwnerBridge :: IncomingMessage -> Bool
isMatrixOwnerBridge message =
  let config = bridgeConfigFor message
  in config.enabled
    && message.platform == PlatformQQ
    && message.chatId == Just config.qqGroupId
    && bridgeMarker `elem` message.chatAliases

usesMatrixReplyPipeline :: IncomingMessage -> Bool
usesMatrixReplyPipeline message =
  let config = bridgeConfigFor message
  in config.enabled &&
    ((message.platform == PlatformQQ && message.chatId == Just config.qqGroupId)
      || isFMMatrixRoom message)

matrixReplyTarget :: IncomingMessage -> IncomingMessage
matrixReplyTarget message =
  target
    { platform = PlatformMatrix
    , kind = ChatGroup
    , chatId = Nothing
    , chatAliases = [(bridgeConfigFor message).matrixRoomId]
    , digest = message.digest{botId = Nothing}
    , senderId = Just fmOwnerMatrixId
    , senderUsername = Just fmOwnerMatrixId
    }
  where
    -- A QQ message id is not a Matrix event id. Keeping it here creates an
    -- invalid m.in_reply_to relation that Element cannot resolve.
    target
      | isMatrixOwnerBridge message = message
      | otherwise = message
          { messageId = Nothing
          , replyToMessageId = Nothing
          , raw = Aeson.Null
          }

qqRelayTarget :: IncomingMessage -> IncomingMessage
qqRelayTarget message =
  message
    { platform = PlatformQQ
    , kind = ChatGroup
    , chatId = Just (bridgeConfigFor message).qqGroupId
    , chatAliases = []
    , messageId = Nothing
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    }

isFMMatrixRoom :: IncomingMessage -> Bool
isFMMatrixRoom message =
  let config = bridgeConfigFor message
  in message.platform == PlatformMatrix
    && config.matrixRoomId `elem` message.chatAliases

isQQBridgeUser :: Text -> Bool
isQQBridgeUser userId =
  case Text.stripPrefix "@qq_" userId >>= Text.stripSuffix ":pfeiwu.com" of
    Just qqId -> not (Text.null qqId) && Text.all (`elem` ['0' .. '9']) qqId
    Nothing -> False

firstJust :: [Maybe a] -> Maybe a
firstJust =
  viaNonEmpty head . catMaybes
