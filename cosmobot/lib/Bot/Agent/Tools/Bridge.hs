module Bot.Agent.Tools.Bridge
  ( fmBridgeStatusTool
  , fmBridgeManageTool
  , fmBridgeTestTool
  , fmRelayToOwnerTool
  , fmRelayMessageTool
  , fmTakeoverManageTool
  ) where

import Bot.Agent.Tool
import Bot.Agent.Tools.Common
  ( fieldBoolean
  , jsonText
  , optionalText
  , requiredText
  , superuserOnly
  )
import Bot.Agent.Types (Context (..), toolText)
import qualified Bot.Chat.Bridge.FM as FMBridge
import Bot.Core.Message
  ( ChatKind (ChatGroup, ChatPrivate)
  , ChatPlatform (PlatformQQ, PlatformMatrix)
  , chatPlatformKey
  , IncomingMessage (..)
  , MessageDigest (..)
  )
import qualified Bot.Effect.Chat as Chat
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text

fmBridgeStatusTool :: IOE :> es => Tool (Eff es)
fmBridgeStatusTool =
  allowWhen superuserOnly
  . withDescription "Inspect FM's Matrix-to-QQ bridge mapping and behavior without sending a message. Use this when the owner asks whether the bridge is configured or how it currently routes messages. This reports configuration, not live delivery health; use fm_bridge_test for an end-to-end check."
  $ tool "fm_bridge_status" noArguments do
      context <- askToolContext
      config <- liftIO FMBridge.readFMBridgeConfig
      pure . toolText . jsonText $ Aeson.object
        [ "configured" Aeson..= True
        , "enabled" Aeson..= config.enabled
        , "matrix_room_id" Aeson..= config.matrixRoomId
        , "qq_group_id" Aeson..= config.qqGroupId
        , "owner_matrix_id" Aeson..= FMBridge.fmOwnerMatrixId
        , "owner_qq_id" Aeson..= FMBridge.fmOwnerQQId
        , "owner_messages_matrix_to_qq" Aeson..= True
        , "fm_replies_matrix_and_qq" Aeson..= True
        , "loop_protection" Aeson..= True
        , "current_message_uses_bridge_pipeline" Aeson..= FMBridge.usesMatrixReplyPipeline context.message
        , "live_delivery_health" Aeson..= ("not_tested" :: Text)
        ]

fmBridgeManageTool :: IOE :> es => Tool (Eff es)
fmBridgeManageTool =
  allowWhen superuserOnly
  . withDescription "Manage FM's persistent Matrix-to-QQ bridge from natural language. Actions: status, enable, disable, configure. Configure accepts a Matrix room ID and/or QQ group ID and applies immediately without rebuilding. Only the owner may use it."
  $ tool "fm_bridge_manage"
      ( requiredText "action" "One of: status, enable, disable, configure."
      , optionalText "matrix_room_id" "Optional Matrix room ID beginning with !."
      , optionalText "qq_group_id" "Optional numeric QQ group ID."
      )
      \action requestedRoom requestedGroup -> do
        current <- liftIO FMBridge.readFMBridgeConfig
        let normalized = Text.toLower (Text.strip action)
        case normalized of
          "status" -> pure (toolText (renderBridgeConfig current))
          "enable" -> saveBridgeConfig current{FMBridge.enabled = True}
          "disable" -> saveBridgeConfig current{FMBridge.enabled = False}
          "configure" ->
            case validateBridgeConfig current requestedRoom requestedGroup of
              Left reason -> pure (toolText reason)
              Right updated -> saveBridgeConfig updated
          _ -> pure (toolText "action 必须是 status、enable、disable 或 configure。")
  where
    saveBridgeConfig config = do
      liftIO (FMBridge.writeFMBridgeConfig config)
      pure (toolText ("桥接配置已立即生效：" <> renderBridgeConfig config))

validateBridgeConfig
  :: FMBridge.FMBridgeConfig
  -> Maybe Text
  -> Maybe Text
  -> Either Text FMBridge.FMBridgeConfig
validateBridgeConfig current requestedRoom requestedGroup = do
  room <- case Text.strip <$> requestedRoom of
    Nothing -> Right current.matrixRoomId
    Just value
      | "!" `Text.isPrefixOf` value && ":" `Text.isInfixOf` value -> Right value
      | otherwise -> Left "Matrix room ID 必须以 ! 开头并包含服务器名，例如 !abc:g24.at。"
  group <- case Text.strip <$> requestedGroup of
    Nothing -> Right current.qqGroupId
    Just value -> case readMaybe (toString value) of
      Just parsed | parsed > 0 -> Right parsed
      _ -> Left "QQ 群号必须是正整数。"
  when (isNothing requestedRoom && isNothing requestedGroup) $
    Left "configure 至少需要 matrix_room_id 或 qq_group_id。"
  Right current{FMBridge.matrixRoomId = room, FMBridge.qqGroupId = group}

renderBridgeConfig :: FMBridge.FMBridgeConfig -> Text
renderBridgeConfig config =
  (if config.enabled then "已启用" else "已停用")
    <> "；Matrix 房间：" <> config.matrixRoomId
    <> "；QQ 群：" <> show config.qqGroupId

fmBridgeTestTool :: (Chat.Chat :> es, IOE :> es) => Tool (Eff es)
fmBridgeTestTool =
  noisy
  . allowWhen superuserOnly
  . withDescription "Run one end-to-end FM bridge test by sending a short marked message to both the configured Matrix room and QQ group through the real bridge pipeline. Only call when the owner explicitly asks to test the bridge. Set confirm=true only for that explicit request. Never use arbitrary user content as a relay."
  $ tool "fm_bridge_test"
      ( requiredArgument (fieldBoolean "confirm" "Must be true after the owner explicitly asks to send a bridge test message.") :: ToolArgument Bool
      , optionalText "label" "Optional short diagnostic label, limited to 40 characters."
      )
      \confirm label -> do
        context <- askToolContext
        if not confirm
          then pure (toolText "桥接测试未发送：需要主人明确要求测试，并将 confirm 设为 true。")
          else do
            config <- liftIO FMBridge.readFMBridgeConfig
            if not config.enabled
              then pure (toolText "桥接当前已停用，请先启用后再测试。")
              else do
                let cleanLabel = Text.take 40 . Text.filter (`notElem` ['\r', '\n']) . Text.strip $ fromMaybe "手动检查" label
                    body = "【FM桥接测试】" <> cleanLabel
                    target = bridgeTestTarget config context.message
                results <- Chat.replyTo target body
                let errors = lefts results
                    deliveries = mapMaybe FMBridge.bridgeDeliveryTargets (rights results)
                    matrixDelivered = not (null deliveries)
                    qqDelivered = any (not . null . snd) deliveries
                    status
                      | matrixDelivered && qqDelivered = "桥接测试成功：Matrix 与 QQ 均已收到测试消息。"
                      | matrixDelivered = "桥接测试部分成功：Matrix 已收到，但 QQ 投递失败。"
                      | otherwise = "桥接测试失败：Matrix 未成功投递，因此没有继续同步到 QQ。"
                    detail = if null errors then "" else " 失败信息：" <> Text.intercalate "；" errors
                pure (toolText (status <> detail))

fmRelayToOwnerTool :: Chat.Chat :> es => Tool (Eff es)
fmRelayToOwnerTool =
  withDescription "Send a single private QQ message to the FM owner when a user explicitly asks FM to tell Fix哥 something. The message is delivered privately to the fixed owner account, not posted in the current group or Matrix room. Do not call for ordinary conversation or an implied request."
  $ tool "fm_relay_to_owner"
      (requiredText "content" "The message to tell Fix哥, without adding interpretation.")
      \content -> do
        context <- askToolContext
        if not (explicitRelayRequest context.message.text)
          then pure (toolText "未发送：只有明确要求“告诉 Fix哥”“传话”或“转告”时，FM 才会发送私聊。")
          else do
            let source = fromMaybe "未知用户" (context.message.senderUsername <|> context.message.senderId)
                body = "😻" <> source <> "：" <> Text.strip content
            sent <- Chat.replyTo (ownerPrivateTarget context.message) body
            if any isRight sent
              then pure (toolText "已通过 QQ 私聊转告 Fix哥。")
              else pure (toolText "传话失败：QQ 私聊消息没有成功发送。")

explicitRelayRequest :: Text -> Bool
explicitRelayRequest value =
  let normalized = Text.toCaseFold (Text.filter (not . (`elem` [' ', '\t', '\x3000'])) value)
  in any (`Text.isInfixOf` normalized)
      [ "告诉", "传话", "转告", "通知" ]

ownerPrivateTarget :: IncomingMessage -> IncomingMessage
ownerPrivateTarget message =
  message
    { platform = PlatformQQ
    , kind = ChatPrivate
    , chatId = Nothing
    , chatAliases = []
    , digest = message.digest
        { chatIsAllowed = True
        , senderIsAllowed = True
        , senderIsSuperuser = True
        , mentionsBot = False
        , botId = Just FMBridge.fmBotQQId
        }
    , senderId = Just FMBridge.fmOwnerQQId
    , senderUsername = Just "Fixmood"
    , messageId = Nothing
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , files = []
    , text = ""
    , raw = Aeson.Null
    }

fmRelayMessageTool :: Chat.Chat :> es => Tool (Eff es)
fmRelayMessageTool =
  withDescription "Send one private QQ message to another person when a user explicitly asks FM to tell, relay, or notify them. target may be a positive QQ number or a unique nickname/card in the current QQ group. Resolve nicknames from the current group only; if there are zero or multiple matches, do not send. Never post the relay in the current chat."
  $ tool "fm_relay_message"
      ( requiredText "target" "Positive QQ number, or a unique nickname/card in the current QQ group."
      , requiredText "content" "The exact message to relay, without adding interpretation."
      )
      \target content -> do
        context <- askToolContext
        if not (explicitRelayRequest context.message.text)
          then pure (toolText "未发送：只有明确要求告诉、传话或转告时，FM 才会发送私聊。")
          else do
            resolved <- resolveRelayTarget target context.message
            case resolved of
              Left reason -> pure (toolText reason)
              Right userId -> do
                let source = fromMaybe "未知用户" (context.message.senderUsername <|> context.message.senderId)
                    body = "😻" <> source <> "：" <> Text.strip content
                sent <- Chat.replyTo (qqPrivateTarget context.message userId) body
                if any isRight sent
                  then pure (toolText ("已通过 QQ 私聊转告 " <> target <> "。"))
                  else pure (toolText ("传话失败：QQ 私聊消息没有成功发送给 " <> target <> "。"))

resolveRelayTarget :: Chat.Chat :> es => Text -> IncomingMessage -> Eff es (Either Text Text)
resolveRelayTarget rawTarget message =
  let target = Text.strip rawTarget
  in case readMaybe (toString target) :: Maybe Integer of
       Just userId | userId > 0 -> pure (Right (show userId))
       _ -> case (message.kind, message.chatId) of
         (ChatGroup, Just _) -> do
           members <- Chat.listGroupMembers message
           pure (resolveMemberValue target members)
         _ -> pure (Left "按昵称传话只能在 QQ 群中使用，请改用对方 QQ 号。")

resolveMemberValue :: Text -> Maybe Aeson.Value -> Either Text Text
resolveMemberValue target members =
  case relayMemberMatches target members of
    [] -> Left ("当前群没有找到名为“" <> target <> "”的成员，请改用 QQ 号。")
    [userId] -> Right userId
    _ -> Left ("当前群有多名成员匹配“" <> target <> "”，请改用 QQ 号明确指定。")

relayMemberMatches :: Text -> Maybe Aeson.Value -> [Text]
relayMemberMatches target members =
  [ userId
  | (userId, names) <- fromMaybe [] (members >>= parseRelayMembers)
  , target `elem` names
  ]

parseRelayMembers :: Aeson.Value -> Maybe [(Text, [Text])]
parseRelayMembers = AesonTypes.parseMaybe $ Aeson.withArray "QQ members" (traverse parseRelayMember . toList)
  where
    parseRelayMember = Aeson.withObject "QQ member" $ \object -> do
      userId <- parseRelayUserId =<< object Aeson..: "user_id"
      nickname <- object Aeson..:? "nickname"
      card <- object Aeson..:? "card"
      pure (userId, catMaybes [card, nickname])

    parseRelayUserId value =
      (Text.pack . show <$> (Aeson.parseJSON value :: AesonTypes.Parser Integer))
        <|> Aeson.parseJSON value

qqPrivateTarget :: IncomingMessage -> Text -> IncomingMessage
qqPrivateTarget message userId =
  (ownerPrivateTarget message){senderId = Just userId, senderUsername = Nothing}

bridgeTestTarget :: FMBridge.FMBridgeConfig -> IncomingMessage -> IncomingMessage
bridgeTestTarget config message =
  message
    { platform = PlatformQQ
    , kind = ChatGroup
    , chatId = Just config.qqGroupId
    , chatAliases = []
    , digest = message.digest
        { chatIsAllowed = True
        , senderIsAllowed = True
        , senderIsSuperuser = True
        }
    , messageId = Nothing
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , files = []
    , text = ""
    , raw = Aeson.Null
    }

fmTakeoverManageTool :: (Chat.Chat :> es, IOE :> es) => Tool (Eff es)
fmTakeoverManageTool =
  noisy
  . allowWhen superuserOnly
  . withDescription "Manage FM's bidirectional takeover relay. Actions: start, status, stop. start requires target_platform=qq or matrix, target_kind=group/private, and target_id (QQ group or user number, or full Matrix room ID). The current chat becomes the control conversation: target messages are relayed here as 😻Name：content, and the owner's messages here are relayed to the target as 😼Fix哥：content. Only the owner may use it."
  $ tool "fm_takeover_manage"
      ( requiredText "action" "One of: start, status, stop."
      , optionalText "target_platform" "Required for start: qq or matrix."
      , optionalText "target_kind" "For QQ: group or private; Matrix uses group."
      , optionalText "target_id" "Required for start: QQ group number or full Matrix room ID."
      )
      \action requestedPlatform requestedKind requestedTarget -> do
        context <- askToolContext
        let normalized = Text.toLower (Text.strip action)
        case normalized of
          "status" -> do
            state <- liftIO FMBridge.readFMTakeoverState
            pure (toolText (renderTakeoverStatus state))
          "stop" -> do
            current <- liftIO FMBridge.readFMTakeoverState
            case current of
              Nothing -> pure (toolText "当前没有正在进行的 FM 接管。")
              Just state -> do
                announcementResults <- Chat.replyTo
                  (FMBridge.takeoverTargetMessage (FMBridge.takeoverSource state))
                  "😻FM：这里已解除 Fix哥的接管。"
                liftIO (FMBridge.writeFMTakeoverState Nothing)
                let announcement =
                      if any isRight announcementResults
                        then "目标提示已发送。"
                        else "目标提示发送失败，但接管状态已关闭。"
                pure (toolText ("已结束 FM 接管，消息转发已停止；" <> announcement))
          "start" ->
            case (requestedPlatform, requestedKind, requestedTarget) of
              (Just platform, maybeKind, Just target) ->
                case FMBridge.takeoverAddressFromRequest platform (fromMaybe "group" maybeKind) target of
                  Left reason -> pure (toolText reason)
                  Right sourceTarget -> do
                    let state = FMBridge.FMTakeoverState
                          { FMBridge.takeoverEnabled = True
                          , FMBridge.takeoverSource = sourceTarget
                          , FMBridge.takeoverDestination = FMBridge.takeoverAddressFromMessage context.message
                          }
                    liftIO (FMBridge.writeFMTakeoverState (Just state))
                    announcementResults <- Chat.replyTo
                      (FMBridge.takeoverTargetMessage sourceTarget)
                      "😻FM：这里已被 Fix哥接管。"
                    let announcement =
                          if any isRight announcementResults
                            then "；已向目标发送接管提示。"
                            else "；接管已开启，但目标提示发送失败。"
                    pure (toolText ("已开始接管：" <> renderAddress sourceTarget <> "；消息会转发到当前对话" <> announcement))
              _ -> pure (toolText "start 需要同时提供 target_platform 和 target_id。")
          _ -> pure (toolText "action 必须是 status、start 或 stop。")
  where
    renderTakeoverStatus Nothing = "当前没有正在进行的 FM 接管。"
    renderTakeoverStatus (Just state)
      | not (FMBridge.takeoverEnabled state) = "当前没有正在进行的 FM 接管。"
      | otherwise = "接管中：" <> renderAddress (FMBridge.takeoverSource state) <> "；回传到：" <> renderAddress (FMBridge.takeoverDestination state)

    renderAddress address =
      case FMBridge.takeoverPlatform address of
        PlatformQQ -> case FMBridge.takeoverKind address of
          ChatPrivate -> "QQ 私聊 " <> fromMaybe "未知用户" (FMBridge.takeoverPeerId address)
          _ -> "QQ群 " <> maybe "未知群" show (FMBridge.takeoverChatId address)
        PlatformMatrix -> fromMaybe "Matrix 房间" (viaNonEmpty head (FMBridge.takeoverAliases address))
        platform -> chatPlatformKey platform
