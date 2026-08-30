{-|
Module      : Bot.Agent.Tools.Trigger
Description : Per-chat natural-language trigger management
-}
module Bot.Agent.Tools.Trigger
  ( triggerManageTool
  )
where

import Bot.Agent.Tool
import Bot.Agent.Tools.Common
import Bot.Agent.Types
import Bot.Core.Message
import qualified Bot.Trigger as Trigger
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text

data TriggerArgs = TriggerArgs
  { action :: !Text
  , modes :: ![Text]
  , keywords :: ![Text]
  , targetPlatform :: !(Maybe Text)
  , targetKind :: !(Maybe Text)
  , targetId :: !(Maybe Text)
  }

triggerManageTool :: (IOE :> es) => Tool (Eff es)
triggerManageTool =
  allowWhen superuserOnly
  . noisy
  . withDescription "Manage trigger modes. Actions: status, set, add, remove, reset. Without a target, manage the current chat. For cross-chat management, provide target_platform (qq or matrix), target_kind (group or private), and target_id (QQ group/user number or full Matrix room ID). Modes may include prefix (FM + space), mention (@FM), reply, name, keyword, image, file, audio, sticker, and all. Only the owner may change another chat."
  $ tool "trigger_manage"
      (parsedArguments triggerSchema parseTriggerArgs)
      runTriggerManage
  where
    runTriggerManage TriggerArgs{action, modes, keywords, targetPlatform, targetKind, targetId} = do
      context <- askToolContext
      case targetMessage context.message targetPlatform targetKind targetId of
        Left reason -> pure (toolText reason)
        Right message -> do
          let normalizedAction = Text.toCaseFold (Text.strip action)
              parsedModes = mapMaybe Trigger.parseTriggerMode modes
              badModes = filter (isNothing . Trigger.parseTriggerMode) modes
          case normalizedAction of
            "status" -> statusResult message
            "查看" -> statusResult message
            "set" -> setResult message parsedModes badModes keywords
            "设置" -> setResult message parsedModes badModes keywords
            "add" -> updateResult message True parsedModes badModes keywords
            "增加" -> updateResult message True parsedModes badModes keywords
            "添加" -> updateResult message True parsedModes badModes keywords
            "remove" -> updateResult message False parsedModes badModes keywords
            "删除" -> updateResult message False parsedModes badModes keywords
            "关闭" -> updateResult message False parsedModes badModes keywords
            "reset" -> do
              liftIO (Trigger.clearTriggerConfig message)
              pure (toolText "已恢复目标窗口的默认触发规则。")
            "恢复默认" -> do
              liftIO (Trigger.clearTriggerConfig message)
              pure (toolText "已恢复目标窗口的默认触发规则。")
            _ -> pure (invalidArgument "action must be status, set, add, remove, or reset")

    statusResult message = do
      config <- liftIO (Trigger.loadTriggerConfig message)
      pure (toolText (Trigger.renderTriggerConfig config))

    setResult message parsedModes badModes rawKeywords
      | not (null badModes) = pure (invalidArgument ("无法识别触发方式：" <> Text.intercalate "、" badModes))
      | null parsedModes = pure (invalidArgument "set 至少需要一种触发方式。")
      | otherwise = do
          let config = Trigger.TriggerConfig (ordNub parsedModes) (cleanKeywords rawKeywords)
          liftIO (Trigger.saveTriggerConfig message config)
          pure (toolText ("已设置当前窗口的触发方式：" <> Trigger.renderTriggerConfig (Just config)))

    updateResult message adding parsedModes badModes rawKeywords
      | not (null badModes) = pure (invalidArgument ("无法识别触发方式：" <> Text.intercalate "、" badModes))
      | null parsedModes && null rawKeywords = pure (invalidArgument "add/remove 至少需要一种触发方式或关键词。")
      | otherwise = do
          current <- liftIO (Trigger.loadTriggerConfig message)
          let old = fromMaybe (Trigger.TriggerConfig [] []) current
              nextModes = if adding then ordNub (old.modes <> parsedModes) else filter (`notElem` parsedModes) old.modes
              nextKeywords = if null rawKeywords then old.keywords else if adding then ordNub (old.keywords <> cleanKeywords rawKeywords) else filter (`notElem` cleanKeywords rawKeywords) old.keywords
              next = old{Trigger.modes = nextModes, Trigger.keywords = nextKeywords}
          if null nextModes && null nextKeywords
            then liftIO (Trigger.clearTriggerConfig message) >> pure (toolText "已清除自定义触发方式，当前窗口恢复默认规则。")
            else liftIO (Trigger.saveTriggerConfig message next) >> pure (toolText ("已更新当前窗口的触发方式：" <> Trigger.renderTriggerConfig (Just next)))

    cleanKeywords = filter (not . Text.null) . map Text.strip

    invalidArgument message = toolFailure (permanentArgumentFailure message message)

targetMessage :: IncomingMessage -> Maybe Text -> Maybe Text -> Maybe Text -> Either Text IncomingMessage
targetMessage current Nothing Nothing Nothing = Right current
targetMessage current (Just rawPlatform) maybeKind (Just rawTarget) = do
  platform <- case Text.toCaseFold (Text.strip rawPlatform) of
    "qq" -> Right PlatformQQ
    "matrix" -> Right PlatformMatrix
    value -> Left ("目标平台只能是 qq 或 matrix，收到：" <> value)
  let target = Text.strip rawTarget
      kindText = Text.toCaseFold (Text.strip (fromMaybe "group" maybeKind))
  if Text.null target then Left "目标 ID 不能为空。" else case platform of
    PlatformQQ
      | kindText `elem` ["private", "user", "person", "私聊"]
      , Just userId <- readMaybe (toString target)
      , userId > (0 :: Integer) ->
          Right current{platform = PlatformQQ, kind = ChatPrivate, chatId = Just userId, chatAliases = [], senderId = Just target}
      | kindText `elem` ["group", "群"]
      , Just groupId <- readMaybe (toString target)
      , groupId > (0 :: Integer) ->
          Right current{platform = PlatformQQ, kind = ChatGroup, chatId = Just groupId, chatAliases = []}
      | otherwise -> Left "QQ 目标 ID 需要填写正整数群号或私聊账号。"
    PlatformMatrix
      | "!" `Text.isPrefixOf` target && ":" `Text.isInfixOf` target ->
          Right current{platform = PlatformMatrix, kind = ChatGroup, chatId = Nothing, chatAliases = [target], senderId = Nothing}
      | otherwise -> Left "Matrix 目标 ID 必须是完整房间 ID，例如 !abc:g24.at。"
    _ -> Left "不支持的目标平台。"
targetMessage _ (Just _) _ Nothing = Left "指定 target_platform 时必须同时填写 target_id。"
targetMessage _ Nothing _ (Just _) = Left "指定 target_id 时必须同时填写 target_platform。"
targetMessage _ _ _ _ = Left "target_platform、target_kind、target_id 参数不完整。"

triggerSchema :: Aeson.Value
triggerSchema = Aeson.object
  [ "type" Aeson..= ("object" :: Text)
  , "properties" Aeson..= Aeson.object
      [ "action" Aeson..= Aeson.object ["type" Aeson..= ("string" :: Text), "description" Aeson..= ("status, set, add, remove, reset" :: Text)]
      , "modes" Aeson..= snd (fieldTextArray "modes" "Trigger modes: prefix, mention, reply, name, keyword, image, file, audio, sticker, all.")
      , "keywords" Aeson..= snd (fieldTextArray "keywords" "Optional keywords used by keyword mode.")
      , "target_platform" Aeson..= Aeson.object ["type" Aeson..= ("string" :: Text), "description" Aeson..= ("Optional cross-chat target: qq or matrix." :: Text)]
      , "target_kind" Aeson..= Aeson.object ["type" Aeson..= ("string" :: Text), "description" Aeson..= ("Optional target kind: group or private." :: Text)]
      , "target_id" Aeson..= Aeson.object ["type" Aeson..= ("string" :: Text), "description" Aeson..= ("Optional QQ group/user number or full Matrix room ID." :: Text)]
      ]
  , "required" Aeson..= ["action" :: Text]
  ]

parseTriggerArgs :: Aeson.Value -> AesonTypes.Parser TriggerArgs
parseTriggerArgs = Aeson.withObject "trigger arguments" \object ->
  TriggerArgs
    <$> object Aeson..: Key.fromText "action"
    <*> object Aeson..:? Key.fromText "modes" Aeson..!= []
    <*> object Aeson..:? Key.fromText "keywords" Aeson..!= []
    <*> object Aeson..:? Key.fromText "target_platform"
    <*> object Aeson..:? Key.fromText "target_kind"
    <*> object Aeson..:? Key.fromText "target_id"
