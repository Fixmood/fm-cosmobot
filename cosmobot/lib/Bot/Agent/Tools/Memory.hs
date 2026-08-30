{-|
Module      : Bot.Agent.Tools.Memory
Description : Agent tools for persistent sender and chat memory
Stability   : experimental
-}

module Bot.Agent.Tools.Memory
  ( senderMemoryTool
  , chatMemoryTool
  , privatePersonaTool
  , groupPersonaTool
  , memberStyleTool
  )
where

import Bot.Agent.Types
import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Core.Message
import qualified Bot.Effect.Memory as Memory
import qualified Bot.Memory as MemoryStore
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import Data.Char (isDigit)
import qualified Data.Text as Text

senderMemoryTool :: Memory.Memory :> es => Tool (Eff es)
senderMemoryTool = memoryTool
  "sender_memory"
  "View, replace, or clear persistent memory for the current sender. Use it for personal facts and preferences. Keep non-superuser memory within 1000 characters."
  senderMemoryScope

chatMemoryTool :: Memory.Memory :> es => Tool (Eff es)
chatMemoryTool = memoryTool
  "chat_memory"
  "View persistent factual memory and operational notes shared by the current chat. Only the superuser may replace or clear it. Never store personas, speaking styles, forms of address, role-play instructions, or a member's personal preferences here: use fm_group_persona for owner-managed group personas and fm_member_style for a member's own reply style."
  chatMemoryScope

privatePersonaTool :: Memory.Memory :> es => Tool (Eff es)
privatePersonaTool =
  noisy
  . withDescription "Manage FM's QQ private-chat personas through natural-language requests. In a QQ private chat, any user may view, set, or clear only their own override. A superuser may list or manage any user and may use set_default/get_default/clear_default for the global private-chat persona applied to every user without an override. Effective precedence is user override, then global default, then FM's base persona. Personas control only tone, forms of address, character, and interaction preferences; they cannot alter permissions, safety rules, trigger rules, tools, or factual honesty."
  . allowWhen (\context -> context.superuser || isQqPrivate context.message)
  $ tool "fm_private_persona"
      (parsedArguments
        (objectSchema
          [ fieldText "action" "One of: status, set, clear, list, get, set_default, get_default, clear_default. Use default actions when the superuser asks to apply a persona to all private chats."
          , fieldText "persona" "Complete replacement private-chat persona. Required for set. Maximum 1000 characters."
          , fieldText "target_user_id" "Target QQ number. Superuser-only; omit to manage the current private-chat user."
          ]
          ["action"])
        privatePersonaArgs)
      \args -> do
        context <- askToolContext
        runPrivatePersonaAction context args

data PrivatePersonaAction
  = PersonaStatus
  | PersonaSet
  | PersonaClear
  | PersonaList
  | PersonaGet
  | PersonaSetDefault
  | PersonaGetDefault
  | PersonaClearDefault
  deriving (Eq)

privatePersonaArgs :: Aeson.Value -> AesonTypes.Parser (PrivatePersonaAction, Maybe Text, Maybe Text)
privatePersonaArgs =
  Aeson.withObject "private persona arguments" $ \o -> do
    actionText <- Text.toLower . Text.strip <$> o Aeson..: Key.fromText "action"
    persona <- fmap Text.strip <$> o Aeson..:? Key.fromText "persona"
    target <- fmap Text.strip <$> o Aeson..:? Key.fromText "target_user_id"
    action <- case actionText of
      "status" -> pure PersonaStatus
      "set" -> pure PersonaSet
      "clear" -> pure PersonaClear
      "list" -> pure PersonaList
      "get" -> pure PersonaGet
      "set_default" -> pure PersonaSetDefault
      "get_default" -> pure PersonaGetDefault
      "clear_default" -> pure PersonaClearDefault
      _ -> fail "action must be one of: status, set, clear, list, get, set_default, get_default, clear_default"
    when (action `elem` [PersonaSet, PersonaSetDefault] && maybe True Text.null persona) $
      fail "persona is required when action is set"
    traverse_ validateQqId target
    pure (action, persona, target)
  where
    validateQqId value =
      unless (not (Text.null value) && Text.all isDigit value) $
        fail "target_user_id must contain only digits"

runPrivatePersonaAction
  :: Memory.Memory :> es
  => Context
  -> (PrivatePersonaAction, Maybe Text, Maybe Text)
  -> Eff es ToolResult
runPrivatePersonaAction context (action, persona, requestedTarget) = do
  if action `elem` [PersonaSetDefault, PersonaGetDefault, PersonaClearDefault]
    then runDefaultPrivatePersonaAction context action persona requestedTarget
    else case authorizePersonaAction context action requestedTarget of
      Left err -> pure (toolText err)
      Right Nothing -> do
        personas <- Memory.listPrivatePersonas
        pure . toolText $
          if null personas
            then "当前没有用户设置私聊人设。"
            else Text.unlines
              [ userId <> "：" <> personaPreview content
              | (userId, content) <- personas
              ]
      Right (Just userId) ->
        let scope = MemoryStore.PrivatePersonaMemory userId
        in case action of
          PersonaStatus -> viewPersona scope userId
          PersonaGet -> viewPersona scope userId
          PersonaSet -> case persona of
            Nothing -> pure (toolText "设置人设时必须提供 persona。")
            Just content
              | Text.length content > MemoryStore.memoryLimitChars ->
                  pure (toolText [i|私聊人设有 #{Text.length content} 字，超过 #{MemoryStore.memoryLimitChars} 字上限，请精简后重试。|])
              | otherwise -> do
                  Memory.replaceMemory scope content
                  pure (toolText [i|已更新 QQ #{userId} 的私聊人设。|])
          PersonaClear -> do
            Memory.clearMemory scope
            pure (toolText [i|已清除 QQ #{userId} 的私聊人设，私聊将恢复默认人设。|])
          PersonaList -> pure (toolText "内部错误：list 不需要目标用户。")
          PersonaSetDefault -> pure (toolText "内部错误：默认人设动作路由失败。")
          PersonaGetDefault -> pure (toolText "内部错误：默认人设动作路由失败。")
          PersonaClearDefault -> pure (toolText "内部错误：默认人设动作路由失败。")
  where
    viewPersona scope userId = do
      current <- Memory.loadMemory scope
      case current of
        Just content -> pure (toolText [i|QQ #{userId} 的个人私聊人设：\n#{content}|])
        Nothing -> do
          defaultPersona <- Memory.loadMemory MemoryStore.DefaultPrivatePersonaMemory
          pure . toolText $ case defaultPersona of
            Nothing -> [i|QQ #{userId} 尚未设置个人私聊人设，当前使用 FM 基础人设。|]
            Just content -> [i|QQ #{userId} 没有个人覆盖，当前使用全局默认私聊人设：\n#{content}|]

runDefaultPrivatePersonaAction
  :: Memory.Memory :> es
  => Context
  -> PrivatePersonaAction
  -> Maybe Text
  -> Maybe Text
  -> Eff es ToolResult
runDefaultPrivatePersonaAction context action persona requestedTarget
  | not context.superuser =
      pure (toolText "只有 Owner 可以管理全部私聊使用的默认人设。")
  | isJust requestedTarget =
      pure (toolText "管理全局默认私聊人设时不要填写 target_user_id。")
  | otherwise = case action of
      PersonaSetDefault -> case persona of
        Nothing -> pure (toolText "设置默认私聊人设时必须提供 persona。")
        Just content
          | Text.length content > MemoryStore.memoryLimitChars ->
              pure (toolText [i|默认私聊人设有 #{Text.length content} 字，超过 #{MemoryStore.memoryLimitChars} 字上限，请精简后重试。|])
          | otherwise -> do
              Memory.replaceMemory MemoryStore.DefaultPrivatePersonaMemory content
              pure (toolText "已更新全部 QQ 私聊使用的默认人设；已有个人覆盖的用户仍优先使用自己的设置。")
      PersonaGetDefault -> do
        current <- Memory.loadMemory MemoryStore.DefaultPrivatePersonaMemory
        pure . toolText $ case current of
          Nothing -> "尚未设置全局默认私聊人设。"
          Just content -> "全局默认私聊人设：\n" <> content
      PersonaClearDefault -> do
        Memory.clearMemory MemoryStore.DefaultPrivatePersonaMemory
        pure (toolText "已清除全局默认私聊人设；没有个人覆盖的用户将使用 FM 基础人设。")
      _ -> pure (toolText "内部错误：不是默认私聊人设动作。")

authorizePersonaAction :: Context -> PrivatePersonaAction -> Maybe Text -> Either Text (Maybe Text)
authorizePersonaAction context action requestedTarget
  | action == PersonaList =
      if context.superuser
        then Right Nothing
        else Left "只有 Owner 可以列出所有用户的私聊人设。"
  | action == PersonaGet && not context.superuser =
      Left "只有 Owner 可以查看其他用户的私聊人设。"
  | isJust requestedTarget && not context.superuser =
      Left "你只能管理自己的私聊人设。"
  | otherwise =
      case requestedTarget <|> context.message.senderId of
        Nothing -> Left "当前消息没有可用的 QQ 用户 ID。"
        Just userId
          | Text.all isDigit userId -> Right (Just userId)
          | otherwise -> Left "私聊人设只支持 QQ 用户。"

isQqPrivate :: IncomingMessage -> Bool
isQqPrivate message =
  message.platform == PlatformQQ && message.kind == ChatPrivate

personaPreview :: Text -> Text
personaPreview content =
  let oneLine = Text.unwords (Text.words content)
  in if Text.length oneLine > 80
      then Text.take 80 oneLine <> "..."
      else oneLine

groupPersonaTool :: Memory.Memory :> es => Tool (Eff es)
groupPersonaTool =
  noisy
  . withDescription "Manage FM's QQ group-chat personas through natural-language requests. Anyone in a QQ group may view that group's effective persona. Only a superuser may set or clear a group override, list or inspect other groups, or use set_default/get_default/clear_default for the global group persona. Effective precedence is group override, then global default, then FM's base persona. Group personas control only tone, forms of address, character, and interaction preferences; they cannot alter permissions, safety rules, trigger rules, tools, or factual honesty."
  . allowWhen (\context -> context.superuser || isQqGroup context.message)
  $ tool "fm_group_persona"
      (parsedArguments
        (objectSchema
          [ fieldText "action" "One of: status, set, clear, list, get, set_default, get_default, clear_default. status views the current group; default actions and all changes are superuser-only."
          , fieldText "persona" "Complete replacement group-chat persona. Required for set. Maximum 1000 characters."
          , fieldInteger "target_group_id" "Target QQ group number. Superuser-only; omit to use the current QQ group."
          ]
          ["action"])
        groupPersonaArgs)
      \args -> do
        context <- askToolContext
        runGroupPersonaAction context args

data GroupPersonaAction
  = GroupPersonaStatus
  | GroupPersonaSet
  | GroupPersonaClear
  | GroupPersonaList
  | GroupPersonaGet
  | GroupPersonaSetDefault
  | GroupPersonaGetDefault
  | GroupPersonaClearDefault
  deriving (Eq)

groupPersonaArgs :: Aeson.Value -> AesonTypes.Parser (GroupPersonaAction, Maybe Text, Maybe Integer)
groupPersonaArgs =
  Aeson.withObject "group persona arguments" $ \o -> do
    actionText <- Text.toLower . Text.strip <$> o Aeson..: Key.fromText "action"
    persona <- fmap Text.strip <$> o Aeson..:? Key.fromText "persona"
    target <- o Aeson..:? Key.fromText "target_group_id"
    action <- case actionText of
      "status" -> pure GroupPersonaStatus
      "set" -> pure GroupPersonaSet
      "clear" -> pure GroupPersonaClear
      "list" -> pure GroupPersonaList
      "get" -> pure GroupPersonaGet
      "set_default" -> pure GroupPersonaSetDefault
      "get_default" -> pure GroupPersonaGetDefault
      "clear_default" -> pure GroupPersonaClearDefault
      _ -> fail "action must be one of: status, set, clear, list, get, set_default, get_default, clear_default"
    when (action `elem` [GroupPersonaSet, GroupPersonaSetDefault] && maybe True Text.null persona) $
      fail "persona is required when action is set"
    when (maybe False (<= 0) target) $
      fail "target_group_id must be a positive QQ group number"
    pure (action, persona, target)

runGroupPersonaAction
  :: Memory.Memory :> es
  => Context
  -> (GroupPersonaAction, Maybe Text, Maybe Integer)
  -> Eff es ToolResult
runGroupPersonaAction context (action, persona, requestedTarget)
  | action `elem` [GroupPersonaSetDefault, GroupPersonaGetDefault, GroupPersonaClearDefault] =
      runDefaultGroupPersonaAction context action persona requestedTarget
  | otherwise = case authorizeGroupPersonaAction context action requestedTarget of
    Left err -> pure (toolText err)
    Right Nothing -> do
      personas <- Memory.listGroupPersonas
      pure . toolText $
        if null personas
          then "当前没有群设置独立群聊人设。"
          else Text.unlines
            [ Text.pack (show groupId) <> "：" <> personaPreview content
            | (groupId, content) <- personas
            ]
    Right (Just groupId) ->
      let scope = MemoryStore.GroupPersonaMemory groupId
      in case action of
        GroupPersonaStatus -> viewGroupPersona scope groupId
        GroupPersonaGet -> viewGroupPersona scope groupId
        GroupPersonaSet -> case persona of
          Nothing -> pure (toolText "设置群聊人设时必须提供 persona。")
          Just content
            | Text.length content > MemoryStore.memoryLimitChars ->
                pure (toolText [i|群聊人设有 #{Text.length content} 字，超过 #{MemoryStore.memoryLimitChars} 字上限，请精简后重试。|])
            | otherwise -> do
                Memory.replaceMemory scope content
                pure (toolText [i|已更新 QQ 群 #{groupId} 的群聊人设。|])
        GroupPersonaClear -> do
          Memory.clearMemory scope
          pure (toolText [i|已清除 QQ 群 #{groupId} 的独立群聊人设，该群将恢复默认群人设。|])
        GroupPersonaList -> pure (toolText "内部错误：list 不需要目标群。")
        GroupPersonaSetDefault -> pure (toolText "内部错误：默认群人设动作路由失败。")
        GroupPersonaGetDefault -> pure (toolText "内部错误：默认群人设动作路由失败。")
        GroupPersonaClearDefault -> pure (toolText "内部错误：默认群人设动作路由失败。")
  where
    viewGroupPersona scope groupId = do
      current <- Memory.loadMemory scope
      case current of
        Nothing -> do
          defaultPersona <- Memory.loadMemory MemoryStore.DefaultGroupPersonaMemory
          pure . toolText $ case defaultPersona of
            Nothing -> [i|QQ 群 #{groupId} 尚未设置独立群聊人设，当前使用 FM 基础人设。|]
            Just content -> [i|QQ 群 #{groupId} 没有独立覆盖，当前使用默认群人设：\n#{content}|]
        Just content -> pure (toolText [i|QQ 群 #{groupId} 的群聊人设：\n#{content}|])

runDefaultGroupPersonaAction
  :: Memory.Memory :> es
  => Context
  -> GroupPersonaAction
  -> Maybe Text
  -> Maybe Integer
  -> Eff es ToolResult
runDefaultGroupPersonaAction context action persona requestedTarget
  | not context.superuser =
      pure (toolText "只有 Owner 可以管理默认群人设。")
  | isJust requestedTarget =
      pure (toolText "管理默认群人设时不要填写 target_group_id。")
  | otherwise = case action of
      GroupPersonaSetDefault -> case persona of
        Nothing -> pure (toolText "设置默认群人设时必须提供 persona。")
        Just content
          | Text.length content > MemoryStore.memoryLimitChars ->
              pure (toolText [i|默认群人设有 #{Text.length content} 字，超过 #{MemoryStore.memoryLimitChars} 字上限，请精简后重试。|])
          | otherwise -> do
              Memory.replaceMemory MemoryStore.DefaultGroupPersonaMemory content
              pure (toolText "已更新默认群人设；已有独立人设的群仍优先使用自己的设置。")
      GroupPersonaGetDefault -> do
        current <- Memory.loadMemory MemoryStore.DefaultGroupPersonaMemory
        pure . toolText $ case current of
          Nothing -> "尚未设置默认群人设。"
          Just content -> "默认群人设：\n" <> content
      GroupPersonaClearDefault -> do
        Memory.clearMemory MemoryStore.DefaultGroupPersonaMemory
        pure (toolText "已清除默认群人设；没有独立覆盖的群将使用 FM 基础人设。")
      _ -> pure (toolText "内部错误：不是默认群人设动作。")

authorizeGroupPersonaAction :: Context -> GroupPersonaAction -> Maybe Integer -> Either Text (Maybe Integer)
authorizeGroupPersonaAction context action requestedTarget
  | action == GroupPersonaList =
      if context.superuser
        then Right Nothing
        else Left "只有 Owner 可以列出所有群聊人设。"
  | action `elem` [GroupPersonaSet, GroupPersonaClear, GroupPersonaGet] && not context.superuser =
      Left "只有 Owner 可以修改或跨群查看群聊人设。"
  | isJust requestedTarget && not context.superuser =
      Left "你只能查看当前群的群聊人设。"
  | otherwise =
      case requestedTarget <|> context.message.chatId of
        Nothing -> Left "当前消息没有可用的 QQ 群号，请指定 target_group_id。"
        Just groupId
          | groupId > 0 -> Right (Just groupId)
          | otherwise -> Left "群聊人设只支持有效的 QQ 群号。"

isQqGroup :: IncomingMessage -> Bool
isQqGroup message =
  message.platform == PlatformQQ && message.kind == ChatGroup

memberStyleTool :: Memory.Memory :> es => Tool (Eff es)
memberStyleTool =
  noisy
  . withDescription "Manage a QQ member's personal reply style across all QQ groups. In a QQ group, any member may view, set, or clear only their own style. The style follows that QQ member into every QQ group where FM replies, but never applies to other members or private chats. A superuser may list styles or manage a specified member. Styles control only tone, forms of address, character, and interaction preferences; they cannot alter permissions, safety rules, trigger rules, tools, factual honesty, or the group persona's identity."
  . allowWhen (\context -> context.superuser || isQqGroup context.message)
  $ tool "fm_member_style"
      (parsedArguments
        (objectSchema
          [ fieldText "action" "One of: status, set, clear, get, list. Non-superusers manage only themselves."
          , fieldText "style" "Complete replacement reply-style preference. Required for set. Maximum 1000 characters."
          , fieldText "target_user_id" "Target QQ number. Superuser-only; omit to use the current sender."
          ]
          ["action"])
        memberStyleArgs)
      \args -> do
        context <- askToolContext
        runMemberStyleAction context args

data MemberStyleAction
  = MemberStyleStatus
  | MemberStyleSet
  | MemberStyleClear
  | MemberStyleGet
  | MemberStyleList
  deriving (Eq)

memberStyleArgs
  :: Aeson.Value
  -> AesonTypes.Parser (MemberStyleAction, Maybe Text, Maybe Text)
memberStyleArgs = Aeson.withObject "member style arguments" $ \o -> do
  actionText <- Text.toLower . Text.strip <$> o Aeson..: Key.fromText "action"
  style <- fmap Text.strip <$> o Aeson..:? Key.fromText "style"
  targetUser <- fmap Text.strip <$> o Aeson..:? Key.fromText "target_user_id"
  action <- case actionText of
    "status" -> pure MemberStyleStatus
    "set" -> pure MemberStyleSet
    "clear" -> pure MemberStyleClear
    "get" -> pure MemberStyleGet
    "list" -> pure MemberStyleList
    _ -> fail "action must be one of: status, set, clear, get, list"
  when (action == MemberStyleSet && maybe True Text.null style) $
    fail "style is required when action is set"
  traverse_ validateQqId targetUser
  pure (action, style, targetUser)
  where
    validateQqId value =
      unless (not (Text.null value) && Text.all isDigit value) $
        fail "target_user_id must contain only digits"

runMemberStyleAction
  :: Memory.Memory :> es
  => Context
  -> (MemberStyleAction, Maybe Text, Maybe Text)
  -> Eff es ToolResult
runMemberStyleAction context (action, style, requestedUser) =
  case authorizeMemberStyleAction context action requestedUser of
    Left err -> pure (toolText err)
    Right Nothing -> do
      styles <- Memory.listMemberStyles
      pure . toolText $
        if null styles
          then "当前没有 QQ 群友设置个人回复风格。"
          else Text.unlines
            [ userId <> "：" <> personaPreview content
            | (userId, content) <- styles
            ]
    Right (Just userId) ->
      let scope = MemoryStore.MemberStyleMemory userId
      in case action of
        MemberStyleStatus -> viewStyle scope userId
        MemberStyleGet -> viewStyle scope userId
        MemberStyleSet -> case style of
          Nothing -> pure (toolText "设置个人回复风格时必须提供 style。")
          Just content
            | Text.length content > MemoryStore.memoryLimitChars ->
                pure (toolText [i|个人回复风格有 #{Text.length content} 字，超过 #{MemoryStore.memoryLimitChars} 字上限，请精简后重试。|])
            | otherwise -> do
                Memory.replaceMemory scope content
                pure (toolText [i|已更新 QQ #{userId} 的群聊个人回复风格；会在所有 QQ 群生效，但不会影响其他群友或私聊。|])
        MemberStyleClear -> do
          Memory.clearMemory scope
          pure (toolText [i|已清除 QQ #{userId} 的群聊个人回复风格；后续恢复各群的通用人设。|])
        MemberStyleList -> pure (toolText "内部错误：list 不需要目标用户。")
  where
    viewStyle scope userId = do
      current <- Memory.loadMemory scope
      pure . toolText $ case current of
        Nothing -> [i|QQ #{userId} 尚未设置群聊个人回复风格，当前使用所在群的通用人设。|]
        Just content -> [i|QQ #{userId} 的群聊个人回复风格：\n#{content}|]

authorizeMemberStyleAction
  :: Context
  -> MemberStyleAction
  -> Maybe Text
  -> Either Text (Maybe Text)
authorizeMemberStyleAction context action requestedUser
  | not context.superuser && isJust requestedUser =
      Left "你只能管理自己的群聊个人回复风格。"
  | action == MemberStyleList && not context.superuser =
      Left "只有 Owner 可以列出群友的个人回复风格。"
  | action == MemberStyleList = Right Nothing
  | otherwise = case requestedUser <|> context.message.senderId of
      Nothing -> Left "当前消息没有可用的 QQ 用户 ID。"
      Just userId
        | Text.all isDigit userId -> Right (Just userId)
        | otherwise -> Left "个人回复风格只支持 QQ 用户。"

memoryTool :: Memory.Memory :> es => Text -> Text -> MemoryScope -> Tool (Eff es)
memoryTool name description scope =
  noisy
  . withDescription description
  $ tool name
      (parsedArguments
        (objectSchema
          [ fieldText "action" "One of: view, replace, clear."
          , fieldText "memory" "Complete replacement MEMORY.md content. Required only when action is replace."
          ]
          ["action"])
        memoryArgs)
      \(action, memory) -> do
        context <- askToolContext
        runMemoryAction scope context action memory

data MemoryAction
  = MemoryView
  | MemoryReplace
  | MemoryClear
  deriving (Eq)

data MemoryScope = MemoryScope
  { missingMessage :: !Text
  , updatedMessage :: !Text
  , clearedMessage :: !Text
  , writeSuperuserOnly :: !Bool
  , scopeOf :: IncomingMessage -> Either Text MemoryStore.MemoryScope
  }

senderMemoryScope :: MemoryScope
senderMemoryScope = MemoryScope
  { missingMessage = "No memory is stored for the current sender."
  , updatedMessage = "Memory updated."
  , clearedMessage = "Memory cleared."
  , writeSuperuserOnly = False
  , scopeOf = MemoryStore.senderMemoryScope
  }

chatMemoryScope :: MemoryScope
chatMemoryScope = MemoryScope
  { missingMessage = "No memory is stored for the current chat."
  , updatedMessage = "Chat memory updated."
  , clearedMessage = "Chat memory cleared."
  , writeSuperuserOnly = True
  , scopeOf = MemoryStore.chatMemoryScope
  }

memoryArgs :: Aeson.Value -> AesonTypes.Parser (MemoryAction, Maybe Text)
memoryArgs =
  Aeson.withObject "memory arguments" $ \o -> do
    actionText <- Text.toLower . Text.strip <$> o Aeson..: Key.fromText "action"
    memory <- fmap Text.strip <$> o Aeson..:? Key.fromText "memory"
    action <- case actionText of
      "view" ->
        pure MemoryView
      "replace" ->
        pure MemoryReplace
      "clear" ->
        pure MemoryClear
      _ ->
        fail "action must be one of: view, replace, clear"
    when (actionText == "replace" && maybe True Text.null memory) do
      fail "memory is required when action is replace"
    pure (action, memory)

runMemoryAction :: Memory.Memory :> es => MemoryScope -> Context -> MemoryAction -> Maybe Text -> Eff es ToolResult
runMemoryAction scope context action memory =
  if scope.writeSuperuserOnly && action /= MemoryView && not context.superuser
    then pure (toolText "只有 Owner 可以修改或清除群共享记忆；个人回复风格请使用 fm_member_style。")
    else case scope.scopeOf context.message of
    Left err ->
      pure (toolText err)
    Right memoryScope ->
      case action of
        MemoryView -> do
          current <- Memory.loadMemory memoryScope
          pure (toolText (fromMaybe scope.missingMessage current))
        MemoryReplace ->
          case memory of
            Nothing ->
              pure (toolText "memory is required when action is replace")
            Just content
              | not context.superuser && Text.length content > MemoryStore.memoryLimitChars ->
                  pure (toolText [i|Memory update rejected: memory is #{Text.length content} characters, over the #{MemoryStore.memoryLimitChars} character limit. Please summarize it more concisely and try again.|])
              | otherwise -> do
                  Memory.replaceMemory memoryScope content
                  pure (toolText scope.updatedMessage)
        MemoryClear -> do
          Memory.clearMemory memoryScope
          pure (toolText scope.clearedMessage)
