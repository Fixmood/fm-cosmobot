{-|
Module      : Bot.Agent.Tools.Model
Description : Runtime chat-model selection tools
Stability   : experimental
-}
module Bot.Agent.Tools.Model
  ( chatModelStatusTool
  , accountBalanceTool
  , chatModelManageTool
  , chatModelAddTool
  , chatModelEditTool
  , chatModelDeleteTool
  , chatModelSwitchTool
  , chatModelResetTool
  )
where

import Bot.Agent.Tool
import Bot.Agent.Tools.Common
import Bot.Agent.Types
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Lifecycle as Lifecycle
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Aeson.Key as AesonKey
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text
import Network.HTTP.Req

chatModelStatusTool :: LLM.LLM :> es => Tool (Eff es)
chatModelStatusTool =
  withDescription "Show FM's current chat model and every configured model profile. Use this for natural requests about available models, the active model, vision models, or before switching when the user's wording is ambiguous."
  $ tool "chat_model_status" noArguments do
      models <- LLM.listChatModels
      pure . toolText $ renderModels models

data ChatModelManageArgs = ChatModelManageArgs
  { manageAction :: !Text
  , manageTarget :: !(Maybe Text)
  , manageName :: !(Maybe Text)
  , manageBaseUrl :: !(Maybe Text)
  , manageApiKey :: !(Maybe Text)
  , manageModel :: !(Maybe Text)
  , manageReasoning :: !(Maybe Text)
  , manageTimeout :: !(Maybe Int)
  }

chatModelManageTool
  :: (LLM.LLM :> es, Lifecycle.Lifecycle :> es, Chat.Chat :> es)
  => Tool (Eff es)
chatModelManageTool =
  allowWhen superuserOnly
  . withDescription "Unified model management. action can be status, add, edit, delete, switch, or reset. Use natural language to decide the action. For add/edit, provide only the fields the user gave; missing required fields should be requested. API keys must never be echoed. Add/edit always test the endpoint before saving; switch also tests before changing the active model."
  $ tool "chat_model_manage"
      (parsedArguments manageSchema parseManageArgs)
      runManage
  where
    runManage args = do
      context <- askToolContext
      case Text.toCaseFold (Text.strip args.manageAction) of
        "status" -> chatModelStatusToolRunner
        "查看" -> chatModelStatusToolRunner
        "list" -> chatModelStatusToolRunner
        "add" -> runAdd context args
        "添加" -> runAdd context args
        "edit" -> runEdit context args
        "修改" -> runEdit context args
        "改名" -> runEdit context args
        "delete" -> runDelete context args
        "删除" -> runDelete context args
        "switch" -> runSwitch args
        "切换" -> runSwitch args
        "reset" -> runReset
        "恢复默认" -> runReset
        action -> pure (toolFailure (permanentArgumentFailure ("不支持的模型管理操作：" <> action) "请使用查看、添加、修改、删除、切换或恢复默认。"))
      where
        chatModelStatusToolRunner = LLM.listChatModels <&> toolText . renderModels
        runSwitch args = do
          target <- requireTarget args
          case target of
            Left result -> pure result
            Right targetName ->
              LLM.probeChatModel targetName >>= \case
                Left err -> pure (toolFailure (permanentArgumentFailure ("模型测试失败，未切换：" <> err) err))
                Right () -> LLM.selectChatModel targetName <&> \case
                  Left err -> toolFailure (permanentArgumentFailure err err)
                  Right selected -> toolText ("模型测试通过，已切换到 " <> renderModel selected <> ".")
        runReset = LLM.resetChatModel <&> \case
          Nothing -> toolFailure (permanentArgumentFailure "没有配置默认模型。" "没有配置默认模型。")
          Just selected -> toolText ("已恢复默认模型：" <> renderModel selected <> ".")
        runAdd context args = do
          missing <- requireFields [ ("名称", args.manageName), ("API 地址", args.manageBaseUrl), ("API Key", args.manageApiKey), ("模型 ID", args.manageModel) ]
          case missing of
            Just err -> pure (toolFailure (permanentArgumentFailure err err))
            Nothing -> do
              result <- LLM.addChatModel LLM.ChatModelConfig
                { profileName = fromMaybe "" args.manageName
                , baseUrl = fromMaybe "" args.manageBaseUrl
                , apiKey = fromMaybe "" args.manageApiKey
                , model = fromMaybe "" args.manageModel
                , reasoningEffort = fromMaybe "low" args.manageReasoning
                , requestTimeout = fromMaybe 120 args.manageTimeout
                }
              restartAfter context result "模型配置已测试通过并添加，FM正在重启加载模型列表。当前模型不会自动改变。"
        runEdit context args = do
          target <- requireTarget args
          case target of
            Left result -> pure result
            Right targetName -> do
              result <- LLM.editChatModel targetName LLM.ChatModelPatch
                { newProfileName = args.manageName
                , newBaseUrl = args.manageBaseUrl
                , newApiKey = args.manageApiKey
                , newModel = args.manageModel
                , newReasoningEffort = args.manageReasoning
                , newRequestTimeout = args.manageTimeout
                }
              restartAfter context result "模型配置已测试通过并修改，FM正在重启加载配置。当前模型不会自动改变。"
        runDelete context args = do
          target <- requireTarget args
          case target of
            Left result -> pure result
            Right targetName -> do
              result <- LLM.deleteChatModel targetName
              restartAfter context result "模型配置已删除，FM正在重启加载模型列表。"
        restartAfter context result message =
          case result of
            Left err -> pure (toolFailure (permanentArgumentFailure ("操作失败：" <> err) err))
            Right () -> do
              void (Chat.replyTo context.message message)
              Lifecycle.requestRestart context.message ""
              pure (toolText "操作已完成。")
        requireTarget args =
          case Text.strip <$> args.manageTarget of
            Just value | not (Text.null value) -> pure (Right value)
            _ -> pure (Left (toolFailure (permanentArgumentFailure "缺少目标模型。" "请提供模型名称或模型 ID。")))

requireFields :: [(Text, Maybe Text)] -> Eff es (Maybe Text)
requireFields fields =
  pure $ case [name | (name, Just value) <- fields, Text.null (Text.strip value)] <> [name | (name, Nothing) <- fields] of
    [] -> Nothing
    missing -> Just ("缺少必要配置：" <> Text.intercalate "、" missing)

manageSchema :: Aeson.Value
manageSchema = Aeson.object
  [ "type" Aeson..= ("object" :: Text)
  , "properties" Aeson..= Aeson.object
      [ "action" Aeson..= stringProperty
      , "target" Aeson..= stringProperty
      , "name" Aeson..= stringProperty
      , "base_url" Aeson..= stringProperty
      , "api_key" Aeson..= stringProperty
      , "model" Aeson..= stringProperty
      , "reasoning_effort" Aeson..= stringProperty
      , "timeout" Aeson..= integerProperty
      ]
  , "required" Aeson..= (["action"] :: [Text])
  , "additionalProperties" Aeson..= False
  ]
  where
    stringProperty = Aeson.object ["type" Aeson..= ("string" :: Text)]
    integerProperty = Aeson.object ["type" Aeson..= ("integer" :: Text)]

parseManageArgs :: Aeson.Value -> AesonTypes.Parser ChatModelManageArgs
parseManageArgs = Aeson.withObject "chat_model_manage arguments" \object ->
  ChatModelManageArgs
    <$> object Aeson..: AesonKey.fromText "action"
    <*> object Aeson..:? AesonKey.fromText "target"
    <*> object Aeson..:? AesonKey.fromText "name"
    <*> object Aeson..:? AesonKey.fromText "base_url"
    <*> object Aeson..:? AesonKey.fromText "api_key"
    <*> object Aeson..:? AesonKey.fromText "model"
    <*> object Aeson..:? AesonKey.fromText "reasoning_effort"
    <*> object Aeson..:? AesonKey.fromText "timeout"

accountBalanceTool :: (LLM.LLM :> es, HTTP.HTTP :> es, IOE :> es) => Tool (Eff es)
accountBalanceTool =
  allowWhen superuserOnly
  . withDescription "Query one or all of FM's configured API balances and quotas. Supported targets are all, DeepSeek, WeiLai, BotCF, and Tavily. Use this for natural requests about remaining credit, balance, or Tavily search quota. Omit target to query all providers. Never expose API keys, authorization headers, or private endpoint details."
  $ tool "account_balance"
      (optionalText "target" "Optional provider: all, deepseek, weilai, botcf, or tavily. Omit to query all providers.")
      \rawTarget ->
        case normalizeBalanceTarget rawTarget of
          Left err ->
            pure (toolFailure (permanentArgumentFailure err err))
          Right target -> do
            context <- askToolContext
            providerResults <-
              if target == BalanceTavily
                then pure []
                else LLM.queryAccountBalance (llmBalanceTarget target)
            tavilyResults <-
              if target `elem` [BalanceAll, BalanceTavily]
                then (: []) <$> queryTavilyBalance context.toolConfig.tavilyApiKey
                else pure []
            pure . toolText . renderBalanceResults $ providerResults <> tavilyResults

chatModelSwitchTool :: LLM.LLM :> es => Tool (Eff es)
chatModelSwitchTool =
  allowWhen superuserOnly
  . withDescription "Safely switch FM's global chat model profile. Before saving the switch, probe the target provider with a minimal request; if DNS, API key, endpoint, or model ID is invalid, keep the current model unchanged and report the failure. Use this for natural-language requests such as switching to a vision-capable model or DeepSeek Pro. Only do this when the superuser clearly asks."
  $ tool "chat_model_switch"
      (requiredText "target" "Configured profile name or exact model id returned by chat_model_status.")
      \target -> do
        let cleanTarget = Text.strip target
        LLM.probeChatModel cleanTarget >>= \case
          Left err ->
            pure (toolFailure (permanentArgumentFailure ("模型测试失败，未切换：" <> err) err))
          Right () ->
            LLM.selectChatModel cleanTarget <&> \case
              Left err -> toolFailure (permanentArgumentFailure err err)
              Right selected -> toolText ("模型测试通过，已切换到 " <> renderModel selected <> ".")

chatModelAddTool
  :: (LLM.LLM :> es, Lifecycle.Lifecycle :> es)
  => Tool (Eff es)
chatModelAddTool =
  allowWhen superuserOnly
  . withDescription "Add a new OpenAI-compatible chat model profile from natural-language configuration. Required: profile name, API base URL, API key, and model ID. Optional: reasoning effort and timeout. The endpoint is probed before anything is saved; on success the configuration is persisted and FM restarts to load it. Never echo the API key. Do not switch to the new model unless the user separately asks to switch."
  $ tool "chat_model_add"
      ( requiredText "name" "A unique profile name, using letters, numbers, hyphens, or underscores."
      , requiredText "base_url" "OpenAI-compatible API base URL, including the correct /v1 path when required."
      , requiredText "api_key" "API key for this provider. Never display it in the response."
      , requiredText "model" "Exact model ID accepted by the provider."
      , withDefault "low" (optionalText "reasoning_effort" "Optional reasoning effort, defaults to low.")
      , withDefault 120 (optionalInt "timeout" "Optional request timeout in seconds, defaults to 120.")
      )
      \name baseUrl apiKey model reasoningEffort timeout -> do
        context <- askToolContext
        result <- LLM.addChatModel LLM.ChatModelConfig
          { profileName = Text.strip name
          , baseUrl = Text.strip baseUrl
          , apiKey = Text.strip apiKey
          , model = Text.strip model
          , reasoningEffort = Text.strip reasoningEffort
          , requestTimeout = timeout
          }
        case result of
          Left err ->
            pure (toolFailure (permanentArgumentFailure ("模型测试失败，未保存：" <> err) err))
          Right () -> do
            Lifecycle.requestRestart context.message "模型配置已测试通过并保存，FM正在重启加载新模型列表。当前模型不会自动改变。"
            pure (toolText "模型配置已测试通过并保存，FM正在重启加载新模型列表。当前模型不会自动改变。")

chatModelEditTool
  :: (LLM.LLM :> es, Lifecycle.Lifecycle :> es)
  => Tool (Eff es)
chatModelEditTool =
  allowWhen superuserOnly
  . withDescription "Edit an existing OpenAI-compatible chat model profile. The target is required; every other field is optional and remains unchanged when omitted. The updated endpoint is probed before saving. Never echo API keys."
  $ tool "chat_model_edit"
      ( requiredText "target" "Existing profile name or exact model id."
      , optionalText "name" "Optional new profile name."
      , optionalText "base_url" "Optional new API base URL."
      , optionalText "api_key" "Optional replacement API key; omit to keep the existing key."
      , optionalText "model" "Optional replacement model ID."
      , optionalText "reasoning_effort" "Optional replacement reasoning effort."
      , optionalInt "timeout" "Optional replacement timeout in seconds."
      )
      \target name baseUrl apiKey model reasoningEffort timeout -> do
        context <- askToolContext
        result <- LLM.editChatModel (Text.strip target) LLM.ChatModelPatch
          { newProfileName = name
          , newBaseUrl = baseUrl
          , newApiKey = apiKey
          , newModel = model
          , newReasoningEffort = reasoningEffort
          , newRequestTimeout = timeout
          }
        case result of
          Left err -> pure (toolFailure (permanentArgumentFailure ("模型测试失败，修改未保存：" <> err) err))
          Right () -> do
            Lifecycle.requestRestart context.message "模型配置已测试通过并修改，FM正在重启加载配置。当前模型不会自动改变。"
            pure (toolText "模型配置已测试通过并修改，FM正在重启加载配置。当前模型不会自动改变。")

chatModelDeleteTool
  :: (LLM.LLM :> es, Lifecycle.Lifecycle :> es)
  => Tool (Eff es)
chatModelDeleteTool =
  allowWhen superuserOnly
  . withDescription "Delete an existing chat model profile by profile name or exact model id. Refuse to delete the currently selected/default profile. Ask for confirmation in natural language before calling this tool when the user is only asking whether deletion is possible."
  $ tool "chat_model_delete"
      (requiredText "target" "Profile name or exact model id to delete.")
      \target -> do
        context <- askToolContext
        result <- LLM.deleteChatModel (Text.strip target)
        case result of
          Left err -> pure (toolFailure (permanentArgumentFailure ("模型配置未删除：" <> err) err))
          Right () -> do
            Lifecycle.requestRestart context.message "模型配置已删除，FM正在重启加载模型列表。"
            pure (toolText "模型配置已删除，FM正在重启加载模型列表。")

chatModelResetTool :: LLM.LLM :> es => Tool (Eff es)
chatModelResetTool =
  allowWhen superuserOnly
  . withDescription "Restore FM's global chat model to the default profile from config.toml. Use only when the superuser asks to restore or reset the model."
  $ tool "chat_model_reset" noArguments do
      LLM.resetChatModel <&> \case
        Nothing -> toolFailure (permanentArgumentFailure "No default chat model is configured." "No default chat model is configured.")
        Just selected -> toolText ("Restored the default chat model: " <> renderModel selected <> ".")

renderModels :: [LLM.ChatModelInfo] -> Text
renderModels [] =
  "No chat model profiles are configured."
renderModels models =
  Text.unlines
    ( "Configured chat model profiles:"
    : [ "- " <> renderModel modelInfo
          <> if modelInfo.current then " [current]" else ""
          <> if modelInfo.configuredDefault then " [default]" else ""
      | modelInfo <- models
      ]
    )

renderModel :: LLM.ChatModelInfo -> Text
renderModel modelInfo =
  modelInfo.provider <> " (" <> modelInfo.model <> ")"

data BalanceTarget
  = BalanceAll
  | BalanceDeepSeek
  | BalanceWeilai
  | BalanceBotcf
  | BalanceTavily
  deriving (Eq)

normalizeBalanceTarget :: Maybe Text -> Either Text BalanceTarget
normalizeBalanceTarget Nothing = Right BalanceAll
normalizeBalanceTarget (Just raw) =
  case Text.toCaseFold (Text.strip raw) of
    "" -> Right BalanceAll
    "all" -> Right BalanceAll
    "全部" -> Right BalanceAll
    "所有" -> Right BalanceAll
    "所有api" -> Right BalanceAll
    "deepseek" -> Right BalanceDeepSeek
    "深度求索" -> Right BalanceDeepSeek
    "weilai" -> Right BalanceWeilai
    "weilai.uk" -> Right BalanceWeilai
    "未来" -> Right BalanceWeilai
    "botcf" -> Right BalanceBotcf
    "botcf.com" -> Right BalanceBotcf
    "tavily" -> Right BalanceTavily
    "搜索" -> Right BalanceTavily
    target -> Left ("不支持的余额渠道：" <> target <> "。可查询全部、DeepSeek、WeiLai、BotCF 或 Tavily。")

llmBalanceTarget :: BalanceTarget -> Maybe Text
llmBalanceTarget = \case
  BalanceAll -> Nothing
  BalanceDeepSeek -> Just "deepseek"
  BalanceWeilai -> Just "weilai"
  BalanceBotcf -> Just "botcf"
  BalanceTavily -> Just "tavily"

queryTavilyBalance
  :: (HTTP.HTTP :> es, IOE :> es)
  => Maybe Text
  -> Eff es LLM.AccountBalanceResult
queryTavilyBalance maybeApiKey =
  case Text.strip <$> maybeApiKey of
    Nothing -> pure (balanceResultFailure "Tavily" "没有配置 Tavily API Key。")
    Just "" -> pure (balanceResultFailure "Tavily" "没有配置 Tavily API Key。")
    Just apiKey -> do
      result <- trySync $ responseBody <$> HTTP.runReq
        (req GET
          (https "api.tavily.com" /: "usage")
          NoReqBody
          jsonResponse
          (header "Authorization" (ByteString.pack [i|Bearer #{apiKey}|])))
      pure $ case result of
        Left (_ :: SomeException) ->
          balanceResultFailure "Tavily" "额度接口暂时请求失败。"
        Right value ->
          LLM.AccountBalanceResult
            { sourceName = "Tavily"
            , queryResult = parseTavilyUsage value
            }

parseTavilyUsage :: Aeson.Value -> Either Text LLM.AccountBalance
parseTavilyUsage value =
  first (const "Tavily 额度接口返回了无法识别的数据。") $
    AesonTypes.parseEither parser value
  where
    parser = Aeson.withObject "Tavily usage" \object -> do
      account <- object Aeson..: "account"
      usage <- account Aeson..: "plan_usage" :: AesonTypes.Parser Int
      limit <- account Aeson..: "plan_limit" :: AesonTypes.Parser Int
      plan <- account Aeson..:? "current_plan" Aeson..!= "未知套餐"
      pure LLM.AccountBalance
        { available = limit > usage
        , fields =
            [ ("套餐", plan)
            , ("已使用", showText usage <> " 次")
            , ("总额度", showText limit <> " 次")
            , ("剩余", showText (max 0 (limit - usage)) <> " 次")
            ]
        }

balanceResultFailure :: Text -> Text -> LLM.AccountBalanceResult
balanceResultFailure source err =
  LLM.AccountBalanceResult{sourceName = source, queryResult = Left err}

renderBalanceResults :: [LLM.AccountBalanceResult] -> Text
renderBalanceResults [] =
  "没有找到可查询的 API 渠道。"
renderBalanceResults results =
  Text.intercalate "\n\n" (map renderResult results)
  where
    renderResult result =
      "【" <> result.sourceName <> "】\n" <>
        case result.queryResult of
          Left err -> "- 查询失败：" <> err
          Right balance ->
            Text.unlines $
              (if balance.available then "- 状态：可用" else "- 状态：不可用或额度已用尽")
                : case balance.fields of
                    [] -> ["- 接口没有返回明细"]
                    fields -> ["- " <> label <> "：" <> value | (label, value) <- fields]

showText :: Show a => a -> Text
showText = Text.pack . show
