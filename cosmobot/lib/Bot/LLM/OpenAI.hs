{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.LLM.OpenAI
Description : OpenAI-compatible LLM interpreter
Stability   : experimental
-}


module Bot.LLM.OpenAI
  ( runLLM
  , resolveChatModelTarget
  )
where

import Bot.Prelude
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import Bot.LLM.OpenAI.Config
import qualified Bot.LLM.OpenAI.Retry as Retry
import qualified Bot.LLM.OpenAI.Transport as Transport
import Bot.LLM.Types
import Control.Monad.Trans.Resource (ResourceT)
import qualified Data.Text as Text
import Data.Char (isAlphaNum)
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.Types as AesonTypes
import Network.HTTP.Req
import qualified Streaming.ByteString as Q
import qualified Streaming
import qualified Streaming.Prelude as S
import qualified Effectful.Concurrent.MVar as MVar
import Effectful.FileSystem
import Effectful.Process
import Effectful.Timeout
import GHC.Clock (getMonotonicTimeNSec)
import qualified System.Directory as Directory
import System.FilePath (takeDirectory)
import System.IO.Error (catchIOError)

-- | Interpret LLM requests through an OpenAI-compatible HTTP endpoint.
runLLM
  :: ( Concurrent :> es
     , Fail :> es
     , Timeout :> es
     , FileSystem :> es
     , Process :> es
     , HTTP.HTTP :> es
     , Media.Media :> es
     , IOE :> es)
  => KatipE :> es
  => Config
  -> Eff (LLM.LLM : es) a
  -> Eff es a
runLLM cfg action = do
  initialSelection <- liftIO (loadChatModelSelection cfg)
  selection <- MVar.newMVar initialSelection
  interpret
    (\localEnv operation ->
      case operation of
        LLM.AskStream messages -> do
          selected <- MVar.readMVar selection
          localSeqLift localEnv \liftLocal ->
            pure $
              LLM.liftLocalStream liftLocal $
                do
                  resolved <- lift (resolveChatMessagesTimed messages)
                  let requestCfg = requestChatConfig cfg selected resolved
                  lift (logAutomaticVisionRoute cfg selected requestCfg)
                  Retry.retryLLMStreamRequest "LLM streaming request" $
                    normalizeReplyResult (Transport.askOpenAIStreaming requestCfg resolved)
        LLM.AskImageStream options messages ->
          localSeqLift localEnv \liftLocal ->
            pure $
              LLM.liftLocalStream liftLocal $
                do
                  resolved <- lift (resolveChatMessagesTimed messages)
                  imageStreamWithFallback cfg options resolved
        LLM.AskImageEditStream options prompt imageRefs maskRef ->
          localSeqLift localEnv \liftLocal ->
            pure $
              LLM.liftLocalStream liftLocal $
                do
                  resolvedImageRefs <- lift (traverse Media.publicMediaRef imageRefs)
                  resolvedMaskRef <- lift (traverse Media.publicMediaRef maskRef)
                  Retry.retryLLMStreamRequest "LLM image edit streaming request" $
                    askImageEditStreamingWithMedia cfg options prompt resolvedImageRefs resolvedMaskRef
        LLM.AskAudioStream options messages ->
          localSeqLift localEnv \liftLocal ->
            pure $
              LLM.liftLocalStream liftLocal $
                do
                  resolved <- lift (resolveChatMessagesTimed messages)
                  Retry.retryLLMStreamRequest "LLM audio streaming request" $
                    normalizeReplyResult (Transport.askAudioOpenAIStreaming cfg options resolved)
        LLM.AskToolsStream tools messages -> do
          selected <- MVar.readMVar selection
          localSeqLift localEnv \liftLocal ->
            pure $
              LLM.liftLocalStream liftLocal $
                do
                  resolved <- lift (resolveChatMessagesTimed messages)
                  let requestCfg = requestChatConfig cfg selected resolved
                  lift (logAutomaticVisionRoute cfg selected requestCfg)
                  Retry.retryLLMStreamRequest "LLM streaming request" $
                    Transport.askOpenAIWithToolsStreaming requestCfg tools resolved
        LLM.ListChatModels -> do
          selected <- MVar.readMVar selection
          pure (chatModelInfos cfg selected)
        LLM.CurrentChatModel -> do
          selected <- MVar.readMVar selection
          pure (find (.current) (chatModelInfos cfg selected))
        LLM.QueryAccountBalance target -> do
          selected <- MVar.readMVar selection
          queryAccountBalances (activeChatConfig cfg selected) target
        LLM.ProbeChatModel target ->
          case resolveChatModelTarget cfg target of
            Left err ->
              pure (Left err)
            Right selectedName -> do
              let messages = [LLM.userText "Reply with exactly: OK"]
                  requestCfg = requestChatConfig cfg selectedName messages
              trySync (S.effects (Transport.askOpenAIStreaming requestCfg messages)) <&> \case
                Left (err :: SomeException) -> Left (llmExceptionSummary err)
                Right response
                  | Text.null (Text.strip response) -> Left "接口返回了空响应，模型未通过测试。请检查 API 地址是否需要包含 /v1，以及模型 ID 是否正确。"
                  | otherwise -> Right ()
        LLM.AddChatModel candidate ->
          case validateNewChatModel candidate of
            Left err -> pure (Left err)
            Right provider ->
              if any (\name -> Text.toCaseFold name == Text.toCaseFold candidate.profileName) (Map.keys cfg.chatProviders)
                then pure (Left "这个模型配置名称已经存在，请换一个名称。")
                else do
                  let target = Just candidate.profileName
                      probeCfg :: Config
                      probeCfg = cfg
                        { chatProvider = Just provider
                        , chatProviderName = target
                        , chatProviders = Map.singleton candidate.profileName provider
                        }
                      messages = [LLM.userText "Reply with exactly: OK"]
                      requestCfg = requestChatConfig probeCfg target messages
                  probe <- trySync (S.effects (Transport.askOpenAIStreaming requestCfg messages))
                  case probe of
                    Left (err :: SomeException) -> pure (Left (llmExceptionSummary err))
                    Right response
                      | Text.null (Text.strip response) ->
                          pure (Left "接口返回了空响应，模型未通过测试。请检查 API 地址是否需要包含 /v1，以及模型 ID 是否正确。")
                      | otherwise -> do
                      saved <- trySync (liftIO (appendChatProviderConfig candidate provider))
                      pure $ case saved of
                        Left (err :: SomeException) -> Left ("模型测试通过，但保存配置失败：" <> Text.pack (show err))
                        Right _ -> Right ()
        LLM.EditChatModel target patch ->
          case resolveChatModelTarget cfg target of
            Left err -> pure (Left err)
            Right Nothing -> pure (Left "找不到要修改的模型配置。")
            Right (Just oldName) ->
              case Map.lookup oldName cfg.chatProviders of
                Nothing -> pure (Left "找不到要修改的模型配置。")
                Just oldProvider -> do
                  let candidate = patchedChatModel oldName oldProvider patch
                  case validateNewChatModel candidate of
                    Left err -> pure (Left err)
                    Right provider ->
                      if renamedToExisting cfg oldName candidate.profileName
                        then pure (Left "新的模型配置名称已经存在，请换一个名称。")
                        else do
                          let newName = candidate.profileName
                              probeCfg :: Config
                              probeCfg = cfg
                                { chatProvider = Just provider
                                , chatProviderName = Just newName
                                , chatProviders = Map.singleton newName provider
                                }
                              messages = [LLM.userText "Reply with exactly: OK"]
                              requestCfg = requestChatConfig probeCfg (Just newName) messages
                          probe <- trySync (S.effects (Transport.askOpenAIStreaming requestCfg messages))
                          case probe of
                            Left (err :: SomeException) -> pure (Left (llmExceptionSummary err))
                            Right response
                              | Text.null (Text.strip response) -> pure (Left "接口返回了空响应，修改未保存。")
                              | otherwise -> do
                                  saved <- trySync (liftIO (replaceChatProviderConfig oldName candidate provider))
                                  pure $ case saved of
                                    Left (err :: SomeException) -> Left ("模型测试通过，但保存修改失败：" <> Text.pack (show err))
                                    Right _ -> Right ()
        LLM.DeleteChatModel target ->
          case resolveChatModelTarget cfg target of
            Left err -> pure (Left err)
            Right Nothing -> pure (Left "找不到要删除的模型配置。")
            Right (Just name)
              | Just name == cfg.chatProviderName -> pure (Left "不能删除当前默认模型，请先切换到其他模型。")
              | otherwise -> do
                  saved <- trySync (liftIO (deleteChatProviderConfig name))
                  pure $ case saved of
                    Left (err :: SomeException) -> Left ("删除模型配置失败：" <> Text.pack (show err))
                    Right _ -> Right ()
        LLM.SelectChatModel target ->
          case resolveChatModelTarget cfg target of
            Left err ->
              pure (Left err)
            Right selectedName -> do
              liftIO (persistChatModelSelection selectedName)
              MVar.modifyMVar_ selection (const (pure selectedName))
              logInfo [i|Chat model switched to provider=#{fromMaybe "default" selectedName}|]
              pure $ maybe (Left "The selected chat model is unavailable.") Right
                (find (.current) (chatModelInfos cfg selectedName))
        LLM.ResetChatModel -> do
          let selectedName = cfg.chatProviderName
          liftIO (persistChatModelSelection selectedName)
          MVar.modifyMVar_ selection (const (pure selectedName))
          logInfo [i|Chat model reset to configured default provider=#{fromMaybe "default" selectedName}|]
          pure (find (.current) (chatModelInfos cfg selectedName))
    )
    action

queryAccountBalances
  :: (HTTP.HTTP :> es, KatipE :> es, IOE :> es)
  => Config
  -> Maybe Text
  -> Eff es [LLM.AccountBalanceResult]
queryAccountBalances cfg target = do
  let chatActions =
        [ queryDeepSeekBalance provider
        | wantsBalanceTarget target ["deepseek"]
        , provider <- maybeToList cfg.chatProvider
        , isProviderBase "api.deepseek.com" provider.baseUrl
        ]
      imageActions =
        [ action
        | provider <- catMaybes [cfg.imageProvider, cfg.imageFallbackProvider]
        , action <- maybeToList (imageBalanceAction target provider)
        ]
      actions = chatActions <> imageActions
  if null actions
    then pure [balanceFailure "API" "没有找到对应的已配置余额渠道。"]
    else sequence actions

imageBalanceAction
  :: (HTTP.HTTP :> es, KatipE :> es, IOE :> es)
  => Maybe Text
  -> ImageProviderConfig
  -> Maybe (Eff es LLM.AccountBalanceResult)
imageBalanceAction target provider
  | isProviderBase "weilai.uk" provider.baseUrl
      && wantsBalanceTarget target ["weilai", "weilai.uk"] =
      Just (queryWeilaiBalance provider)
  | isProviderBase "botcf.com" provider.baseUrl
      && wantsBalanceTarget target ["botcf", "botcf.com"] =
      Just (queryBotcfBalance provider)
  | otherwise =
      Nothing

wantsBalanceTarget :: Maybe Text -> [Text] -> Bool
wantsBalanceTarget Nothing _ = True
wantsBalanceTarget (Just raw) aliases =
  Text.toCaseFold (Text.strip raw) `elem` aliases

isProviderBase :: Text -> Text -> Bool
isProviderBase host =
  Text.isInfixOf host . Text.toCaseFold

queryDeepSeekBalance
  :: (HTTP.HTTP :> es, KatipE :> es, IOE :> es)
  => ChatProviderConfig
  -> Eff es LLM.AccountBalanceResult
queryDeepSeekBalance provider =
  withProviderKey "DeepSeek" provider.apiKey \apiKey ->
    runBalanceQuery "DeepSeek"
      (responseBody <$> HTTP.runReq
        (req GET
          (https "api.deepseek.com" /: "user" /: "balance")
          NoReqBody
          jsonResponse
          (bearerHeader apiKey)))
      parseDeepSeekBalance

queryWeilaiBalance
  :: (HTTP.HTTP :> es, KatipE :> es, IOE :> es)
  => ImageProviderConfig
  -> Eff es LLM.AccountBalanceResult
queryWeilaiBalance provider =
  withProviderKey "WeiLai" provider.apiKey \apiKey ->
    runBalanceQuery "WeiLai"
      (responseBody <$> HTTP.runReq
        (req GET
          (https "weilai.uk" /: "v1" /: "usage")
          NoReqBody
          jsonResponse
          (bearerHeader apiKey)))
      parseWeilaiBalance

queryBotcfBalance
  :: (HTTP.HTTP :> es, KatipE :> es, IOE :> es)
  => ImageProviderConfig
  -> Eff es LLM.AccountBalanceResult
queryBotcfBalance provider =
  withProviderKey "BotCF" provider.apiKey \apiKey ->
    runBalanceQuery "BotCF"
      (responseBody <$> HTTP.runReq
        (req GET
          (https "botcf.com" /: "v1" /: "user" /: "balance")
          NoReqBody
          jsonResponse
          (bearerHeader apiKey)))
      parseBotcfBalance

withProviderKey
  :: Text
  -> Maybe Text
  -> (Text -> Eff es LLM.AccountBalanceResult)
  -> Eff es LLM.AccountBalanceResult
withProviderKey source apiKey action =
  case Text.strip <$> apiKey of
    Just key | not (Text.null key) -> action key
    _ -> pure (balanceFailure source "没有配置 API Key。")

runBalanceQuery
  :: (KatipE :> es, IOE :> es)
  => Text
  -> Eff es Aeson.Value
  -> (Aeson.Value -> Either Text LLM.AccountBalance)
  -> Eff es LLM.AccountBalanceResult
runBalanceQuery source requestAction parser = do
  result <- trySync requestAction
  case result of
    Left (err :: SomeException) -> do
      logWarning [i|#{source} balance request failed: #{displayException err}|]
      pure (balanceFailure source "余额接口暂时请求失败。")
    Right value ->
      pure LLM.AccountBalanceResult
        { sourceName = source
        , queryResult = parser value
        }

balanceFailure :: Text -> Text -> LLM.AccountBalanceResult
balanceFailure source err =
  LLM.AccountBalanceResult{sourceName = source, queryResult = Left err}

bearerHeader apiKey =
  header "Authorization" ("Bearer " <> TextEncoding.encodeUtf8 apiKey)

parseDeepSeekBalance :: Aeson.Value -> Either Text LLM.AccountBalance
parseDeepSeekBalance =
  parseBalance "DeepSeek" $ Aeson.withObject "DeepSeek balance" \object -> do
    available <- object Aeson..: "is_available"
    balanceInfos <- object Aeson..:? "balance_infos" Aeson..!= []
    fields <- concat <$> traverse parseItem balanceInfos
    pure LLM.AccountBalance{available, fields}
  where
    parseItem = Aeson.withObject "DeepSeek balance item" \object -> do
      currency <- object Aeson..: "currency"
      total <- requiredAmount object "total_balance"
      granted <- requiredAmount object "granted_balance"
      toppedUp <- requiredAmount object "topped_up_balance"
      pure
        [ ("总余额", amountWithUnit total currency)
        , ("充值余额", amountWithUnit toppedUp currency)
        , ("赠送余额", amountWithUnit granted currency)
        ]

parseWeilaiBalance :: Aeson.Value -> Either Text LLM.AccountBalance
parseWeilaiBalance =
  parseBalance "WeiLai" $ Aeson.withObject "WeiLai usage" \object -> do
    available <- object Aeson..:? "isValid" Aeson..!= True
    unit <- object Aeson..:? "unit" Aeson..!= "USD"
    remaining <- requiredAmount object "remaining"
    planName <- object Aeson..:? "planName"
    let fields = maybe [] (\name -> [("套餐", name)]) planName
          <> [("剩余余额", amountWithUnit remaining unit)]
    pure LLM.AccountBalance{available, fields}

parseBotcfBalance :: Aeson.Value -> Either Text LLM.AccountBalance
parseBotcfBalance =
  parseBalance "BotCF" $ Aeson.withObject "BotCF balance" \object -> do
    available <- object Aeson..:? "success" Aeson..!= True
    unit <- object Aeson..:? "unit" Aeson..!= "CNY"
    remaining <- requiredAmount object "remaining"
    total <- optionalAmount object "total"
    used <- optionalAmount object "used"
    let fields =
          [("剩余余额", amountWithUnit remaining unit)]
            <> maybe [] (\value -> [("累计额度", amountWithUnit value unit)]) total
            <> maybe [] (\value -> [("已使用", amountWithUnit value unit)]) used
    pure LLM.AccountBalance{available, fields}

parseBalance
  :: Text
  -> (Aeson.Value -> AesonTypes.Parser LLM.AccountBalance)
  -> Aeson.Value
  -> Either Text LLM.AccountBalance
parseBalance source parser value =
  first (const (source <> " 余额接口返回了无法识别的数据。")) $
    AesonTypes.parseEither parser value

requiredAmount :: AesonTypes.Object -> AesonKey.Key -> AesonTypes.Parser Text
requiredAmount object key =
  object Aeson..: key >>= amountText

optionalAmount :: AesonTypes.Object -> AesonKey.Key -> AesonTypes.Parser (Maybe Text)
optionalAmount object key =
  object Aeson..:? key >>= traverse amountText

amountText :: Aeson.Value -> AesonTypes.Parser Text
amountText = \case
  Aeson.String value -> pure value
  Aeson.Number value -> pure (Text.pack (show value))
  _ -> fail "amount must be a number or string"

amountWithUnit :: Text -> Text -> Text
amountWithUnit value unit =
  value <> " " <> unit

activeChatConfig :: Config -> Maybe Text -> Config
activeChatConfig cfg selectedName =
  cfg
    { chatProvider =
        (selectedName >>= (`Map.lookup` cfg.chatProviders))
          <|> cfg.chatProvider
    }

requestChatConfig :: Config -> Maybe Text -> [ChatMessage] -> Config
requestChatConfig cfg selectedName messages
  | null (chatImageRefs messages) = active
  | otherwise = fromMaybe active (visionChatConfig cfg)
  where
    active = activeChatConfig cfg selectedName

visionChatConfig :: Config -> Maybe Config
visionChatConfig cfg = do
  (name, provider) <- find (uncurry isVisionProvider) (Map.toAscList cfg.chatProviders)
  pure Config
    { chatProvider = Just provider
    , chatProviderName = Just name
    , chatProviders = cfg.chatProviders
    , imageProvider = cfg.imageProvider
    , imageFallbackProvider = cfg.imageFallbackProvider
    , audioProvider = cfg.audioProvider
    }

isVisionProvider :: Text -> ChatProviderConfig -> Bool
isVisionProvider name provider =
  any ("vision" `Text.isInfixOf`)
    [ Text.toCaseFold name
    , Text.toCaseFold provider.model
    ]

logAutomaticVisionRoute :: KatipE :> es => Config -> Maybe Text -> Config -> Eff es ()
logAutomaticVisionRoute cfg selectedName requestCfg = do
  let active = activeChatConfig cfg selectedName
      routedName = requestCfg.chatProviderName
  when (routedName /= active.chatProviderName) $
    logInfo [i|Automatically routing image context to chat provider=#{fromMaybe "vision" routedName}|]

chatModelInfos :: Config -> Maybe Text -> [LLM.ChatModelInfo]
chatModelInfos cfg selectedName
  | Map.null cfg.chatProviders =
      case cfg.chatProvider of
        Nothing -> []
        Just provider -> [modelInfo (fromMaybe "default" cfg.chatProviderName) provider]
  | otherwise =
      [ modelInfo name provider
      | (name, provider) <- Map.toAscList cfg.chatProviders
      ]
  where
    activeName = selectedName <|> cfg.chatProviderName
    modelInfo name provider =
      LLM.ChatModelInfo
        { provider = name
        , model = provider.model
        , current = activeName == Just name || (isNothing activeName && Just provider == cfg.chatProvider)
        , configuredDefault = cfg.chatProviderName == Just name
        }

resolveChatModelTarget :: Config -> Text -> Either Text (Maybe Text)
resolveChatModelTarget cfg rawTarget
  | Text.null target =
      Left "Model target must not be empty."
  | [name] <- aliasMatches =
      Right (Just name)
  | [name] <- modelMatches =
      Right (Just name)
  | length modelMatches > 1 =
      Left ("That model id belongs to multiple profiles; use one of these profile names: " <> Text.intercalate ", " modelMatches)
  | otherwise =
      Left ("Unknown chat model profile or model id: " <> target <> ". Available profiles: " <> Text.intercalate ", " (Map.keys cfg.chatProviders))
  where
    target = Text.toCaseFold (Text.strip rawTarget)
    aliasMatches =
      [ name
      | name <- Map.keys cfg.chatProviders
      , Text.toCaseFold name == target
      ]
    modelMatches =
      [ name
      | (name, provider) <- Map.toAscList cfg.chatProviders
      , Text.toCaseFold provider.model == target
      ]

chatModelSelectionFile :: FilePath
chatModelSelectionFile =
  "chat-model-selection"

loadChatModelSelection :: Config -> IO (Maybe Text)
loadChatModelSelection cfg =
  catchIOError load (const (pure cfg.chatProviderName))
  where
    load = do
      exists <- Directory.doesFileExist chatModelSelectionFile
      if not exists
        then pure cfg.chatProviderName
        else do
          selected <- Text.strip <$> TextIO.readFile chatModelSelectionFile
          pure $ if Map.member selected cfg.chatProviders
            then Just selected
            else cfg.chatProviderName

persistChatModelSelection :: Maybe Text -> IO ()
persistChatModelSelection selectedName =
  write
  where
    tempPath = chatModelSelectionFile <> ".tmp"
    write = do
      Directory.createDirectoryIfMissing True (takeDirectory chatModelSelectionFile)
      TextIO.writeFile tempPath (fromMaybe "" selectedName <> "\n")
      Directory.renameFile tempPath chatModelSelectionFile

validateNewChatModel :: LLM.ChatModelConfig -> Either Text ChatProviderConfig
validateNewChatModel candidate
  | Text.null name = Left "模型配置名称不能为空。"
  | not (Text.all validNameChar name) = Left "模型配置名称只能使用字母、数字、短横线或下划线。"
  | Text.null base = Left "API 地址不能为空。"
  | not (Text.isPrefixOf "http://" (Text.toCaseFold base) || Text.isPrefixOf "https://" (Text.toCaseFold base)) = Left "API 地址必须以 http:// 或 https:// 开头。"
  | Text.null key = Left "API Key 不能为空。"
  | Text.null modelName = Left "模型 ID 不能为空。"
  | timeout <= 0 = Left "超时时间必须是正数。"
  | otherwise = Right ChatProviderConfig
      { baseUrl = base
      , apiKey = Just key
      , model = modelName
      , reasoningEffort = effort
      , requestTimeout = timeout
      }
  where
    name = Text.strip candidate.profileName
    base = Text.strip candidate.baseUrl
    key = Text.strip candidate.apiKey
    modelName = Text.strip candidate.model
    effort = if Text.null (Text.strip candidate.reasoningEffort) then "low" else Text.strip candidate.reasoningEffort
    timeout = candidate.requestTimeout
    validNameChar c = isAlphaNum c || c == '-' || c == '_'

patchedChatModel :: Text -> ChatProviderConfig -> LLM.ChatModelPatch -> LLM.ChatModelConfig
patchedChatModel oldName old LLM.ChatModelPatch{newProfileName, newBaseUrl, newApiKey, newModel, newReasoningEffort, newRequestTimeout} =
  LLM.ChatModelConfig
    { profileName = fromMaybe oldName (Text.strip <$> newProfileName)
    , baseUrl = fromMaybe old.baseUrl (Text.strip <$> newBaseUrl)
    , apiKey = fromMaybe (fromMaybe "" old.apiKey) (Text.strip <$> newApiKey)
    , model = fromMaybe old.model (Text.strip <$> newModel)
    , reasoningEffort = fromMaybe old.reasoningEffort (Text.strip <$> newReasoningEffort)
    , requestTimeout = fromMaybe old.requestTimeout newRequestTimeout
    }

renamedToExisting :: Config -> Text -> Text -> Bool
renamedToExisting cfg oldName newName =
  Text.toCaseFold newName /= Text.toCaseFold oldName
    && any (\name -> Text.toCaseFold name == Text.toCaseFold newName) (Map.keys cfg.chatProviders)

appendChatProviderConfig :: LLM.ChatModelConfig -> ChatProviderConfig -> IO ()
appendChatProviderConfig candidate provider = do
  content <- TextIO.readFile "config.toml"
  let name = Text.strip candidate.profileName
      newline = if Text.isSuffixOf "\n" content then "" else "\n"
      section = Text.concat
        [ newline
        , "\n[llm.chat_provider.", name, "]\n"
        , "base_url = ", tomlString provider.baseUrl, "\n"
        , "api_key = ", tomlString (fromMaybe "" provider.apiKey), "\n"
        , "model = ", tomlString provider.model, "\n"
        , "reasoning_effort = ", tomlString provider.reasoningEffort, "\n"
        , "timeout = ", showText provider.requestTimeout, "\n"
        ]
      tempPath = "config.toml.fm-model.tmp"
  TextIO.writeFile tempPath (content <> section)
  Directory.renameFile tempPath "config.toml"

replaceChatProviderConfig :: Text -> LLM.ChatModelConfig -> ChatProviderConfig -> IO ()
replaceChatProviderConfig oldName candidate provider = do
  content <- TextIO.readFile "config.toml"
  let withoutOld = removeChatProviderSection oldName content
      section = chatProviderSection candidate provider
      tempPath = "config.toml.fm-model.tmp"
      newline = if Text.isSuffixOf "\n" withoutOld then "" else "\n"
  TextIO.writeFile tempPath (withoutOld <> newline <> "\n" <> section)
  Directory.renameFile tempPath "config.toml"

deleteChatProviderConfig :: Text -> IO ()
deleteChatProviderConfig name = do
  content <- TextIO.readFile "config.toml"
  let tempPath = "config.toml.fm-model.tmp"
  TextIO.writeFile tempPath (removeChatProviderSection name content)
  Directory.renameFile tempPath "config.toml"

chatProviderSection :: LLM.ChatModelConfig -> ChatProviderConfig -> Text
chatProviderSection candidate provider =
  Text.concat
    [ "[llm.chat_provider.", Text.strip candidate.profileName, "]\n"
    , "base_url = ", tomlString provider.baseUrl, "\n"
    , "api_key = ", tomlString (fromMaybe "" provider.apiKey), "\n"
    , "model = ", tomlString provider.model, "\n"
    , "reasoning_effort = ", tomlString provider.reasoningEffort, "\n"
    , "timeout = ", showText provider.requestTimeout, "\n"
    ]

removeChatProviderSection :: Text -> Text -> Text
removeChatProviderSection name content =
  Text.unlines (go (Text.lines content))
  where
    header = "[llm.chat_provider." <> Text.strip name <> "]"
    go [] = []
    go (line : rest)
      | Text.strip line == header = skipUntilNextSection rest
      | otherwise = line : go rest
    skipUntilNextSection [] = []
    skipUntilNextSection (line : rest)
      | Text.isPrefixOf "[" (Text.strip line) = go (line : rest)
      | otherwise = skipUntilNextSection rest

tomlString :: Text -> Text
tomlString value =
  "\"" <> Text.replace "\\" "\\\\" (Text.replace "\"" "\\\"" value) <> "\""

showText :: Show a => a -> Text
showText = Text.pack . show

resolveChatMessages :: Media.Media :> es => [ChatMessage] -> Eff es [ChatMessage]
resolveChatMessages =
  traverse resolveChatMessage

resolveChatMessagesTimed :: (IOE :> es, KatipE :> es, Media.Media :> es) => [ChatMessage] -> Eff es [ChatMessage]
resolveChatMessagesTimed messages = do
  startedAt <- monotonicMilliseconds
  resolved <- resolveChatMessages messages
  finishedAt <- monotonicMilliseconds
  let refs = chatImageRefs messages
  logDebug [i|LLM media resolution media_refs=#{length refs} distinct_refs=#{length (ordNub refs)} duration_ms=#{finishedAt - startedAt}|]
  pure resolved

chatImageRefs :: [ChatMessage] -> [Text]
chatImageRefs =
  concatMap messageImageRefs

messageImageRefs :: ChatMessage -> [Text]
messageImageRefs message =
  case message.content of
    Just (PartsContent parts) ->
      [ ref
      | ImageUrlPart ref <- parts
      ]
    _ ->
      []

monotonicMilliseconds :: IOE :> es => Eff es Integer
monotonicMilliseconds =
  fromIntegral . (`div` 1_000_000) <$> liftIO getMonotonicTimeNSec

resolveChatMessage :: Media.Media :> es => ChatMessage -> Eff es ChatMessage
resolveChatMessage message =
  case message.content of
    Just (PartsContent parts) -> do
      resolvedParts <- traverse resolveContentPart parts
      pure ChatMessage
        { role = message.role
        , content = Just (PartsContent resolvedParts)
        , toolCalls = message.toolCalls
        , toolCallId = message.toolCallId
        }
    _ ->
      pure message

resolveContentPart :: Media.Media :> es => ContentPart -> Eff es ContentPart
resolveContentPart = \case
  ImageUrlPart ref ->
    ImageUrlPart <$> Media.modelImageRef ref
  part ->
    pure part

-- Image Generation Media Streaming

askImageStreamingWithMedia
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Timeout :> es, Media.Media :> es, FileSystem :> es, Process :> es, Fail :> es)
  => Config
  -> LLM.ImageRequestOptions
  -> [ChatMessage]
  -> Stream (Of Text) (Eff es) Text
askImageStreamingWithMedia cfg options messages =
  normalizeReplyResult (Transport.askImageOpenAIStreaming cfg options messages (storeImageFromTransport "LLM image streaming request"))

imageStreamWithFallback
  :: (Concurrent :> es, HTTP.HTTP :> es, IOE :> es, KatipE :> es, Timeout :> es, Media.Media :> es, FileSystem :> es, Process :> es, Fail :> es)
  => Config
  -> LLM.ImageRequestOptions
  -> [ChatMessage]
  -> Stream (Of Text) (Eff es) Text
imageStreamWithFallback cfg options messages =
  catchStream primary $ \err ->
    case cfg.imageFallbackProvider of
      Nothing -> lift (throwIO err)
      Just fallbackProvider@ImageProviderConfig{model = fallbackModel} -> do
        lift $ logWarning [i|Primary image provider failed; switching to fallback image provider endpoint=#{safeImageEndpoint fallbackProvider} model=#{fallbackModel}|]
        Retry.retryLLMStreamRequest "LLM fallback image streaming request" $
          askImageStreamingWithMedia
            cfg{imageProvider = Just fallbackProvider, imageFallbackProvider = Nothing}
            options
            messages
  where
    primary =
      Retry.retryLLMStreamRequest "LLM image streaming request" $
        askImageStreamingWithMedia cfg options messages

catchStream
  :: Stream (Of a) (Eff es) r
  -> (SomeException -> Stream (Of a) (Eff es) r)
  -> Stream (Of a) (Eff es) r
catchStream stream handler = do
  inspected <- lift (try (Streaming.inspect stream))
  case inspected of
    Left err -> handler err
    Right (Left result) -> pure result
    Right (Right (value S.:> rest)) -> do
      S.yield value
      catchStream rest handler

askImageEditStreamingWithMedia
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Timeout :> es, Media.Media :> es, FileSystem :> es, Fail :> es)
  => Config
  -> LLM.ImageRequestOptions
  -> Text
  -> [Text]
  -> Maybe Text
  -> Stream (Of Text) (Eff es) Text
askImageEditStreamingWithMedia cfg options prompt imageRefs maskRef =
  Transport.askImageEditOpenAIStreaming cfg options prompt imageRefs maskRef (storeImageFromTransport "LLM image edit streaming request")

storeImageFromTransport
  :: (IOE :> es, Timeout :> es, Media.Media :> es)
  => Text
  -> ImageProviderConfig
  -> Text
  -> Q.ByteStream (Eff es) ()
  -> Stream (Of Text) (Eff es) Text
storeImageFromTransport label ImageProviderConfig{baseUrl, model, requestTimeout, outputFormat} _key bytes = do
  let mime = generatedImageMimeType outputFormat
      sourceName = Just (generatedImageSourceName mime baseUrl model)
  storeImageByteStream label requestTimeout mime sourceName bytes

storeImageByteStream
  :: (IOE :> es, Timeout :> es, Media.Media :> es)
  => Text
  -> Int
  -> Text
  -> Maybe Text
  -> Q.ByteStream (Eff es) ()
  -> Stream (Of Text) (Eff es) Text
storeImageByteStream label requestTimeout mime sourceName bytes = do
  ref <- lift $
    runTimedImageMediaStore label requestTimeout $
      withEffToIO (ConcUnlift Persistent Unlimited) \runInIO ->
        runInIO $
          Media.storeMediaObject Media.MediaObject
          { bytes = effByteStreamToResourceTIO runInIO bytes
          , mimeType = mime
          , sourceName
          }
  case ref of
    Nothing ->
      lift (throwIO (LLMException "Image generation response could not be stored in media cache."))
    Just mediaRef -> do
      let answer = ReplyBody.imageDirective mediaRef
      S.yield answer
      pure answer

runTimedImageMediaStore :: (Timeout :> es, IOE :> es) => Text -> Int -> Eff es a -> Eff es a
runTimedImageMediaStore label timeoutSeconds action = do
  result <- timeout (timeoutSeconds * 1000000) action
  case result of
    Just value ->
      pure value
    Nothing ->
      throwIO (LLMException [i|#{label} timed out after #{timeoutSeconds} seconds.|])

effByteStreamToResourceTIO
  :: (forall a. Eff es a -> IO a)
  -> Q.ByteStream (Eff es) ()
  -> Q.ByteStream (ResourceT IO) ()
effByteStreamToResourceTIO runInIO byteStream =
  Q.fromChunks (go (Q.toChunks byteStream))
  where
    go chunks = do
      next <- liftIO (runInIO (S.next chunks))
      case next of
        Left () ->
          pure ()
        Right (chunk, rest) -> do
          S.yield chunk
          go rest

generatedImageMimeType :: Maybe Text -> Text
generatedImageMimeType outputFormat =
  case Text.toLower . Text.strip <$> outputFormat of
    Just "jpeg" -> "image/jpeg"
    Just "webp" -> "image/webp"
    _ -> "image/png"

generatedImageSourceName :: Text -> Text -> Text -> Text
generatedImageSourceName mime baseUrl model =
  let extension = case Text.toLower mime of
        "image/jpeg" -> "jpg"
        "image/webp" -> "webp"
        _ -> "png"
      provider = sanitizeFilePart (imageProviderHost baseUrl)
      modelPart = sanitizeFilePart model
  in "llm-image-" <> provider <> "-" <> modelPart <> "." <> extension

imageProviderHost :: Text -> Text
imageProviderHost url =
  let withoutScheme = Text.dropWhile (== '/') (Text.dropWhile (/= ':') url & Text.drop 1)
  in Text.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#') withoutScheme

sanitizeFilePart :: Text -> Text
sanitizeFilePart = Text.map (\c -> if isAlphaNum c || c == '.' || c == '_' || c == '-' then c else '-')

safeImageEndpoint :: ImageProviderConfig -> Text
safeImageEndpoint provider = imageProviderHost provider.baseUrl

-- Reply Normalization

data ReplyNormalizeState
  = ReplyNormal !Text
  | ReplyCollectImage !Text

normalizeReplyResult :: Media.Media :> es => Stream (Of Text) (Eff es) Text -> Stream (Of Text) (Eff es) Text
normalizeReplyResult stream =
  normalizeReplyResultWith (ReplyNormal "") stream

normalizeReplyResultWith :: Media.Media :> es => ReplyNormalizeState -> Stream (Of Text) (Eff es) Text -> Stream (Of Text) (Eff es) Text
normalizeReplyResultWith normalizeState stream =
  lift (S.next stream) >>= \case
    Left result -> do
      flushReplyNormalizeState normalizeState
      lift (Media.normalizeReplyBody result)
    Right (chunk, rest) -> do
      nextState <- normalizeReplyChunk normalizeState chunk
      normalizeReplyResultWith nextState rest

normalizeReplyChunk :: Media.Media :> es => ReplyNormalizeState -> Text -> Stream (Of Text) (Eff es) ReplyNormalizeState
normalizeReplyChunk normalizeState chunk =
  case normalizeState of
    ReplyNormal pending ->
      processNormal (pending <> chunk)
    ReplyCollectImage pending ->
      processCollectedImage (pending <> chunk)

processNormal :: Media.Media :> es => Text -> Stream (Of Text) (Eff es) ReplyNormalizeState
processNormal text =
  case findImageDirectiveStart text of
    Just offset -> do
      let (prefix, imageAndRest) = Text.splitAt offset text
      yieldNonEmpty prefix
      processCollectedImage imageAndRest
    Nothing -> do
      let (ready, pending) = splitReadyText text
      yieldNonEmpty ready
      pure (ReplyNormal pending)

processCollectedImage :: Media.Media :> es => Text -> Stream (Of Text) (Eff es) ReplyNormalizeState
processCollectedImage text =
  case Text.breakOn "\n" text of
    (imageLine, restWithNewline)
      | Text.null restWithNewline ->
          pure (ReplyCollectImage imageLine)
      | otherwise -> do
          normalized <- lift (Media.normalizeReplyBody imageLine)
          yieldNonEmpty normalized
          S.yield "\n"
          processNormal (Text.drop 1 restWithNewline)

flushReplyNormalizeState :: Media.Media :> es => ReplyNormalizeState -> Stream (Of Text) (Eff es) ()
flushReplyNormalizeState = \case
  ReplyNormal pending ->
    yieldNonEmpty pending
  ReplyCollectImage pending -> do
    normalized <- lift (Media.normalizeReplyBody pending)
    yieldNonEmpty normalized

findImageDirectiveStart :: Text -> Maybe Int
findImageDirectiveStart =
  go 0
  where
    go offset text =
      case Text.breakOn imageDirectivePrefix text of
        (_, "") ->
          Nothing
        (before, _matched)
          | imageDirectiveAtLineStart before ->
              Just (offset + Text.length before)
          | otherwise ->
              let consumed = Text.length before + 1
              in go (offset + consumed) (Text.drop consumed text)

imageDirectiveAtLineStart :: Text -> Bool
imageDirectiveAtLineStart before =
  Text.all isHorizontalSpace (Text.takeWhileEnd (/= '\n') before)

splitReadyText :: Text -> (Text, Text)
splitReadyText text =
  Text.splitAt (max 0 (Text.length text - imageDirectiveOverlapChars)) text

imageDirectivePrefix :: Text
imageDirectivePrefix =
  "[image] "

imageDirectiveOverlapChars :: Int
imageDirectiveOverlapChars =
  Text.length imageDirectivePrefix + 1

isHorizontalSpace :: Char -> Bool
isHorizontalSpace char =
  char == ' ' || char == '\t' || char == '\r'

yieldNonEmpty :: Text -> Stream (Of Text) (Eff es) ()
yieldNonEmpty text =
  unless (Text.null text) (S.yield text)
