{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.RPC.RuntimeConfig
Description : Authenticated runtime configuration RPC operations
-}
module Bot.RPC.RuntimeConfig
  ( configMethod
  )
where

import Bot.Prelude
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Memory as Memory
import qualified Bot.JSONRPC as RPC
import qualified Bot.Memory as MemoryStore
import qualified Bot.Trigger as Trigger
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text

configMethod
  :: forall es. (LLM.LLM :> es, Memory.Memory :> es, IOE :> es)
  => RPC.RpcRequest
  -> Eff es (Maybe (Either RPC.RpcError Aeson.Value))
configMethod request =
  case RPC.requestMethod request of
    "config.snapshot" -> Just . Right <$> snapshot
    "config.persona" -> Just <$> personaAction (RPC.requestParams request)
    "config.trigger" -> Just <$> triggerAction (RPC.requestParams request)
    "config.model" -> Just <$> modelAction (RPC.requestParams request)
    _ -> pure Nothing
  where
    snapshot :: Eff es Aeson.Value
    snapshot = do
      models <- LLM.listChatModels
      private <- Memory.listPrivatePersonas
      groups <- Memory.listGroupPersonas
      styles <- Memory.listMemberStyles
      privateDefault <- Memory.loadMemory MemoryStore.DefaultPrivatePersonaMemory
      groupDefault <- Memory.loadMemory MemoryStore.DefaultGroupPersonaMemory
      triggers <- liftIO Trigger.listTriggerConfigs
      pure $ Aeson.object
        [ "models" Aeson..= map modelValue models
        , "private_default" Aeson..= privateDefault
        , "private_personas" Aeson..= map pairValue private
        , "group_default" Aeson..= groupDefault
        , "group_personas" Aeson..= map groupValue groups
        , "member_styles" Aeson..= map pairValue styles
        , "triggers" Aeson..= map triggerValue triggers
        ]

    personaAction :: Aeson.Value -> Eff es (Either RPC.RpcError Aeson.Value)
    personaAction value = case parsePersona value of
      Left message -> pure (Left (RPC.rpcError "invalid_params" message))
      Right PersonaArgs{..} -> case (personaScope scope ident :: Either Text MemoryStore.MemoryScope) of
        Left message -> pure (Left (RPC.rpcError "invalid_params" message))
        Right target -> case action of
          "get" -> do
            current <- Memory.loadMemory target
            pure (Right (personaResult target current))
          "status" -> do
            current <- Memory.loadMemory target
            pure (Right (personaResult target current))
          "set" -> case content of
            Nothing -> pure (Left (RPC.rpcError "invalid_params" "content is required for set"))
            Just text
              | Text.null (Text.strip text) -> pure (Left (RPC.rpcError "invalid_params" "content must not be empty"))
              | Text.length text > MemoryStore.memoryLimitChars -> pure (Left (RPC.rpcError "invalid_params" "content exceeds 1000 characters"))
              | otherwise -> Memory.replaceMemory target text >> pure (Right (savedValue target))
          "clear" -> Memory.clearMemory target >> pure (Right (savedValue target))
          _ -> pure (Left (RPC.rpcError "invalid_params" "action must be get, status, set, or clear"))

    triggerAction :: Aeson.Value -> Eff es (Either RPC.RpcError Aeson.Value)
    triggerAction value = case parseTrigger value of
      Left message -> pure (Left (RPC.rpcError "invalid_params" message))
      Right TriggerArgs{..}
        | action == "list" -> Right . Aeson.toJSON . map triggerValue <$> liftIO Trigger.listTriggerConfigs
        | Text.null (Text.strip scope) -> pure (Left (RPC.rpcError "invalid_params" "scope is required"))
        | action == "get" || action == "status" -> do
            current <- liftIO (Trigger.loadTriggerConfigByKey scope)
            pure (Right (triggerResult scope current))
        | action == "clear" -> liftIO (Trigger.clearTriggerConfigByKey scope) >> pure (Right (triggerSavedValue scope))
        | action == "set" -> do
            let modes = traverse parseMode modesRaw :: Either Text [Trigger.TriggerMode]
            case modes of
              Left message -> pure (Left (RPC.rpcError "invalid_params" message))
              Right parsed
                | null parsed -> pure (Left (RPC.rpcError "invalid_params" "modes must not be empty"))
                | otherwise -> do
                    let config = Trigger.TriggerConfig (ordNub parsed) (clean keywords)
                    liftIO (Trigger.saveTriggerConfigByKey scope config)
                    pure (Right (triggerSavedValue scope))
        | otherwise -> pure (Left (RPC.rpcError "invalid_params" "action must be list, get, status, set, or clear"))
      where
        parseMode :: Text -> Either Text Trigger.TriggerMode
        parseMode raw = maybe (Left ("unknown trigger mode: " <> raw)) Right (Trigger.parseTriggerMode raw)
        clean = filter (not . Text.null) . map Text.strip

    modelAction :: Aeson.Value -> Eff es (Either RPC.RpcError Aeson.Value)
    modelAction value = case parseModel value of
      Left message -> pure (Left (RPC.rpcError "invalid_params" message))
      Right ModelArgs{..} -> case action of
        "status" -> (Right . Aeson.object . (: []) . ("models" Aeson..=) . map modelValue) <$> LLM.listChatModels
        "switch" -> case target of
          Nothing -> pure (Left (RPC.rpcError "invalid_params" "target is required for switch"))
          Just name -> LLM.probeChatModel name >>= \case
            Left err -> pure (Left (RPC.rpcError "validation_failed" ("model probe failed: " <> err)))
            Right () -> LLM.selectChatModel name >>= resultToRpc
        "reset" -> do
          result <- LLM.resetChatModel
          pure $ Right $ Aeson.object
            [ "saved" Aeson..= True
            , "applied" Aeson..= isJust result
            , "model" Aeson..= (modelValue <$> result)
            ]
        "add" -> case (profileName, baseUrl, apiKey, modelName) of
          (Just name, Just base, Just modelApiKey, Just modelId) -> resultUnitToRpc =<< LLM.addChatModel LLM.ChatModelConfig
            { profileName = name, baseUrl = base, apiKey = modelApiKey, model = modelId
            , reasoningEffort = fromMaybe "low" reasoning, requestTimeout = fromMaybe 60 timeout }
          _ -> pure (Left (RPC.rpcError "invalid_params" "add requires profile_name, base_url, api_key, and model"))
        "edit" -> case target of
          Nothing -> pure (Left (RPC.rpcError "invalid_params" "target is required for edit"))
          Just name -> resultUnitToRpc =<< LLM.editChatModel name LLM.ChatModelPatch
            { newProfileName = profileName, newBaseUrl = baseUrl, newApiKey = apiKey
            , newModel = modelName, newReasoningEffort = reasoning, newRequestTimeout = timeout }
        "delete" -> maybe (pure (Left (RPC.rpcError "invalid_params" "target is required for delete"))) (resultUnitToRpc <=< LLM.deleteChatModel) target
        _ -> pure (Left (RPC.rpcError "invalid_params" "action must be status, add, edit, delete, switch, or reset"))

    resultUnitToRpc :: Either Text () -> Eff es (Either RPC.RpcError Aeson.Value)
    resultUnitToRpc result = pure $ case result of
      Left err -> Left (RPC.rpcError "operation_failed" err)
      Right () -> Right (Aeson.object ["saved" Aeson..= True, "applied" Aeson..= True])
    resultToRpc :: Either Text LLM.ChatModelInfo -> Eff es (Either RPC.RpcError Aeson.Value)
    resultToRpc result = pure $ case result of
      Left err -> Left (RPC.rpcError "operation_failed" err)
      Right info -> Right (Aeson.object ["saved" Aeson..= True, "applied" Aeson..= True, "model" Aeson..= modelValue info])

data PersonaArgs = PersonaArgs { action :: Text, scope :: Text, ident :: Maybe Text, content :: Maybe Text }
data TriggerArgs = TriggerArgs { action :: Text, scope :: Text, modesRaw :: [Text], keywords :: [Text] }
data ModelArgs = ModelArgs { action :: Text, target :: Maybe Text, profileName :: Maybe Text, baseUrl :: Maybe Text, apiKey :: Maybe Text, modelName :: Maybe Text, reasoning :: Maybe Text, timeout :: Maybe Int }

parsePersona :: Aeson.Value -> Either Text PersonaArgs
parsePersona value = first toText
  (AesonTypes.parseEither (Aeson.withObject "config.persona params" (\o ->
    PersonaArgs <$> o Aeson..:? "action" Aeson..!= "get" <*> o Aeson..: "scope" <*> o Aeson..:? "id" <*> o Aeson..:? "content"
  )) value)

parseTrigger :: Aeson.Value -> Either Text TriggerArgs
parseTrigger value = first toText
  (AesonTypes.parseEither (Aeson.withObject "config.trigger params" (\o ->
    TriggerArgs <$> o Aeson..:? "action" Aeson..!= "get" <*> o Aeson..:? "scope" Aeson..!= "" <*> o Aeson..:? "modes" Aeson..!= [] <*> o Aeson..:? "keywords" Aeson..!= []
  )) value)

parseModel :: Aeson.Value -> Either Text ModelArgs
parseModel value = first toText
  (AesonTypes.parseEither (Aeson.withObject "config.model params" (\o ->
    ModelArgs <$> o Aeson..:? "action" Aeson..!= "status" <*> o Aeson..:? "target" <*> o Aeson..:? "profile_name" <*> o Aeson..:? "base_url" <*> o Aeson..:? "api_key" <*> o Aeson..:? "model" <*> o Aeson..:? "reasoning_effort" <*> o Aeson..:? "timeout"
  )) value)

personaScope :: Text -> Maybe Text -> Either Text MemoryStore.MemoryScope
personaScope raw ident = case Text.toCaseFold (Text.strip raw) of
  "private" -> MemoryStore.PrivatePersonaMemory <$> requireId "private" ident
  "private_default" -> Right MemoryStore.DefaultPrivatePersonaMemory
  "group" -> MemoryStore.GroupPersonaMemory <$> requireInteger "group" ident
  "group_default" -> Right MemoryStore.DefaultGroupPersonaMemory
  "member" -> MemoryStore.MemberStyleMemory <$> requireId "member" ident
  _ -> Left "scope must be private, private_default, group, group_default, or member"
  where
    requireId :: Text -> Maybe Text -> Either Text Text
    requireId label value =
      maybe (Left (label <> " scope requires id")) Right (value >>= nonEmpty)
    requireInteger :: Text -> Maybe Text -> Either Text Integer
    requireInteger label value =
      maybe (Left (label <> " scope requires a positive numeric id")) Right (value >>= readPositive . Text.unpack)
    readPositive text = case readMaybe text of Just number | number > (0 :: Integer) -> Just number; _ -> Nothing
    nonEmpty value = let clean = Text.strip value in if Text.null clean then Nothing else Just clean

personaResult scope_ value = Aeson.object ["scope" Aeson..= showScope scope_, "content" Aeson..= value, "saved" Aeson..= isJust value, "applied" Aeson..= isJust value]
savedValue scope_ = Aeson.object ["scope" Aeson..= showScope scope_, "saved" Aeson..= True, "applied" Aeson..= True]
triggerSavedValue scope_ = Aeson.object ["scope" Aeson..= scope_, "saved" Aeson..= True, "applied" Aeson..= True]
showScope = \case
  MemoryStore.PrivatePersonaMemory id_ -> "private:" <> id_
  MemoryStore.DefaultPrivatePersonaMemory -> "private_default"
  MemoryStore.GroupPersonaMemory id_ -> "group:" <> Text.pack (show id_)
  MemoryStore.DefaultGroupPersonaMemory -> "group_default"
  MemoryStore.MemberStyleMemory id_ -> "member:" <> id_
  _ -> "unsupported"

triggerResult scope_ value = Aeson.object ["scope" Aeson..= scope_, "config" Aeson..= value, "saved" Aeson..= isJust value, "applied" Aeson..= isJust value]
pairValue (entryId, value) = Aeson.object ["id" Aeson..= entryId, "content" Aeson..= value]
groupValue (entryId, value) = Aeson.object ["id" Aeson..= entryId, "content" Aeson..= value]
triggerValue (scopeKey, value) = Aeson.object ["scope" Aeson..= scopeKey, "config" Aeson..= value]
modelValue info = Aeson.object ["provider" Aeson..= info.provider, "model" Aeson..= info.model, "current" Aeson..= info.current, "configured_default" Aeson..= info.configuredDefault]
