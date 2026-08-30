{-|
Module      : Bot.RPC.Audit
Description : Agent audit JSON-RPC method handlers
Stability   : experimental
-}

module Bot.RPC.Audit
  ( auditRpcCallbacks
  )
where

import qualified Bot.Effect.AgentAudit as AgentAudit
import Bot.Prelude
import Bot.Core.Message
import Bot.Core.Thread
import qualified Bot.JSONRPC as RPC
import Bot.RPC.Server (RpcServerCallbacks (..), noRpcServerCallbacks)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes

auditRpcCallbacks :: AgentAudit.AgentAudit :> es => RpcServerCallbacks es
auditRpcCallbacks =
  noRpcServerCallbacks{auditMethod = dispatchAuditMethod}

dispatchAuditMethod
  :: AgentAudit.AgentAudit :> es
  => RPC.RpcRequest
  -> Eff es (Maybe (Either RPC.RpcError Aeson.Value))
dispatchAuditMethod request =
  Just <$> case RPC.requestMethod request of
    "audit.recent" ->
      parseParams request parseLimit \limit ->
        Aeson.toJSON <$> AgentAudit.queryRecentAuditRecords limit
    "audit.get" ->
      parseParams request parseAuditId \auditId ->
        Aeson.toJSON <$> AgentAudit.queryAuditRecord auditId
    "audit.thread" ->
      parseParams request parseMessageKey \messageKey ->
        Aeson.toJSON <$> AgentAudit.queryThreadAudit messageKey
    "audit.thread_messages" ->
      parseParams request parseMessageKeys \messageKeys ->
        Aeson.toJSON <$> AgentAudit.queryThreadMessagesAudit messageKeys
    "audit.subscribe" ->
      pure (Right (Aeson.object ["subscribed" Aeson..= True]))
    _ ->
      let method = RPC.requestMethod request
      in pure (Left (RPC.rpcError "method_not_found" [i|Unknown RPC method: #{method}|]))

parseParams
  :: RPC.RpcRequest
  -> (Aeson.Value -> AesonTypes.Parser a)
  -> (a -> Eff es Aeson.Value)
  -> Eff es (Either RPC.RpcError Aeson.Value)
parseParams request parser action =
  case AesonTypes.parseEither parser (RPC.requestParams request) of
    Left err ->
      pure (Left (RPC.rpcError "invalid_params" (toText err)))
    Right value ->
      Right <$> action value

parseLimit :: Aeson.Value -> AesonTypes.Parser Int
parseLimit =
  Aeson.withObject "audit.recent params" \o ->
    fromMaybe 20 <$> o Aeson..:? "limit"

parseAuditId :: Aeson.Value -> AesonTypes.Parser Integer
parseAuditId =
  Aeson.withObject "audit.get params" \o ->
    o Aeson..: "audit_id" <|> o Aeson..: "id"

parseMessageKey :: Aeson.Value -> AesonTypes.Parser ThreadMessageKey
parseMessageKey =
  Aeson.withObject "audit.thread params" \o ->
    ThreadMessageKey
      <$> (o Aeson..: "platform" >>= parsePlatform)
      <*> o Aeson..:? "chat_id"
      <*> (textMessageId <$> o Aeson..: "message_id")

parseMessageKeys :: Aeson.Value -> AesonTypes.Parser [ThreadMessageKey]
parseMessageKeys =
  Aeson.withObject "audit.thread_messages params" \o -> do
    platform <- o Aeson..: "platform" >>= parsePlatform
    chatId <- o Aeson..:? "chat_id"
    messageIds <- o Aeson..: "message_ids"
    pure [ThreadMessageKey{platform, chatId, messageId = textMessageId messageId} | messageId <- messageIds]

parsePlatform :: Text -> AesonTypes.Parser ChatPlatform
parsePlatform = \case
  "qq" -> pure PlatformQQ
  "telegram" -> pure PlatformTelegram
  "matrix" -> pure PlatformMatrix
  "discord" -> pure PlatformDiscord
  "rpc" -> pure PlatformRPC
  "acp" -> pure PlatformACP
  _ -> fail "platform must be one of: qq, telegram, matrix, discord, rpc, acp"
