{-|
Module      : Bot.RPC.Client
Description : Command-line JSON-RPC websocket client
Stability   : experimental
-}

module Bot.RPC.Client
  ( RpcClientCommand (..)
  , RpcClientOptions (..)
  , runRpcClientCommand
  )
where

import Bot.Prelude
import qualified Bot.RPC.Config as RPCConfig
import qualified Bot.JSONRPC as RPC
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import qualified Network.WebSockets as WS
import qualified Toml
import Toml.Schema

data RpcClientCommand
  = RpcAuditRecent !Int
  | RpcAuditShow !Integer
  | RpcAuditThread !Text !(Maybe Integer) !Text
  | RpcMediaStats !Int
  | RpcMediaResolveSource !Text
  | RpcMediaGet !Text
  | RpcMediaDelete !Text
  | RpcMediaGc !Int
  | RpcCall !Text !Aeson.Value
  deriving (Eq, Show)

data RpcClientOptions = RpcClientOptions
  { configPath :: !FilePath
  , host :: !(Maybe String)
  , port :: !(Maybe Int)
  , token :: !(Maybe Text)
  }
  deriving (Eq, Show)

newtype RpcClientFileConfig = RpcClientFileConfig
  { rpc :: RPCConfig.FileConfig
  }
  deriving (Show)

instance FromValue RpcClientFileConfig where
  fromValue = parseTableFromValue $
    RpcClientFileConfig
      <$> fmap (fromMaybe RPCConfig.defaultFileConfig) (optKey "rpc")

runRpcClientCommand :: RpcClientOptions -> RpcClientCommand -> IO ()
runRpcClientCommand options command = do
  cfg <- applyRpcClientOptions options <$> loadRpcClientConfig options.configPath
  when (Text.null cfg.token) $
    fail "rpc.token must be configured for RPC client authentication"
  let request = requestForCommand command
  WS.runClientWith cfg.host cfg.port "/rpc" WS.defaultConnectionOptions (authHeaders cfg) \conn -> do
    WS.sendTextData conn (Aeson.encode (requestValue request))
    responseBytes <- WS.receiveData conn
    responseValue <- case Aeson.eitherDecodeStrict' responseBytes of
      Left err ->
        fail [i|RPC response was not valid JSON: #{err}|]
      Right value ->
        pure (value :: Aeson.Value)
    LazyByteString.putStr (AesonPretty.encodePretty responseValue <> "\n")

loadRpcClientConfig :: FilePath -> IO RPCConfig.Config
loadRpcClientConfig path = do
  content <- TextIO.readFile path
  case Toml.decode content of
    Toml.Failure errors ->
      fail [i|Failed to parse #{path}: #{unlines (map toText errors)}|]
    Toml.Success warnings config_ -> do
      traverse_ (putStrLn . ("TOML warning: " <>)) warnings
      pure (RPCConfig.toRuntimeConfig (config_ :: RpcClientFileConfig).rpc)

applyRpcClientOptions :: RpcClientOptions -> RPCConfig.Config -> RPCConfig.Config
applyRpcClientOptions options RPCConfig.Config{enabled, host, port, token} =
  RPCConfig.Config
    { enabled
    , host = fromMaybe host options.host
    , port = fromMaybe port options.port
    , token = fromMaybe token options.token
    }

requestForCommand :: RpcClientCommand -> RPC.RpcRequest
requestForCommand = \case
  RpcAuditRecent limit ->
    rpcRequest "audit.recent" (Aeson.object ["limit" Aeson..= limit])
  RpcAuditShow auditId ->
    rpcRequest "audit.get" (Aeson.object ["audit_id" Aeson..= auditId])
  RpcAuditThread platform chatId messageId ->
    rpcRequest "audit.thread" (Aeson.object
      [ "platform" Aeson..= platform
      , "chat_id" Aeson..= chatId
      , "message_id" Aeson..= messageId
      ])
  RpcMediaStats limit ->
    rpcRequest "media.stats" (Aeson.object ["limit" Aeson..= limit])
  RpcMediaResolveSource sourceRef ->
    rpcRequest "media.resolve_source" (Aeson.object ["sourceRef" Aeson..= sourceRef])
  RpcMediaGet mediaId ->
    rpcRequest "media.get" (Aeson.object ["mediaId" Aeson..= mediaId])
  RpcMediaDelete mediaId ->
    rpcRequest "media.delete" (Aeson.object ["mediaId" Aeson..= mediaId])
  RpcMediaGc maxAgeSeconds ->
    rpcRequest "media.gc" (Aeson.object ["maxAgeSeconds" Aeson..= maxAgeSeconds])
  RpcCall method params ->
    rpcRequest method params

rpcRequest :: Text -> Aeson.Value -> RPC.RpcRequest
rpcRequest method params =
  RPC.rpcRequest method params "cli-1"

requestValue :: RPC.RpcRequest -> Aeson.Value
requestValue =
  Aeson.toJSON

authHeaders :: RPCConfig.Config -> WS.Headers
authHeaders RPCConfig.Config{token} =
  [("Authorization", "Bearer " <> TextEncoding.encodeUtf8 token)]
