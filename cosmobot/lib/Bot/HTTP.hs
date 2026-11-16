{-|
Module      : Bot.HTTP
Description : Shared HTTP client interpreter
Stability   : experimental
-}

module Bot.HTTP
  ( runHTTP
  , httpsEndpointUrl
  , streamingJsonPostRequest
  )
where

import Bot.Prelude
import qualified Bot.Effect.HTTP as HTTP
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text
import Network.Connection (TLSSettings (..))
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as ClientTLS
import Network.HTTP.Req (HttpConfig (..), Option, Req, Url, useHttpsURI, (/:))
import qualified Network.HTTP.Req as Req
import qualified Network.TLS as TLS
import System.IO.Error (ioError, userError)
import qualified Text.URI as URI

runHTTP :: IOE :> es => Eff (HTTP.HTTP : es) a -> Eff es a
runHTTP inner = do
  sharedManager <- liftIO newTlsManager
  directManager <- liftIO newDirectTlsManager
  interpret
    ( \_ -> \case
        HTTP.Manager ->
          pure sharedManager
        HTTP.RunReq action ->
          liftIO $ runReqWithConfigIO (httpConfig sharedManager) action
        HTTP.RunReqWithConfig config action ->
          liftIO $ runReqWithConfigIO (withSharedManager sharedManager config) action
        HTTP.OpenResponse request ->
          liftIO $ Client.responseOpen request
            (if isDirectLLMRequest request then directManager else sharedManager)
    )
    inner

withSharedManager :: Client.Manager -> HttpConfig -> HttpConfig
withSharedManager sharedManager config =
  config
    { httpConfigAltManager = Just sharedManager
    }

httpsEndpointUrl :: Text -> [Text] -> IO (Url 'Req.Https, Option 'Req.Https)
httpsEndpointUrl endpoint path = do
  uri <- URI.mkURI endpoint
  case useHttpsURI uri of
    Nothing ->
      ioError (userError [i|Unsupported HTTPS endpoint URL: #{endpoint}. Use a full HTTPS base URL.|])
    Just (url, options) ->
      pure (foldl' (/:) url path, options)

-- 2026-11-16：内网直连、外网走代理的分流（容器→宿主 7890 已被 ufw 放行）。
internalHosts :: [ByteString.ByteString]
internalHosts =
  map
    ByteString.pack
    [ "fm-domain", "fm-snowluma", "fm-admin", "localhost", "127.0.0.1", "::1" ]

isInternalHost :: ByteString.ByteString -> Bool
isInternalHost host =
  host `elem` internalHosts
    || ByteString.pack "172.20." `ByteString.isPrefixOf` host
    || ByteString.null host

newTlsManager :: IO Client.Manager
newTlsManager =
  -- 内网直连；其它（github/google/外网 API）走 172.20.0.1:7890 代理。
  -- 响应头上限从默认 4096 提高到 64KB：x.com 之类站点首页的响应头远超标，
  -- 默认设置会直接抛 OverlongHeaders（2026-11-16 实测）。
  ClientTLS.newTlsManagerWith $
    Client.managerSetMaxNumberHeaders 1000
      . Client.managerSetMaxHeaderLength 65536
      $ ClientTLS.tlsManagerSettings
        { Client.managerConnCount = sharedManagerConnectionCount
        , Client.managerModifyRequest = \request ->
            pure $
              if isInternalHost (Client.host request)
                then request{Client.proxy = Nothing}
                else request{Client.proxy = Just (Client.Proxy (ByteString.pack "172.20.0.1") 7890)}
        }

-- DeepSeek is reachable directly from the server, while the configured
-- proxy is currently unreliable for its streaming endpoint. Keep Matrix and
-- other shared HTTP traffic on the existing proxy manager.
newDirectTlsManager :: IO Client.Manager
newDirectTlsManager =
  ClientTLS.newTlsManagerWith $
    Client.managerSetMaxNumberHeaders 1000
      . Client.managerSetMaxHeaderLength 65536
      $ ClientTLS.tlsManagerSettings
        { Client.managerConnCount = sharedManagerConnectionCount
        }

isDirectLLMRequest :: Client.Request -> Bool
isDirectLLMRequest request =
  Client.host request `elem`
    map ByteString.pack
      [ "api.deepseek.com"
      , "weilai.uk"
      , "botcf.com"
      ]

sharedManagerConnectionCount :: Int
sharedManagerConnectionCount =
  64

httpConfig :: Client.Manager -> HttpConfig
httpConfig sharedManager =
  Req.defaultHttpConfig
    { httpConfigAltManager = Just sharedManager
    }

runReqWithConfigIO :: HttpConfig -> Req a -> IO a
runReqWithConfigIO =
  Req.runReq

tlsSettings :: TLSSettings
tlsSettings =
  TLSSettingsSimple
    { settingDisableCertificateValidation = False
    , settingDisableSession = False
    , settingUseServerName = True
    , settingClientSupported =
        TLS.defaultSupported
          { TLS.supportedExtendedMainSecret = TLS.AllowEMS
          }
    }

streamingJsonPostRequest :: Aeson.ToJSON body => Text -> [Text] -> Text -> Int -> body -> IO Client.Request
streamingJsonPostRequest endpoint path apiKey timeoutMicros body = do
  base <- Client.parseRequest (Text.unpack (endpointText endpoint path))
  pure base
    { Client.method = "POST"
    , Client.requestHeaders =
        [ ("Authorization", ByteString.pack [i|Bearer #{apiKey}|])
        , ("Content-Type", "application/json")
        ]
    , Client.requestBody = Client.RequestBodyLBS (Aeson.encode body)
    , Client.responseTimeout = Client.responseTimeoutMicro timeoutMicros
    }

endpointText :: Text -> [Text] -> Text
endpointText endpoint path =
  case path of
    [] -> Text.dropWhileEnd (== '/') endpoint
    _  -> Text.dropWhileEnd (== '/') endpoint <> "/" <> Text.intercalate "/" path
