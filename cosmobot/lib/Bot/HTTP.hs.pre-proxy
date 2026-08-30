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
  interpret
    ( \_ -> \case
        HTTP.Manager ->
          pure sharedManager
        HTTP.RunReq action ->
          liftIO $ runReqWithConfigIO (httpConfig sharedManager) action
        HTTP.RunReqWithConfig config action ->
          liftIO $ runReqWithConfigIO (withSharedManager sharedManager config) action
        HTTP.OpenResponse request ->
          liftIO $ Client.responseOpen request sharedManager
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

newTlsManager :: IO Client.Manager
newTlsManager =
  ClientTLS.newTlsManagerWith
    (ClientTLS.mkManagerSettings tlsSettings Nothing)
      { Client.managerConnCount = sharedManagerConnectionCount
      }

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
