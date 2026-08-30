module Bot.Agent.Tools.Matrix
  ( matrixRequestTool
  )
where

import Bot.Agent.Failure (externalServiceFailure)
import Bot.Agent.Tool
import Bot.Agent.Tools.Common
import Bot.Agent.Types
import Bot.Core.Message (ChatPlatform (PlatformMatrix), IncomingMessage (..))
import qualified Bot.Effect.Matrix as Matrix
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes

matrixRequestTool :: Matrix.Matrix :> es => Tool (Eff es)
matrixRequestTool =
  tagged [chatTag]
  . allowWhen matrixSuperuser
  . withDescription "Call an authenticated Matrix Client-Server JSON endpoint. Only use Matrix client or media paths; credentials and headers are managed by the bot."
  $ tool "matrix_request"
      (parsedArguments (objectSchema
        [ fieldText "method" "One of: GET, POST, PUT, DELETE."
        , fieldTextArray "path" "Decoded Matrix URL path segments; must start with _matrix/client or _matrix/media."
        , ("query", Aeson.object ["type" Aeson..= ("object" :: Text), "additionalProperties" Aeson..= Aeson.object ["type" Aeson..= ("string" :: Text)]])
        , ("body", Aeson.object [])
        ] ["method", "path"])
        parseRequest)
      \request -> do
        context <- askToolContext
        if matrixSuperuser context
          then do
            result <- trySync (Matrix.matrixClientCall request)
            pure $ either (toolFailure . externalServiceFailure "Matrix request failed." . toText . displayException) (toolText . jsonText) result
          else pure (toolFailure (permissionDeniedFailure "matrix_request requires a Matrix superuser." "matrix_request was called outside a Matrix superuser context."))

matrixSuperuser :: Context -> Bool
matrixSuperuser context =
  superuserOnly context && context.message.platform == PlatformMatrix

parseRequest :: Aeson.Value -> AesonTypes.Parser Matrix.MatrixClientRequest
parseRequest = Aeson.withObject "matrix_request arguments" \object -> do
  methodText <- object Aeson..: Key.fromText "method"
  requestMethod <- case methodText :: Text of
    "GET" -> pure Matrix.MatrixGet
    "POST" -> pure Matrix.MatrixPost
    "PUT" -> pure Matrix.MatrixPut
    "DELETE" -> pure Matrix.MatrixDelete
    _ -> fail "method must be one of: GET, POST, PUT, DELETE."
  requestPath <- object Aeson..: Key.fromText "path"
  unless (isMatrixPath requestPath) $ fail "path must start with _matrix/client or _matrix/media."
  requestQuery <- (object Aeson..:? Key.fromText "query") >>= maybe (pure []) queryPairs
  requestBody <- object Aeson..:? Key.fromText "body"
  case (requestMethod, requestBody) of
    (Matrix.MatrixPost, Just{}) -> pure ()
    (Matrix.MatrixPut, Just{}) -> pure ()
    (Matrix.MatrixPost, Nothing) -> fail "body is required for POST."
    (Matrix.MatrixPut, Nothing) -> fail "body is required for PUT."
    (_, Nothing) -> pure ()
    _ -> fail "body is only accepted for POST and PUT."
  pure Matrix.MatrixClientRequest{method = requestMethod, path = requestPath, query = requestQuery, body = requestBody}

isMatrixPath :: [Text] -> Bool
isMatrixPath = \case
  "_matrix" : "client" : _ : _ -> True
  "_matrix" : "media" : _ : _ -> True
  _ -> False

queryPairs :: AesonTypes.Object -> AesonTypes.Parser [(Text, Text)]
queryPairs = traverse pair . KeyMap.toList
  where
    pair (key, value) = (Key.toText key,) <$> Aeson.parseJSON value
