{-|
Module      : Bot.JSONRPC
Description : Shared JSON-RPC envelope helpers
Stability   : experimental
-}

module Bot.JSONRPC
  ( JsonRpcRequest
  , JsonRpcResponse
  , JsonRpcError
  , JsonRpcNotification
  , RpcRequest
  , RpcResponse
  , RpcError
  , RpcNotification
  , RequestId
  , jsonRpcRequest
  , rpcRequest
  , requestId
  , requestMethod
  , requestParams
  , successResponse
  , errorResponse
  , parseErrorResponse
  , invalidRequestResponse
  , jsonRpcError
  , rpcError
  , notification
  )
where

import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified JSONRPC

type JsonRpcRequest = JSONRPC.JSONRPCRequest
type JsonRpcResponse = JSONRPC.JSONRPCMessage
type JsonRpcError = JSONRPC.JSONRPCErrorInfo
type JsonRpcNotification = JSONRPC.JSONRPCNotification
type RpcRequest = JsonRpcRequest
type RpcResponse = JsonRpcResponse
type RpcError = JsonRpcError
type RpcNotification = JsonRpcNotification
type RequestId = JSONRPC.RequestId

jsonRpcRequest :: Text -> Aeson.Value -> Text -> JsonRpcRequest
jsonRpcRequest method params requestId_ =
  JSONRPC.JSONRPCRequest JSONRPC.rPC_VERSION (textRequestId requestId_) method params

rpcRequest :: Text -> Aeson.Value -> Text -> RpcRequest
rpcRequest =
  jsonRpcRequest

requestId :: JsonRpcRequest -> RequestId
requestId request =
  request.id

requestMethod :: JsonRpcRequest -> Text
requestMethod request =
  request.method

requestParams :: JsonRpcRequest -> Aeson.Value
requestParams request =
  request.params

successResponse :: Aeson.ToJSON a => RequestId -> a -> JsonRpcResponse
successResponse responseId value =
  JSONRPC.ResponseMessage $
    JSONRPC.JSONRPCResponse JSONRPC.rPC_VERSION responseId (Aeson.toJSON value)

errorResponse :: RequestId -> Text -> Text -> JsonRpcResponse
errorResponse responseId code message =
  JSONRPC.ErrorMessage $
    JSONRPC.JSONRPCError JSONRPC.rPC_VERSION responseId (jsonRpcError code message)

parseErrorResponse :: Text -> JsonRpcResponse
parseErrorResponse message =
  JSONRPC.ErrorMessage $
    JSONRPC.JSONRPCError JSONRPC.rPC_VERSION nullRequestId $
      JSONRPC.JSONRPCErrorInfo JSONRPC.pARSE_ERROR "Parse error" (Just (Aeson.String message))

invalidRequestResponse :: Text -> JsonRpcResponse
invalidRequestResponse message =
  JSONRPC.ErrorMessage $
    JSONRPC.JSONRPCError JSONRPC.rPC_VERSION nullRequestId $
      JSONRPC.JSONRPCErrorInfo JSONRPC.iNVALID_REQUEST "Invalid request" (Just (Aeson.String message))

jsonRpcError :: Text -> Text -> JsonRpcError
jsonRpcError code message =
  JSONRPC.JSONRPCErrorInfo
    { JSONRPC.code = errorCodeNumber code
    , JSONRPC.message = message
    , JSONRPC.errorData =
        Just $
          Aeson.object
            [ "code" Aeson..= code
            ]
    }

rpcError :: Text -> Text -> RpcError
rpcError =
  jsonRpcError

notification :: Aeson.ToJSON a => Text -> a -> JsonRpcNotification
notification method value =
  JSONRPC.JSONRPCNotification JSONRPC.rPC_VERSION method (Aeson.toJSON value)

textRequestId :: Text -> RequestId
textRequestId =
  JSONRPC.RequestId . Aeson.String

nullRequestId :: RequestId
nullRequestId =
  JSONRPC.RequestId Aeson.Null

errorCodeNumber :: Text -> Int
errorCodeNumber = \case
  "invalid_json" -> JSONRPC.pARSE_ERROR
  "invalid_request" -> JSONRPC.iNVALID_REQUEST
  "method_not_found" -> JSONRPC.mETHOD_NOT_FOUND
  "invalid_params" -> JSONRPC.iNVALID_PARAMS
  _ -> JSONRPC.iNTERNAL_ERROR
