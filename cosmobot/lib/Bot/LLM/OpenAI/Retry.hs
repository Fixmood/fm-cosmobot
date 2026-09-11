{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.LLM.OpenAI.Retry
Description : LLM retry and response validation policy
Stability   : experimental
-}

module Bot.LLM.OpenAI.Retry
  ( retryLLMStreamRequest
  , retryLLMStreamRequestWith
  )
where

import Bot.Prelude
import qualified Bot.LLM.Types as LLM
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.List as List
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as HTTPHeader
import qualified Network.HTTP.Types.Status as HTTPStatus
import qualified Streaming.Prelude as S
import GHC.Clock (getMonotonicTimeNSec)

maxLLMRetries :: Int
maxLLMRetries =
  2

retryLLMStreamRequest
  :: (Concurrent :> es, IOE :> es, KatipE :> es)
  => Text
  -> Stream (Of a) (Eff es) r
  -> Stream (Of a) (Eff es) r
retryLLMStreamRequest =
  retryLLMStreamRequestWith \seconds ->
    threadDelay (seconds * 1_000_000)

retryLLMStreamRequestWith
  :: (IOE :> es, KatipE :> es)
  => (Int -> Eff es ())
  -> Text
  -> Stream (Of a) (Eff es) r
  -> Stream (Of a) (Eff es) r
retryLLMStreamRequestWith sleep label source =
  go 0
  where
    go retries =
      do
        startedAt <- lift monotonicMilliseconds
        consume startedAt retries False source

    consume startedAt retries yielded stream = do
      next <- lift (trySync (S.next stream))
      case next of
        Left err
          | not yielded
          , retries < maxLLMRetries
          , retryableHTTPFailure err -> do
              let nextRetry = retries + 1
                  delaySeconds = retryDelaySeconds nextRetry err
              lift do
                finishedAt <- monotonicMilliseconds
                logWarning [i|#{label} failed with #{LLM.llmExceptionSummary err}; retrying attempt #{nextRetry + 1}/#{maxLLMRetries + 1} after #{delaySeconds}s elapsed_ms=#{finishedAt - startedAt}|]
                sleep delaySeconds
              go nextRetry
          | otherwise ->
              lift (throwIO err)
        Right (Left result) ->
          pure result
        Right (Right (chunk, rest)) -> do
          S.yield chunk
          consume startedAt retries True rest

monotonicMilliseconds :: IOE :> es => Eff es Integer
monotonicMilliseconds =
  fromIntegral . (`div` 1_000_000) <$> liftIO getMonotonicTimeNSec

retryableHTTPFailure :: SomeException -> Bool
retryableHTTPFailure err =
  case fromException err of
    Just (HTTP.HttpExceptionRequest _ content) ->
      retryableHTTPContent content
    _ ->
      False

retryableHTTPContent :: HTTP.HttpExceptionContent -> Bool
retryableHTTPContent = \case
  HTTP.StatusCodeException response _ ->
    HTTPStatus.statusCode (HTTP.responseStatus response) `elem` [408, 425, 429, 500, 502, 503, 504]
  HTTP.ResponseTimeout ->
    True
  HTTP.ConnectionTimeout ->
    True
  HTTP.ConnectionFailure{} ->
    True
  HTTP.NoResponseDataReceived ->
    True
  HTTP.IncompleteHeaders ->
    True
  HTTP.ConnectionClosed ->
    True
  HTTP.ResponseBodyTooShort{} ->
    True
  HTTP.InternalException inner ->
    retryableInternalException inner
  _ ->
    False

retryableInternalException :: SomeException -> Bool
retryableInternalException err =
  let rendered = show err :: String
  in any (`List.isInfixOf` rendered)
      [ "HandshakeFailed", "Error_EOF", "Connection reset"
      , "Connection refused", "broken pipe", "resource vanished"
      ]

retryDelaySeconds :: Int -> SomeException -> Int
retryDelaySeconds retryNumber err =
  max 1 (min 2 (fromMaybe retryNumber (retryAfterSeconds err)))

retryAfterSeconds :: SomeException -> Maybe Int
retryAfterSeconds err =
  case fromException err of
    Just (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException response _)) ->
      List.lookup HTTPHeader.hRetryAfter (HTTP.responseHeaders response) >>= parseDeltaSeconds
    _ ->
      Nothing

parseDeltaSeconds :: ByteString.ByteString -> Maybe Int
parseDeltaSeconds raw =
  case ByteString.readInt raw of
    Just (seconds, rest)
      | seconds >= 0
      , ByteString.null rest ->
          Just seconds
    _ ->
      Nothing
