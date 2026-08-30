module Main (main) where

import Bot.Prelude
import qualified Bot.LLM.OpenAI.Retry as Retry
import qualified Data.ByteString as ByteString
import qualified Data.IORef as IORef
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.Internal as HTTPInternal
import qualified Network.HTTP.Types.Header as HTTPHeader
import qualified Network.HTTP.Types.Status as HTTPStatus
import qualified Network.HTTP.Types.Version as HTTPVersion
import qualified Streaming.Prelude as S
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "llm retry"
      [ testCase "retries transient failures with exponential backoff" testExponentialBackoff
      , testCase "honors Retry-After when longer than backoff" testRetryAfter
      , testCase "retries configured transient HTTP failures" testRetryableFailures
      , testCase "does not retry permanent HTTP failures" testPermanentFailures
      , testCase "does not retry after yielding output" testNoRetryAfterOutput
      ]

testExponentialBackoff :: IO ()
testExponentialBackoff = do
  attempts <- IORef.newIORef 0
  delays <- IORef.newIORef []
  result <- runRetry delays (flakyStream attempts 3 (httpException HTTP.ResponseTimeout))
  assertRight result
  IORef.readIORef attempts >>= (@?= 4)
  IORef.readIORef delays >>= (@?= [2, 4, 8])

testRetryAfter :: IO ()
testRetryAfter = do
  attempts <- IORef.newIORef 0
  delays <- IORef.newIORef []
  result <- runRetry delays (flakyStream attempts 1 (statusException 429 [("Retry-After", "10")]))
  assertRight result
  IORef.readIORef delays >>= (@?= [10])

testRetryableFailures :: IO ()
testRetryableFailures =
  for_ retryableExceptions \err -> do
    attempts <- IORef.newIORef 0
    delays <- IORef.newIORef []
    result <- runRetry delays (flakyStream attempts 1 err)
    assertRight result
    IORef.readIORef attempts >>= (@?= 2)
  where
    retryableExceptions =
      map (`statusException` []) [408, 425, 429, 500, 502, 503, 504]
        <> map httpException
          [ HTTP.ResponseTimeout
          , HTTP.ConnectionTimeout
          , HTTP.ConnectionFailure (toException (HTTP.InvalidUrlException "" "connection failed"))
          , HTTP.NoResponseDataReceived
          , HTTP.IncompleteHeaders
          , HTTP.ConnectionClosed
          , HTTP.ResponseBodyTooShort 1 0
          ]

testPermanentFailures :: IO ()
testPermanentFailures =
  for_ [statusException 400 [], httpException (HTTP.InvalidStatusLine "bad")] \err -> do
    attempts <- IORef.newIORef 0
    delays <- IORef.newIORef []
    result <- runRetry delays (flakyStream attempts 1 err)
    assertBool "expected retry wrapper to preserve permanent failure" (isLeft result)
    IORef.readIORef attempts >>= (@?= 1)
    IORef.readIORef delays >>= (@?= [])

testNoRetryAfterOutput :: IO ()
testNoRetryAfterOutput = do
  attempts <- IORef.newIORef (0 :: Int)
  delays <- IORef.newIORef []
  let stream = do
        liftIO $ IORef.modifyIORef' attempts (+ 1)
        S.yield ("partial" :: Text)
        lift $ throwIO (httpException HTTP.ConnectionClosed)
  result <- runRetry delays stream
  assertBool "expected failure after partial output" (isLeft result)
  IORef.readIORef attempts >>= (@?= 1)
  IORef.readIORef delays >>= (@?= [])

runRetry
  :: IORef.IORef [Int]
  -> Stream (Of Text) (Eff '[KatipE, IOE]) ()
  -> IO (Either SomeException ())
runRetry delays stream =
  runEff $
    startKatipE "llm-retry-spec" "test" $
      trySync $
        void $
          S.toList_ $
          Retry.retryLLMStreamRequestWith
            (\seconds -> liftIO $ IORef.modifyIORef' delays (<> [seconds]))
            "test request"
            stream

flakyStream
  :: IORef.IORef Int
  -> Int
  -> SomeException
  -> Stream (Of Text) (Eff '[KatipE, IOE]) ()
flakyStream attempts failures err = do
  attempt <- liftIO $ IORef.atomicModifyIORef' attempts \current ->
    let next = current + 1
    in (next, next)
  when (attempt <= failures) (lift $ throwIO err)

assertRight :: Show err => Either err a -> Assertion
assertRight = \case
  Left err ->
    assertFailure ("expected retry to succeed, got " <> show err)
  Right{} ->
    pure ()

httpException :: HTTP.HttpExceptionContent -> SomeException
httpException =
  toException . HTTP.HttpExceptionRequest HTTP.defaultRequest

statusException :: Int -> HTTPHeader.ResponseHeaders -> SomeException
statusException code headers =
  httpException (HTTP.StatusCodeException response ByteString.empty)
  where
    response = HTTPInternal.Response
      { HTTPInternal.responseStatus = HTTPStatus.mkStatus code ""
      , HTTPInternal.responseVersion = HTTPVersion.http11
      , HTTPInternal.responseHeaders = headers
      , HTTPInternal.responseBody = ()
      , HTTPInternal.responseCookieJar = HTTP.createCookieJar []
      , HTTPInternal.responseClose' = HTTPInternal.ResponseClose (pure ())
      , HTTPInternal.responseOriginalRequest = HTTP.defaultRequest
      , HTTPInternal.responseEarlyHints = []
      }
