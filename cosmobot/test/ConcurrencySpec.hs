{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Bot.Concurrency.Manager
import qualified Bot.Effect.Concurrency as Concurrency
import Bot.Prelude
import qualified Effectful.Concurrent.MVar as MVar
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

data ManagerAbort = ManagerAbort
  deriving stock (Show)

instance Exception ManagerAbort

main :: IO ()
main =
  defaultMain $
    testGroup
      "concurrency"
      [ testCase "normal manager exit cancels and awaits running tasks" testNormalExitCancelsAndAwaits
      , testCase "top exception is thrown into running tasks" testTopExceptionPropagates
      , testCase "cancel then await returns after task cleanup" testCancelThenAwait
      , testCase "awaitAny returns the first completion without cancelling others" testAwaitAny
      ]

testNormalExitCancelsAndAwaits :: Assertion
testNormalExitCancelsAndAwaits = do
  result <- timeout 1_000_000 $ runManaged do
    started <- MVar.newEmptyMVar
    stopped <- MVar.newEmptyMVar
    runConcurrencyManager do
      void $
        Concurrency.fork "worker" $
          (MVar.putMVar started () >> never) `finally` MVar.putMVar stopped ()
      MVar.takeMVar started
    MVar.takeMVar stopped
  result @?= Just ()

testTopExceptionPropagates :: Assertion
testTopExceptionPropagates = do
  result <- timeout 1_000_000 $ runManaged do
    started <- MVar.newEmptyMVar
    observed <- MVar.newEmptyMVar
    outcome <- try $ runConcurrencyManager do
      void $
        Concurrency.fork "worker" do
          MVar.putMVar started ()
          never `catch` \(err :: SomeException) ->
            MVar.putMVar observed (isJust (fromException err :: Maybe ManagerAbort))
      MVar.takeMVar started
      throwIO ManagerAbort
    workerSawAbort <- MVar.takeMVar observed
    pure (isLeft (outcome :: Either SomeException ()) && workerSawAbort)
  result @?= Just True

testCancelThenAwait :: Assertion
testCancelThenAwait = do
  result <- timeout 1_000_000 $ runManaged do
    started <- MVar.newEmptyMVar
    cleaned <- MVar.newEmptyMVar
    runConcurrencyManager do
      worker <- Concurrency.fork "worker" $
        (MVar.putMVar started () >> never) `finally` MVar.putMVar cleaned ()
      MVar.takeMVar started
      cancelled <- Concurrency.cancel worker.handleId
      Concurrency.await worker
      MVar.takeMVar cleaned
      pure cancelled
  result @?= Just True

testAwaitAny :: Assertion
testAwaitAny = do
  result <- timeout 1_000_000 $ runManaged do
    firstGate <- MVar.newEmptyMVar
    secondGate <- MVar.newEmptyMVar
    runConcurrencyManager do
      firstWorker <- Concurrency.fork "first" (MVar.takeMVar firstGate)
      secondWorker <- Concurrency.fork "second" (MVar.takeMVar secondGate)
      MVar.putMVar secondGate ()
      winner <- Concurrency.awaitAny (firstWorker :| [secondWorker])
      firstStatus <- Concurrency.lookup firstWorker.handleId
      MVar.putMVar firstGate ()
      pure (winner, (.status) <$> firstStatus)
  result @?= Just (Concurrency.Handle (Concurrency.Id 2), Just Concurrency.Running)

runManaged :: Eff '[Prim, Concurrent, IOE] a -> IO a
runManaged =
  runEff . runConcurrent . runPrim

never :: Concurrent :> es => Eff es ()
never =
  threadDelay maxBound
