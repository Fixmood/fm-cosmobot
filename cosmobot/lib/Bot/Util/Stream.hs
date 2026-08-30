{-|
Module      : Bot.Util.Stream
Description : Small stream helpers
Stability   : experimental
-}

module Bot.Util.Stream
  ( bracketStream
  , mergeStreams
  , streamFinally
  )
where

import Bot.Prelude
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Effectful.Concurrent.STM as STM
import qualified Streaming as S
import qualified Streaming.Prelude as S

bracketStream
  :: Eff es resource
  -> (resource -> Eff es ())
  -> (resource -> Stream (Of item) (Eff es) result)
  -> Stream (Of item) (Eff es) result
bracketStream acquire release use = do
  resource <- S.lift acquire
  use resource `streamFinally` release resource

-- | Merge several streams into one stream through a bounded FIFO queue.
--
-- The @streaming-concurrency@ package provides @Streaming.Concurrent@ with a
-- similar @withMergedStreams@ helper, but its current Hackage release depends
-- on older @streaming-with@ / @streaming-bytestring@ bounds that do not solve
-- with this project's GHC 9.10 toolchain. Keep this small local version until
-- that dependency chain is usable here.
mergeStreams
  :: (KatipE :> es, Concurrency.Concurrency :> es, Concurrent :> es)
  => [Stream (Of a) (Eff es) ()]
  -> Stream (Of a) (Eff es) ()
mergeStreams streams = do
  queue <- S.lift (STM.newTBQueueIO streamQueueCapacity)
  pumps <- S.lift $
    traverse
      (\(index, stream) -> Concurrency.fork [i|stream.merge.#{index}|] (pump queue stream))
      (zip [(1 :: Int)..] streams)
  readMerged (length streams) queue `streamFinally` cleanupPumps pumps

streamQueueCapacity :: Natural
streamQueueCapacity =
  1024

data MergeEvent a
  = MergeItem !a
  | MergeDone
  | MergeFailed !SomeException

data MergeState = MergeState
  { remaining :: !Int
  , failures :: !Int
  , lastFailure :: !(Maybe SomeException)
  }

readMerged
  :: (KatipE :> es, Concurrent :> es)
  => Int
  -> STM.TBQueue (MergeEvent a)
  -> Stream (Of a) (Eff es) ()
readMerged streamCount queue =
  go MergeState{remaining = streamCount, failures = 0, lastFailure = Nothing}
  where
    go mergeState
      | mergeState.remaining <= 0 =
          finishMerged mergeState
      | otherwise = do
          event <- S.lift (STM.atomically (STM.readTBQueue queue))
          case event of
            MergeItem item -> do
              S.yield item
              go mergeState
            MergeDone ->
              go mergeState{remaining = mergeState.remaining - 1}
            MergeFailed err ->
              go mergeState
                { remaining = mergeState.remaining - 1
                , failures = mergeState.failures + 1
                , lastFailure = Just err
                }

    finishMerged mergeState
      | mergeState.failures == streamCount
      , Just err <- mergeState.lastFailure = do
          S.lift (logWarning [i|All merged stream inputs failed: #{show err :: String}|])
          S.lift (throwIO err)
      | otherwise =
          pure ()

streamFinally
  :: Stream (Of a) (Eff es) r
  -> Eff es ()
  -> Stream (Of a) (Eff es) r
streamFinally stream cleanup =
  go stream
  where
    go current = do
      next <- S.lift (S.next current `onException` cleanup)
      case next of
        Left result -> do
          S.lift cleanup
          pure result
        Right (item, rest) -> do
          S.yield item
          go rest

cleanupPumps :: Concurrency.Concurrency :> es => [Concurrency.Handle] -> Eff es ()
cleanupPumps pumps =
  traverse_ cancelAndAwaitPump pumps

cancelAndAwaitPump :: Concurrency.Concurrency :> es => Concurrency.Handle -> Eff es ()
cancelAndAwaitPump pumpHandle = do
  void (Concurrency.cancel pumpHandle.handleId)
  Concurrency.await pumpHandle

pump
  :: (KatipE :> es, Concurrent :> es)
  => STM.TBQueue (MergeEvent a)
  -> Stream (Of a) (Eff es) ()
  -> Eff es ()
pump queue stream =
  (S.mapM_ (writeMergeEvent queue . MergeItem) stream *> writeMergeEvent queue MergeDone)
    `catchSync` \err -> do
      logError [i|Merged stream input stopped: #{show err :: String}|]
      writeMergeEvent queue (MergeFailed err)

writeMergeEvent :: Concurrent :> es => STM.TBQueue (MergeEvent a) -> MergeEvent a -> Eff es ()
writeMergeEvent queue =
  STM.atomically . STM.writeTBQueue queue
