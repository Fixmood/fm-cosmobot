{-|
Module      : Bot.Effect.Concurrency
Description : Queryable concurrency capability facade
Stability   : experimental
-}

module Bot.Effect.Concurrency
  ( Concurrency (..)
  , Id (..)
  , Status (..)
  , Info (..)
  , Handle (..)
  , Snapshot (..)
  , finished
  , fire
  , fireWithHandle
  , fork
  , forkWithHandle
  , withWorker
  , raceTasks_
  , cancel
  , await
  , awaitAny
  , sleepMicroseconds
  , list
  , lookup
  )
where

import Bot.Prelude hiding (Handle)
import Data.Time (UTCTime)
import qualified Effectful.Concurrent.MVar as MVar

newtype Id = Id
  { unId :: Integer
  }
  deriving stock (Eq, Ord, Show)

data Status
  = Running
  | Completed
  | Failed !Text
  | Cancelled
  deriving stock (Eq, Show)

data Info = Info
  { id :: !Id
  , label :: !Text
  , status :: !Status
  , startedAt :: !UTCTime
  , finishedAt :: !(Maybe UTCTime)
  }
  deriving stock (Eq, Show)

newtype Handle = Handle
  { handleId :: Id
  }
  deriving stock (Eq, Ord, Show)

data Snapshot = Snapshot
  { entries :: ![Info]
  }
  deriving stock (Eq, Show)

finished :: Info -> Bool
finished info =
  isJust info.finishedAt

fire
  :: Concurrency :> es
  => Text
  -> Eff es ()
  -> Eff es ()
fire label =
  void . fork label

fireWithHandle
  :: Concurrency :> es
  => Text
  -> (Handle -> Eff es ())
  -> Eff es ()
fireWithHandle label =
  void . forkWithHandle label

fork
  :: Concurrency :> es
  => Text
  -> Eff es ()
  -> Eff es Handle
fork label action =
  send (Fork label action)

forkWithHandle
  :: Concurrency :> es
  => Text
  -> (Handle -> Eff es ())
  -> Eff es Handle
forkWithHandle label action =
  send (ForkWithHandle label action)

withWorker
  :: Concurrency :> es
  => Text
  -> Eff es ()
  -> Eff es a
  -> Eff es a
withWorker label worker inner = do
  workerHandle <- fork label worker
  inner `finally` cancelAndAwait workerHandle

raceTasks_
  :: (Concurrency :> es, Concurrent :> es, IOE :> es)
  => Text
  -> Eff es ()
  -> Text
  -> Eff es ()
  -> Eff es ()
raceTasks_ leftLabel leftAction rightLabel rightAction = do
  done <- MVar.newEmptyMVar
  left <- fork leftLabel (capture done leftAction)
  right <- fork rightLabel (capture done rightAction)
  let cancelBoth = do
        cancelAndAwait left
        cancelAndAwait right
  result <- MVar.takeMVar done `finally` cancelBoth
  either throwIO pure result
  where
    capture
      :: (Concurrent :> es, IOE :> es)
      => MVar.MVar (Either SomeException ())
      -> Eff es ()
      -> Eff es ()
    capture done action =
      try action >>= void . MVar.tryPutMVar done

cancelAndAwait :: Concurrency :> es => Handle -> Eff es ()
cancelAndAwait workerHandle = do
  void (cancel workerHandle.handleId)
  await workerHandle

data Concurrency :: Effect where
  Fork :: Text -> m () -> Concurrency m Handle
  ForkWithHandle :: Text -> (Handle -> m ()) -> Concurrency m Handle
  Cancel :: Id -> Concurrency m Bool
  Await :: Handle -> Concurrency m ()
  AwaitAny :: NonEmpty Handle -> Concurrency m Handle
  SleepMicroseconds :: Int -> Concurrency m ()
  List :: Concurrency m Snapshot
  Lookup :: Id -> Concurrency m (Maybe Info)

type instance DispatchOf Concurrency = Dynamic

cancel :: Concurrency :> es => Id -> Eff es Bool
cancel =
  send . Cancel

await :: Concurrency :> es => Handle -> Eff es ()
await =
  send . Await

awaitAny :: Concurrency :> es => NonEmpty Handle -> Eff es Handle
awaitAny =
  send . AwaitAny

sleepMicroseconds :: Concurrency :> es => Int -> Eff es ()
sleepMicroseconds =
  send . SleepMicroseconds

list :: Concurrency :> es => Eff es Snapshot
list =
  send List

lookup :: Concurrency :> es => Id -> Eff es (Maybe Info)
lookup =
  send . Lookup
