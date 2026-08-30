{-|
Module      : Bot.Concurrency.Manager
Description : Queryable ownership model for concurrent work
Stability   : experimental
-}

module Bot.Concurrency.Manager
  ( runConcurrencyManager
  )
where

import qualified Bot.Effect.Concurrency as Concurrency
import Bot.Effect.Concurrency
import Bot.Prelude hiding (Handle)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Effectful.Concurrent.Async as Async
import Data.Time (getCurrentTime)
import qualified Effectful.Concurrent.MVar as MVar

data ManagerState = ManagerState
  { nextIdRef :: !(IORef Id)
  , runtimes :: !(IORef (Map Id EntryRuntime))
  }

data EntryRuntime = EntryRuntime
  { info :: !Info
  , thread :: !(Async.Async ())
  }

runConcurrencyManager
  :: (IOE :> es, Prim :> es, Concurrent :> es)
  => Eff (Concurrency : es) a
  -> Eff es a
runConcurrencyManager inner = do
  nextIdRef <- newIORef (Id 1)
  runtimes <- newIORef Map.empty
  let managerState = ManagerState{nextIdRef, runtimes}
      runInner = interpret (runConcurrencyOperation managerState) inner
  try runInner >>= \case
    Right result -> do
      cancelAndAwaitAll managerState
      pure result
    Left err -> do
      cancelAndAwaitAllWith managerState err
      throwIO (err :: SomeException)

runConcurrencyOperation
  :: (IOE :> es, Prim :> es, Concurrent :> es)
  => ManagerState
  -> EffectHandler Concurrency es
runConcurrencyOperation managerState localEnv operation =
  case operation of
    Concurrency.Fork label action ->
      localUnlift localEnv managedActionUnlift \unlift ->
        forkIn managerState label (unlift action)
    Concurrency.ForkWithHandle label action ->
      localUnlift localEnv managedActionUnlift \unlift ->
        forkWithHandleIn managerState label (unlift . action)
    Concurrency.Cancel handleId ->
      cancelIn managerState handleId
    Concurrency.Await workerHandle ->
      awaitIn managerState workerHandle
    Concurrency.AwaitAny workerHandles ->
      awaitAnyIn managerState workerHandles
    Concurrency.SleepMicroseconds microseconds ->
      threadDelay microseconds
    Concurrency.List ->
      listIn managerState
    Concurrency.Lookup handleId ->
      lookupIn managerState handleId

managedActionUnlift :: UnliftStrategy
managedActionUnlift =
  ConcUnlift Persistent (Limited 1)

forkIn
  :: (IOE :> es, Prim :> es, Concurrent :> es)
  => ManagerState
  -> Text
  -> Eff es ()
  -> Eff es Handle
forkIn managerState label action =
  forkWithHandleIn managerState label (const action)

forkWithHandleIn
  :: (IOE :> es, Prim :> es, Concurrent :> es)
  => ManagerState
  -> Text
  -> (Handle -> Eff es ())
  -> Eff es Handle
forkWithHandleIn managerState label action = mask \restore -> do
  entryInfo <- newInfo managerState label
  let workerHandle = Handle{handleId = entryInfo.id}
  startGate <- MVar.newEmptyMVar
  thread <- Async.async $
    restore $
      MVar.takeMVar startGate
        *> runAction managerState entryInfo.id (action workerHandle)
  let runtime = EntryRuntime
        { info = entryInfo
        , thread
        }
  (insertRuntime managerState runtime >> MVar.putMVar startGate ())
    `onException` Async.cancel thread
  pure workerHandle

runAction
  :: (IOE :> es, Prim :> es)
  => ManagerState
  -> Id
  -> Eff es ()
  -> Eff es ()
runAction managerState handleId action =
  trySync action >>= \case
    Right () ->
      finishEntry managerState handleId Completed
    Left err ->
      finishEntry managerState handleId (Failed (Text.pack (show err)))

cancelIn
  :: (IOE :> es, Prim :> es, Concurrent :> es)
  => ManagerState
  -> Id
  -> Eff es Bool
cancelIn managerState handleId = do
  runtime <- lookupRuntime managerState handleId
  case runtime of
    Nothing ->
      pure False
    Just entry
      | finished entry.info ->
          pure False
      | otherwise -> do
          finishEntry managerState handleId Cancelled
          Async.cancel entry.thread
          pure True

awaitIn
  :: (IOE :> es, Prim :> es, Concurrent :> es)
  => ManagerState
  -> Handle
  -> Eff es ()
awaitIn managerState workerHandle =
  liftMaybeThread managerState workerHandle.handleId >>= \case
    Nothing ->
      pure ()
    Just thread ->
      void (Async.waitCatch thread)

awaitAnyIn
  :: (IOE :> es, Prim :> es, Concurrent :> es)
  => ManagerState
  -> NonEmpty Handle
  -> Eff es Handle
awaitAnyIn managerState workerHandles = do
  runtimes <- readIORef managerState.runtimes
  case find (\worker -> Map.notMember worker.handleId runtimes) workerHandles of
    Just worker ->
      pure worker
    Nothing -> do
      let workers = [(worker, (runtimes Map.! worker.handleId).thread) | worker <- toList workerHandles]
      void (Async.waitAny (map snd workers))
      firstCompleted workers
  where
    firstCompleted ((worker, thread) : remaining) =
      Async.poll thread >>= \case
        Just _ -> pure worker
        Nothing -> firstCompleted remaining
    firstCompleted [] =
      error "awaitAny returned without a completed task"

listIn :: Prim :> es => ManagerState -> Eff es Snapshot
listIn managerState =
  Snapshot . map (.info) . Map.elems <$> readIORef managerState.runtimes

lookupIn :: Prim :> es => ManagerState -> Id -> Eff es (Maybe Info)
lookupIn managerState handleId =
  fmap (.info) . Map.lookup handleId <$> readIORef managerState.runtimes

newInfo
  :: (IOE :> es, Prim :> es)
  => ManagerState
  -> Text
  -> Eff es Info
newInfo managerState label = do
  handleId <- allocateId managerState
  startedAt <- liftIO getCurrentTime
  pure Info
    { id = handleId
    , label
    , status = Running
    , startedAt
    , finishedAt = Nothing
    }

allocateId :: Prim :> es => ManagerState -> Eff es Id
allocateId managerState =
  atomicModifyIORef' managerState.nextIdRef \(Id current) ->
    (Id (current + 1), Id current)

insertRuntime :: Prim :> es => ManagerState -> EntryRuntime -> Eff es ()
insertRuntime managerState runtime =
  atomicModifyIORef' managerState.runtimes \runtimes ->
    (Map.insert runtime.info.id runtime runtimes, ())

lookupRuntime :: Prim :> es => ManagerState -> Id -> Eff es (Maybe EntryRuntime)
lookupRuntime managerState handleId =
  Map.lookup handleId <$> readIORef managerState.runtimes

liftMaybeThread :: Prim :> es => ManagerState -> Id -> Eff es (Maybe (Async.Async ()))
liftMaybeThread managerState handleId = do
  runtime <- lookupRuntime managerState handleId
  pure ((.thread) <$> runtime)

finishEntry
  :: (IOE :> es, Prim :> es)
  => ManagerState
  -> Id
  -> Status
  -> Eff es ()
finishEntry managerState handleId status = do
  finishedAt <- liftIO getCurrentTime
  atomicModifyIORef' managerState.runtimes \runtimes ->
    let update runtime =
          if finished runtime.info
            then runtime
            else
              runtime
                { info = runtime.info
                    { status
                    , finishedAt = Just finishedAt
                    }
                }
    in (Map.adjust update handleId runtimes, ())

cancelAndAwaitAll :: (IOE :> es, Prim :> es, Concurrent :> es) => ManagerState -> Eff es ()
cancelAndAwaitAll managerState = do
  threads <- managedThreads managerState
  traverse_ cancelAndAwait threads
  where
    cancelAndAwait (entryInfo, thread) = do
      unless (finished entryInfo) do
        finishEntry managerState entryInfo.id Cancelled
        Async.cancel thread
      void (Async.waitCatch thread)

cancelAndAwaitAllWith
  :: (IOE :> es, Prim :> es, Concurrent :> es)
  => ManagerState
  -> SomeException
  -> Eff es ()
cancelAndAwaitAllWith managerState err = do
  threads <- managedThreads managerState
  traverse_ cancelAndAwaitWith threads
  where
    cancelAndAwaitWith (entryInfo, thread) = do
      unless (finished entryInfo) do
        finishEntry managerState entryInfo.id Cancelled
        Async.cancelWith thread err
      void (Async.waitCatch thread)

managedThreads :: Prim :> es => ManagerState -> Eff es [(Info, Async.Async ())]
managedThreads managerState =
  map (\runtime -> (runtime.info, runtime.thread)) . Map.elems <$> readIORef managerState.runtimes
