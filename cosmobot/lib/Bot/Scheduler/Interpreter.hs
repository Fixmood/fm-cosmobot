{-|
Module      : Bot.Scheduler.Interpreter
Description : Scheduler effect interpreter
Stability   : experimental
-}

module Bot.Scheduler.Interpreter
  ( runScheduler
  )
where

import qualified Bot.Effect.Storage as Storage
import Bot.Core.Message
import qualified Bot.Effect.Concurrency as Concurrency
import Bot.Prelude
import Bot.Scheduler.State
import Bot.Scheduler.Types
import qualified Bot.Storage.Scheduler as SchedulerStorage
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Effectful.Concurrent.MVar as MVar
import qualified Effectful.Concurrent.STM as STM
import Effectful.Timeout

-- | Interpret scheduled messages with an in-memory delay queue.
runScheduler
  :: (IOE :> es, Concurrency.Concurrency :> es, Storage.Storage :> es, Concurrent :> es, Timeout :> es)
  => Eff (Scheduler : es) a
  -> Eff es a
runScheduler inner = do
  storedMessages <- SchedulerStorage.loadScheduledMessages
  queue <- STM.newTBQueueIO scheduledMessageQueueCapacity
  schedulerStateVar <- MVar.newMVar (schedulerStateFromStoredMessages storedMessages)
  schedulerWake <- MVar.newEmptyMVar
  Concurrency.withWorker "scheduler.worker" (schedulerWorker schedulerStateVar schedulerWake queue) $
    interpret
      (\_ -> \case
        ScheduleMessage delaySeconds message -> do
          _ <- persistPendingMessage schedulerStateVar delaySeconds message
          signalSchedulerWake schedulerWake
          pure True
        DeleteScheduledMessage message scheduleId -> do
          deleted <- deletePersistedPendingMessage schedulerStateVar message scheduleId
          when deleted (signalSchedulerWake schedulerWake)
          pure deleted
        ListScheduledMessages message -> do
          now <- currentUnixSeconds
          pending <- MVar.withMVar schedulerStateVar (pure . Map.elems . (.pendingById))
          pure
            [ scheduledMessage now pendingMessage
            | pendingMessage <- pending
            , sameMessageOwner message pendingMessage.message
            ]
        ReceiveScheduledMessage -> do
          pending <- STM.atomically (STM.readTBQueue queue)
          SchedulerStorage.deleteScheduledMessage pending.scheduleId
          pure pending.message)
      inner

schedulerWorker
  :: (Concurrent :> es, IOE :> es, Timeout :> es)
  => MVar.MVar SchedulerState
  -> MVar.MVar ()
  -> STM.TBQueue PendingMessage
  -> Eff es ()
schedulerWorker schedulerStateVar schedulerWake queue =
  forever do
    now <- currentUnixSeconds
    dueMessages <- popDueMessages schedulerStateVar now
    traverse_ (STM.atomically . STM.writeTBQueue queue) dueMessages
    nextDue <- nextDueAt schedulerStateVar
    case nextDue of
      Nothing ->
        MVar.takeMVar schedulerWake
      Just dueAt ->
        void $ timeout (waitMicrosecondsUntil now dueAt) (MVar.takeMVar schedulerWake)
    drainSchedulerWake schedulerWake

popDueMessages :: Concurrent :> es => MVar.MVar SchedulerState -> Integer -> Eff es [PendingMessage]
popDueMessages schedulerStateVar now =
  MVar.modifyMVar schedulerStateVar \schedulerState -> do
    let (nextState, dueMessages) = popDueMessagesFromState now schedulerState
    pure (nextState, dueMessages)

nextDueAt :: (Concurrent :> es) => MVar.MVar SchedulerState -> Eff es (Maybe Integer)
nextDueAt schedulerStateVar =
  MVar.withMVar schedulerStateVar \schedulerState ->
    pure ((.dueAtUnixSeconds) <$> Set.lookupMin schedulerState.pendingByDue)

waitMicrosecondsUntil :: Integer -> Integer -> Int
waitMicrosecondsUntil now dueAt
  | dueAt <= now = 0
  | otherwise =
      fromIntegral (min maxWaitMicroseconds ((dueAt - now) * 1000000))
  where
    maxWaitMicroseconds = toInteger (maxBound :: Int)

signalSchedulerWake :: Concurrent :> es => MVar.MVar () -> Eff es ()
signalSchedulerWake schedulerWake =
  void (MVar.tryPutMVar schedulerWake ())

drainSchedulerWake :: Concurrent :> es => MVar.MVar () -> Eff es ()
drainSchedulerWake schedulerWake = do
  value <- MVar.tryTakeMVar schedulerWake
  when (isJust value) (drainSchedulerWake schedulerWake)

persistPendingMessage ::
  (Concurrent :> es, IOE :> es, Storage.Storage :> es)
  => MVar.MVar SchedulerState -> Int -> IncomingMessage -> Eff es PendingMessage
persistPendingMessage schedulerStateVar delaySeconds message = do
  now <- currentUnixSeconds
  let dueAt = now + fromIntegral (max 0 delaySeconds)
  MVar.modifyMVarMasked schedulerStateVar \schedulerState -> do
    stored <- SchedulerStorage.createScheduledMessage dueAt message
    pure (rememberStoredMessage stored schedulerState)

deletePersistedPendingMessage
  :: (Concurrent :> es, Storage.Storage :> es)
  => MVar.MVar SchedulerState
  -> IncomingMessage
  -> Integer
  -> Eff es Bool
deletePersistedPendingMessage schedulerStateVar message scheduleId =
  MVar.modifyMVarMasked schedulerStateVar \schedulerState -> do
    let (nextState, deleted) = deletePendingMessageFromState message scheduleId schedulerState
    when deleted (SchedulerStorage.deleteScheduledMessage scheduleId)
    pure (nextState, deleted)

currentUnixSeconds :: IOE :> es => Eff es Integer
currentUnixSeconds = liftIO $
  floor <$> getPOSIXTime

scheduledMessageQueueCapacity :: Natural
scheduledMessageQueueCapacity =
  1024
