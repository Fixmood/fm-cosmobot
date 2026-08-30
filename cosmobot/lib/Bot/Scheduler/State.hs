{-|
Module      : Bot.Scheduler.State
Description : Pure scheduler queue state operations
Stability   : experimental
-}

module Bot.Scheduler.State
  ( schedulerStateFromStoredMessages
  , popDueMessagesFromState
  , rememberStoredMessage
  , deletePendingMessageFromState
  , scheduledMessage
  , sameMessageOwner
  )
where

import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Storage.Scheduler as SchedulerStorage
import Bot.Scheduler.Types
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

schedulerStateFromStoredMessages :: [SchedulerStorage.StoredScheduledMessage] -> SchedulerState
schedulerStateFromStoredMessages storedMessages =
  SchedulerState
    { pendingById = Map.fromList [(pending.scheduleId, pending) | pending <- pendingMessages]
    , pendingByDue = Set.fromList [PendingDue{dueAtUnixSeconds = pending.dueAtUnixSeconds, scheduleId = pending.scheduleId} | pending <- pendingMessages]
    }
  where
    pendingMessages =
      [ PendingMessage
          { scheduleId = stored.scheduleId
          , dueAtUnixSeconds = stored.dueAtUnixSeconds
          , message = stored.message
          }
      | stored <- storedMessages
      ]

popDueMessagesFromState :: Integer -> SchedulerState -> (SchedulerState, [PendingMessage])
popDueMessagesFromState now schedulerState =
  go schedulerState []
  where
    go current acc =
      case Set.minView current.pendingByDue of
        Nothing ->
          (current, reverse acc)
        Just (due, rest)
          | due.dueAtUnixSeconds > now ->
              (current, reverse acc)
          | otherwise ->
              let (pendingMessage, nextById) =
                    case Map.lookup due.scheduleId current.pendingById of
                      Nothing ->
                        (Nothing, current.pendingById)
                      Just pending ->
                        (Just pending, Map.delete due.scheduleId current.pendingById)
                  nextState = current{pendingById = nextById, pendingByDue = rest}
              in go nextState (maybe acc (: acc) pendingMessage)

rememberStoredMessage :: SchedulerStorage.StoredScheduledMessage -> SchedulerState -> (SchedulerState, PendingMessage)
rememberStoredMessage stored schedulerState =
  (nextState, pendingMessage)
  where
    pendingMessage = PendingMessage
      { scheduleId = stored.scheduleId
      , dueAtUnixSeconds = stored.dueAtUnixSeconds
      , message = stored.message
      }
    due = PendingDue
      { dueAtUnixSeconds = pendingMessage.dueAtUnixSeconds
      , scheduleId = pendingMessage.scheduleId
      }
    nextState = schedulerState
      { pendingById = Map.insert pendingMessage.scheduleId pendingMessage schedulerState.pendingById
      , pendingByDue = Set.insert due schedulerState.pendingByDue
      }

deletePendingMessageFromState :: IncomingMessage -> Integer -> SchedulerState -> (SchedulerState, Bool)
deletePendingMessageFromState message scheduleId schedulerState =
  case Map.lookup scheduleId schedulerState.pendingById of
    Nothing ->
      (schedulerState, False)
    Just schedule
      | sameMessageOwner message schedule.message ->
          let due = PendingDue
                { dueAtUnixSeconds = schedule.dueAtUnixSeconds
                , scheduleId = scheduleId
                }
              nextState = schedulerState
                { pendingById = Map.delete scheduleId schedulerState.pendingById
                , pendingByDue = Set.delete due schedulerState.pendingByDue
                }
          in (nextState, True)
      | otherwise ->
          (schedulerState, False)

scheduledMessage :: Integer -> PendingMessage -> ScheduledMessage
scheduledMessage now pending =
  ScheduledMessage
    { scheduleId = pending.scheduleId
    , remainingSeconds = remainingSecondsUntil now pending.dueAtUnixSeconds
    , message = pending.message
    }

remainingSecondsUntil :: Integer -> Integer -> Int
remainingSecondsUntil now dueAt
  | dueAt <= now = 0
  | otherwise =
      fromIntegral (dueAt - now)

sameMessageOwner :: IncomingMessage -> IncomingMessage -> Bool
sameMessageOwner left right =
  left.platform == right.platform
    && left.kind == right.kind
    && left.chatId == right.chatId
    && sameSender left right

sameSender :: IncomingMessage -> IncomingMessage -> Bool
sameSender left right =
  case (left.senderId, right.senderId) of
    (Just leftId, Just rightId) ->
      leftId == rightId
    (Nothing, Nothing) ->
      left.senderUsername == right.senderUsername
    _ ->
      False
