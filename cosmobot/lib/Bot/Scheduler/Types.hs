{-|
Module      : Bot.Scheduler.Types
Description : Scheduler domain values
Stability   : experimental
-}

module Bot.Scheduler.Types
  ( Scheduler (..)
  , ScheduledMessage (..)
  , PendingMessage (..)
  , SchedulerState (..)
  , PendingDue (..)
  , scheduleMessage
  , deleteScheduledMessage
  , listScheduledMessages
  , scheduledMessages
  , receiveScheduledMessage
  )
where

import Bot.Core.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Streaming as S
import qualified Streaming.Prelude as S

data ScheduledMessage = ScheduledMessage
  { scheduleId :: !Integer
  , remainingSeconds :: !Int
  , message :: !IncomingMessage
  }
  deriving (Show, Generic, Aeson.ToJSON)

data PendingMessage = PendingMessage
  { scheduleId :: !Integer
  , dueAtUnixSeconds :: !Integer
  , message :: !IncomingMessage
  }

data SchedulerState = SchedulerState
  { pendingById :: !(Map Integer PendingMessage)
  , pendingByDue :: !(Set PendingDue)
  }
  deriving (Generic)

data PendingDue = PendingDue
  { dueAtUnixSeconds :: !Integer
  , scheduleId :: !Integer
  }
  deriving (Eq, Ord)

-- | In-process delayed message scheduler.
data Scheduler :: Effect where
  ScheduleMessage
    :: Int
    -> IncomingMessage
    -> Scheduler m Bool
  ListScheduledMessages
    :: IncomingMessage
    -> Scheduler m [ScheduledMessage]
  ReceiveScheduledMessage
    :: Scheduler m IncomingMessage
  DeleteScheduledMessage
    :: IncomingMessage
    -> Integer
    -> Scheduler m Bool

type instance DispatchOf Scheduler = Dynamic

-- | Re-inject a message into the incoming stream after a delay in seconds.
scheduleMessage :: Scheduler :> es => Int -> IncomingMessage -> Eff es Bool
scheduleMessage delaySeconds message =
  send (ScheduleMessage delaySeconds message)

-- | Return pending scheduled messages owned by the same platform chat sender.
listScheduledMessages :: Scheduler :> es => IncomingMessage -> Eff es [ScheduledMessage]
listScheduledMessages =
  send . ListScheduledMessages

-- | Stream of messages whose delay has elapsed.
scheduledMessages :: Scheduler :> es => Stream (Of IncomingMessage) (Eff es) ()
scheduledMessages = do
  message <- S.lift receiveScheduledMessage
  S.yield message
  scheduledMessages

receiveScheduledMessage :: Scheduler :> es => Eff es IncomingMessage
receiveScheduledMessage =
  send ReceiveScheduledMessage

-- | Delete a scheduled message with ID. Returns True if there is such an ID.
deleteScheduledMessage :: Scheduler :> es => IncomingMessage -> Integer -> Eff es Bool
deleteScheduledMessage message schedId = send (DeleteScheduledMessage message schedId)
