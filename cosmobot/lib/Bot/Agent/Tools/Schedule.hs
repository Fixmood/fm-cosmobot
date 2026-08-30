{-|
Module      : Bot.Agent.Tools.Schedule
Description : Agent scheduler tools
Stability   : experimental
-}

module Bot.Agent.Tools.Schedule
  ( scheduleTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Agent.Types
import Bot.Core.Message
import qualified Bot.Effect.Scheduler as Scheduler
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes

scheduleTool :: Scheduler.Scheduler :> es => Tool (Eff es)
scheduleTool =
  tagged [workTag]
  . withDescription "Create, list, or delete scheduled agent actions owned by the current user in the current chat."
  $ tool "schedule"
      (parsedArguments
        (objectSchema
          [ fieldText "op" "One of: create, list, delete."
          , fieldInteger "delay_seconds" "Delay in seconds; required for create."
          , fieldText "prompt" "Future agent prompt; required for create."
          , fieldInteger "schedule_id" "Schedule id; required for delete."
          ]
          ["op"])
        scheduleArgs)
      \call -> do
        context <- askToolContext
        case call of
          ScheduleCreate delaySeconds prompt -> do
            scheduled <- Scheduler.scheduleMessage delaySeconds (scheduledAgentMessage context delaySeconds prompt)
            pure $ if scheduled
              then toolText [i|Scheduled agent action in #{delaySeconds} seconds.|]
              else toolText "Could not schedule agent action: scheduler is at capacity."
          ScheduleDelete scheduleId -> do
            deleted <- Scheduler.deleteScheduledMessage context.message scheduleId
            pure $ toolText $ if deleted
              then [i|Schedule #{scheduleId} has been removed.|]
              else [i|Schedule #{scheduleId} is not available to the user.|]
          ScheduleList ->
            toolText . jsonText . map scheduleSummary <$> Scheduler.listScheduledMessages context.message

data ScheduleCall
  = ScheduleCreate !Int !Text
  | ScheduleDelete !Integer
  | ScheduleList

scheduleArgs :: Aeson.Value -> AesonTypes.Parser ScheduleCall
scheduleArgs = Aeson.withObject "schedule arguments" \o -> do
  op <- o Aeson..: Key.fromText "op"
  case op :: Text of
    "create" -> ScheduleCreate <$> o Aeson..: Key.fromText "delay_seconds" <*> o Aeson..: Key.fromText "prompt"
    "delete" -> ScheduleDelete <$> o Aeson..: Key.fromText "schedule_id"
    "list" -> pure ScheduleList
    _ -> fail "op must be one of: create, list, delete."

data ScheduleSummary = ScheduleSummary
  { scheduleId :: !Integer
  , remainingSeconds :: !Int
  , prompt :: !Text
  }
  deriving (Show, Generic, Aeson.ToJSON)

scheduleSummary :: Scheduler.ScheduledMessage -> ScheduleSummary
scheduleSummary schedule =
  ScheduleSummary
    { scheduleId = schedule.scheduleId
    , remainingSeconds = schedule.remainingSeconds
    , prompt = scheduledPrompt schedule.message
    }

scheduledPrompt :: IncomingMessage -> Text
scheduledPrompt message =
  fromMaybe message.text (AesonTypes.parseMaybe parsePrompt message.raw)
  where
    parsePrompt =
      Aeson.withObject "scheduled action" (Aeson..: Key.fromText "prompt")

scheduledAgentMessage :: Context -> Int -> Text -> IncomingMessage
scheduledAgentMessage context delaySeconds prompt =
  let original = context.message
      commandText = context.askCommand <> " " <> prompt
  in original
      { messageId = original.messageId
      , replyToMessageId = Nothing
      , mentions = original.mentions
      , mentionUsernames = original.mentionUsernames
      , imageUrls = []
      , files = []
      , text = commandText
      , raw = Aeson.object
          [ "type" Aeson..= Aeson.String "scheduled_agent_action"
          , "delay_seconds" Aeson..= delaySeconds
          , "prompt" Aeson..= prompt
          , "original_message" Aeson..= original.raw
          ]
      }
