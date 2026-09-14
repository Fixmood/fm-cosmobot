{-|
Module      : Bot.Agent.Middleware.ToolEmittedMessage
Description : Tool-emitted chat message capture middleware
Stability   : experimental
-}

module Bot.Agent.Middleware.ToolEmittedMessage
  ( ToolEmittedMessageSink (..)
  , withLinkingToolEmittedMessagesToThread
  , withRecordingToolSelfMessages
  )
where

import Bot.Agent.Core
import Bot.Core.Message
import qualified Bot.Effect.Chat as Chat
import Bot.Prelude

newtype ToolEmittedMessageSink es = ToolEmittedMessageSink
  { remember :: Maybe MessageId -> Eff es ()
  }

withLinkingToolEmittedMessagesToThread
  :: Chat.Chat :> es
  => ToolEmittedMessageSink es
  -> Runtime context (Eff es)
  -> Runtime context (Eff es)
withLinkingToolEmittedMessagesToThread sink program =
  program
    { aroundToolCall = \turn call context action ->
        Chat.runChatRecordingExtraMessages sink.remember $
          program.aroundToolCall turn call context action
    }

-- | The sink receives the platform id of the message that was just sent, so the
-- recorded row can be found again later; without it a self row has no id.
withRecordingToolSelfMessages
  :: Chat.Chat :> es
  => (Maybe MessageId -> Text -> Eff es ())
  -> Runtime context (Eff es)
  -> Runtime context (Eff es)
withRecordingToolSelfMessages recordSelfMessage program =
  program
    { aroundToolCall = \turn call context action ->
        Chat.runChatRecordingSelfMessages recordSelfMessage $
          program.aroundToolCall turn call context action
    }
