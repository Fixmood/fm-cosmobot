{-|
Module      : Bot.Effect.Chat
Description : Business-facing chat capability facade
Stability   : experimental
-}

module Bot.Effect.Chat
  ( Chat
  , ChatHandler
  , replyTo
  , streamReplyTo
  , streamMultipleRepliesTo
  , runChatWithHandler
  , runChatWith
  , chatDriverHandler
  , module Bot.Chat.Types
  , module Bot.Core.ReplyBody
  , module ChatDriver
  )
where

import qualified Bot.Chat.Adapter as ChatAdapter
import Bot.Chat.Types
import Bot.Core.Message
import Bot.Core.ReplyBody
import qualified Bot.Chat.Driver.Types as Driver
import Bot.Effect.ChatDriver as ChatDriver
import Bot.Prelude

type Chat = ChatDriver.ChatDriver

type ChatHandler es = ChatDriver.ChatDriverHandler es

replyTo :: Chat :> es => IncomingMessage -> Text -> Eff es [Either Text MessageId]
replyTo =
  ChatAdapter.replyTo

streamReplyTo
  :: Chat :> es
  => IncomingMessage
  -> Stream (Of Text) (Eff es) r
  -> Stream (Of MessageOutResult) (Eff es) (MessageOutResult, r)
streamReplyTo =
  ChatAdapter.streamReplyTo

streamMultipleRepliesTo
  :: Chat :> es
  => IncomingMessage
  -> Stream (Stream (Of Text) (Eff es)) (Eff es) r
  -> Stream (Of MessageOutResult) (Eff es) (MessageOutResult, r)
streamMultipleRepliesTo =
  ChatAdapter.streamMultipleRepliesTo

runChatWithHandler
  :: ChatHandler es
  -> Eff (Chat : es) a
  -> Eff es a
runChatWithHandler =
  ChatDriver.runChatDriverWithHandler

runChatWith
  :: (Driver.ChatDriver driver, Driver.ChatDriverEffects driver es)
  => driver
  -> Eff (Chat : es) a
  -> Eff es a
runChatWith driver =
  runChatWithHandler (ChatDriver.chatDriverEffectHandler driver)

chatDriverHandler
  :: (Driver.ChatDriver driver, Driver.ChatDriverEffects driver es)
  => driver
  -> ChatHandler es
chatDriverHandler =
  ChatDriver.chatDriverEffectHandler
