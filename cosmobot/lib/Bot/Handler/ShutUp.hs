{-|
Module      : Bot.Handler.ShutUp
Description : Delete incoming messages matching configured patterns
Stability   : experimental
-}

module Bot.Handler.ShutUp
  ( shutUpHandlers
  )
where

import Bot.Core.Message
import Bot.Core.Route
import qualified Bot.Effect.Chat as Chat
import Bot.Handler.ShutUp.Config
import Bot.Prelude
import qualified Data.List as List
import Text.Regex.TDFA (matchTest)

shutUpHandlers :: Chat.Chat :> es => ShutUpConfig -> [RouteHandler es]
shutUpHandlers cfg =
  [ stopOn (deletePattern cfg) deleteMatchedMessage
  ]

deletePattern :: ShutUpConfig -> MessageFilter DeletePattern
deletePattern cfg =
  MessageFilter \message ->
    List.find (`matchesMessage` message) cfg.deletePatterns

matchesMessage :: DeletePattern -> IncomingMessage -> Bool
matchesMessage pattern_ message =
  matchTest pattern_.regex message.text

deleteMatchedMessage :: Chat.Chat :> es => IncomingMessage -> DeletePattern -> Eff es ()
deleteMatchedMessage message _ =
  traverse_ (void . Chat.deleteMessage message) message.messageId
