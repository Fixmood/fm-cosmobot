{-|
Module      : Bot.Handler.Help
Description : Route-algebra generated command help
Stability   : experimental
-}

module Bot.Handler.Help
  ( helpHandlers
  , renderHelp
  )
where

import Bot.Core.Route
import qualified Bot.Effect.Chat as Chat
import Bot.Prelude
import qualified Data.Text as Text

helpHandlers :: Chat.Chat :> es => [RouteHandler es] -> [RouteHandler es]
helpHandlers routes =
  [ helpRoute ]
  where
    helpRoute = withHelp helpMetadata $
      stopOn (command "!help") \message _ ->
        void $ Chat.replyTo message (renderHelp (collectRouteHelp message (helpRoute : routes)))

helpMetadata :: RouteHelp
helpMetadata =
  RouteHelp "!help" "List available commands."

renderHelp :: [RouteHelp] -> Text
renderHelp entries =
  Text.unlines $ "Available commands:" : map renderEntry entries
  where
    renderEntry entry =
      "- `" <> entry.label <> "` — " <> entry.description
