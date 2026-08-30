{-|
Module      : Bot.Effect.Lifecycle
Description : Process lifecycle request capability
Stability   : experimental
-}

module Bot.Effect.Lifecycle
  ( Lifecycle (..)
  , requestRestart
  )
where

import Bot.Core.Message
import Bot.Prelude

data Lifecycle :: Effect where
  RequestRestart :: IncomingMessage -> Text -> Lifecycle m ()

type instance DispatchOf Lifecycle = Dynamic

requestRestart :: Lifecycle :> es => IncomingMessage -> Text -> Eff es ()
requestRestart message body =
  send (RequestRestart message body)
