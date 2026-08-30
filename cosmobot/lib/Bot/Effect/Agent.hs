{-# LANGUAGE DataKinds #-}

{-|
Module      : Bot.Effect.Agent
Description : Re-entrant agent execution capability
Stability   : experimental
-}
module Bot.Effect.Agent
  ( Agent (..)
  , withRun
  )
where

import Bot.Agent.Core (Runtime)
import Bot.Prelude

data Agent :: Effect where
  RunAgent
    :: !(Runtime '[] m)
    -> (Runtime '[] m -> m a)
    -> Agent m a

type instance DispatchOf Agent = Dynamic

withRun
  :: Agent :> es
  => Runtime '[] (Eff es)
  -> (Runtime '[] (Eff es) -> Eff es a)
  -> Eff es a
withRun runtime =
  send . RunAgent runtime
