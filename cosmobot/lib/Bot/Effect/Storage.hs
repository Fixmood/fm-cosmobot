{-|
Module      : Bot.Effect.Storage
Description : Selda storage capability facade
Stability   : experimental
-}

module Bot.Effect.Storage
  ( Storage(..)
  , runSelda
  )
where

import Bot.Prelude
import qualified Database.Selda as Selda
import qualified Database.Selda.SQLite as SeldaSQLite

data Storage :: Effect where
  RunSelda
    :: Selda.SeldaT SeldaSQLite.SQLite IO a
    -> Storage m a

type instance DispatchOf Storage = Dynamic

runSelda :: Storage :> es => Selda.SeldaT SeldaSQLite.SQLite IO a -> Eff es a
runSelda action =
  send (RunSelda action)
