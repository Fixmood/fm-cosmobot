{-|
Module      : Bot.Effect.HTTP
Description : Shared HTTP client capability facade
Stability   : experimental
-}

module Bot.Effect.HTTP
  ( HTTP (..)
  , manager
  , runReq
  , runReqWithConfig
  , openResponse
  )
where

import Bot.Prelude
import qualified Network.HTTP.Client as Client
import Network.HTTP.Req (HttpConfig, Req)

data HTTP :: Effect where
  Manager :: HTTP m Client.Manager
  RunReq :: Req a -> HTTP m a
  RunReqWithConfig :: HttpConfig -> Req a -> HTTP m a
  OpenResponse :: Client.Request -> HTTP m (Client.Response Client.BodyReader)

type instance DispatchOf HTTP = Dynamic

manager :: HTTP :> es => Eff es Client.Manager
manager =
  send Manager

runReq :: HTTP :> es => Req a -> Eff es a
runReq action =
  send (RunReq action)

runReqWithConfig :: HTTP :> es => HttpConfig -> Req a -> Eff es a
runReqWithConfig config action =
  send (RunReqWithConfig config action)

openResponse :: HTTP :> es => Client.Request -> Eff es (Client.Response Client.BodyReader)
openResponse request =
  send (OpenResponse request)
