{-|
Module      : Bot.Prelude
Description : Custom prelude
Stability   : experimental
-}

module Bot.Prelude
  ( module Relude
  , module Data.String.Interpolate
  , module Effectful
  , module Effectful.Concurrent
  , module Effectful.Dispatch.Dynamic
  , module Bot.Log
  , module Effectful.Exception
  , module Effectful.Fail
  , module Effectful.Prim.IORef
  , Stream
  , Of
  )
where

-- ---------------------------------------------------------------------------
-- Prelude
-- ---------------------------------------------------------------------------

import Relude hiding
  ( newIORef
  , readIORef
  , writeIORef
  , modifyIORef
  , modifyIORef'
  , atomicWriteIORef
  , atomicModifyIORef
  , atomicModifyIORef'
  , IORef
  )

-- ---------------------------------------------------------------------------
-- String
-- ---------------------------------------------------------------------------

import Data.String.Interpolate
import Bot.Log

-- ---------------------------------------------------------------------------
-- Effects
-- ---------------------------------------------------------------------------

import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.Exception
import Effectful.Fail
import Effectful.Prim.IORef

-- ---------------------------------------------------------------------------
-- Streams
-- ---------------------------------------------------------------------------

import Streaming (Stream, Of)
