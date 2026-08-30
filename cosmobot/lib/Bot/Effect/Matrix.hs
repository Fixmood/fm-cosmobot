{-# LANGUAGE TypeFamilies #-}

module Bot.Effect.Matrix
  ( Matrix (..)
  , MatrixClientMethod (..)
  , MatrixClientRequest (..)
  , matrixClientCall
  )
where

import Bot.Prelude
import qualified Data.Aeson as Aeson

data Matrix :: Effect where
  MatrixClientCall :: MatrixClientRequest -> Matrix m Aeson.Value

type instance DispatchOf Matrix = Dynamic

data MatrixClientMethod = MatrixGet | MatrixPost | MatrixPut | MatrixDelete
  deriving (Eq, Show)

data MatrixClientRequest = MatrixClientRequest
  { method :: !MatrixClientMethod
  , path :: ![Text]
  , query :: ![(Text, Text)]
  , body :: !(Maybe Aeson.Value)
  }
  deriving (Eq, Show)

matrixClientCall :: Matrix :> es => MatrixClientRequest -> Eff es Aeson.Value
matrixClientCall = send . MatrixClientCall
