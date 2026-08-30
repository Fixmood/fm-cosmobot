{-# LANGUAGE OverloadedLabels #-}
{-|
Module      : Bot.Storage.Matrix
Description : Persistent Matrix driver state
Stability   : experimental
-}

module Bot.Storage.Matrix
  ( loadSyncToken
  , saveSyncToken
  )
where

import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import Bot.Storage.Prelude

data MatrixSyncStateRow = MatrixSyncStateRow
  { key :: Text
  , next_batch :: Text
  }
  deriving (Generic)

instance SqlRow MatrixSyncStateRow

matrixSyncStateRows :: Table MatrixSyncStateRow
matrixSyncStateRows =
  table "matrix_sync_state"
    [ #key :- primary
    ]

matrixSyncStateKey :: Text
matrixSyncStateKey =
  "default"

loadSyncToken :: Storage.Storage :> es => Eff es (Maybe Text)
loadSyncToken = do
  ensureMatrixSyncStateTable
  rows <- runSelda $
    query do
      row <- select matrixSyncStateRows
      restrict (row ! #key .== literal matrixSyncStateKey)
      pure (row ! #next_batch)
  pure (viaNonEmpty head rows)

saveSyncToken :: Storage.Storage :> es => Text -> Eff es ()
saveSyncToken token = do
  ensureMatrixSyncStateTable
  runSelda do
    deleteFrom_ matrixSyncStateRows \row ->
      row ! #key .== literal matrixSyncStateKey
    insert_
      matrixSyncStateRows
      [ MatrixSyncStateRow
          { key = matrixSyncStateKey
          , next_batch = token
          }
      ]

ensureMatrixSyncStateTable :: Storage.Storage :> es => Eff es ()
ensureMatrixSyncStateTable =
  runSelda (tryCreateTable matrixSyncStateRows)
