{-# LANGUAGE OverloadedLabels #-}
{-|
Module      : Bot.Storage.Lifecycle
Description : Durable lifecycle actions executed on process startup
Stability   : experimental
-}

module Bot.Storage.Lifecycle
  ( StoredStartupAction (..)
  , enqueueStartupReply
  , loadStartupActions
  , deleteStartupAction
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import Bot.Storage.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text.Encoding as TextEncoding

data StoredStartupAction = StartupReply
  { actionId :: !Integer
  , actionKey :: !Text
  , message :: !IncomingMessage
  , body :: !Text
  }
  deriving (Show)

data LifecycleActionRow = LifecycleActionRow
  { id :: ID LifecycleActionRow
  , action_key :: Text
  , action_kind :: Text
  , message_json :: Text
  , body :: Text
  }
  deriving (Generic)

instance SqlRow LifecycleActionRow

lifecycleActions :: Table LifecycleActionRow
lifecycleActions =
  table "lifecycle_actions"
    [ #id :- autoPrimary
    , #action_key :- unique
    , #action_kind :- index
    ]

enqueueStartupReply :: Storage.Storage :> es => Text -> IncomingMessage -> Text -> Eff es StoredStartupAction
enqueueStartupReply actionKey message body = do
  ensureLifecycleActionsTable
  runSelda $
    insert_
      lifecycleActions
      [ LifecycleActionRow
          { id = def
          , action_key = actionKey
          , action_kind = "startup_reply"
          , message_json = encodeMessage message
          , body
          }
      ]
  pure StartupReply
    { actionId = 0
    , actionKey
    , message
    , body
    }

loadStartupActions :: Storage.Storage :> es => Eff es [StoredStartupAction]
loadStartupActions = do
  ensureLifecycleActionsTable
  rows <- runSelda $
    query do
      row <- select lifecycleActions
      order (row ! #id) ascending
      pure row
  pure (mapMaybe startupActionFromRow rows)

deleteStartupAction :: Storage.Storage :> es => StoredStartupAction -> Eff es ()
deleteStartupAction action = do
  ensureLifecycleActionsTable
  runSelda $
    deleteFrom_ lifecycleActions \row ->
      row ! #action_key .== literal action.actionKey

ensureLifecycleActionsTable :: Storage.Storage :> es => Eff es ()
ensureLifecycleActionsTable =
  runSelda (tryCreateTable lifecycleActions)

startupActionFromRow :: LifecycleActionRow -> Maybe StoredStartupAction
startupActionFromRow row
  | row.action_kind == "startup_reply" = do
      message <- decodeMessage row.message_json
      pure StartupReply
        { actionId = fromIntegral (fromId row.id)
        , actionKey = row.action_key
        , message
        , body = row.body
        }
  | otherwise =
      Nothing

encodeMessage :: IncomingMessage -> Text
encodeMessage =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

decodeMessage :: Text -> Maybe IncomingMessage
decodeMessage =
  either (const Nothing) Just . Aeson.eitherDecodeStrict' . TextEncoding.encodeUtf8
