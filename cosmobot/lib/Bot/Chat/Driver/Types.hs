{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.Chat.Driver.Types
Description : Shared chat driver adapter types
Stability   : experimental
-}

module Bot.Chat.Driver.Types
  ( ChatDriver (..)
  , uploadFileName
  )
where

import Bot.Core.Message
import qualified Bot.Chat.Types as Chat
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import System.FilePath (takeFileName)

class ChatDriver driver where
  type ChatDriverEffects driver (es :: [Effect]) :: Constraint
  type ChatDriverEffects driver es = ()

  driverPlatform :: driver -> ChatPlatform

  sendReplyMessage :: ChatDriverEffects driver es => driver -> IncomingMessage -> Text -> Eff es (Either Text MessageId)
  sendReplyMessage driver _ _ =
    pure (Left [i|#{driverPlatform driver} does not support replies.|])

  sendReplyMessages :: ChatDriverEffects driver es => driver -> IncomingMessage -> Text -> Eff es [Either Text MessageId]
  sendReplyMessages driver message body =
    (: []) <$> sendReplyMessage driver message body

  -- Send a platform-native merged-forward message. The values are already
  -- encoded platform message nodes; unsupported drivers use the default error.
  sendMergedForward :: ChatDriverEffects driver es => driver -> IncomingMessage -> [Aeson.Value] -> Eff es (Either Text MessageId)
  sendMergedForward driver _ _ =
    pure (Left [i|#{driverPlatform driver} does not support merged forward messages.|])

  sendStreamingReplyMessage :: ChatDriverEffects driver es => driver -> IncomingMessage -> Text -> Eff es (Either Text MessageId)
  sendStreamingReplyMessage =
    sendReplyMessage

  replyAudio :: ChatDriverEffects driver es => driver -> IncomingMessage -> Text -> Maybe Text -> Eff es (Either Text MessageId)
  replyAudio driver _ _ _ =
    pure (Left [i|#{driverPlatform driver} does not support audio replies.|])

  uploadFile :: ChatDriverEffects driver es => driver -> IncomingMessage -> FilePath -> Maybe Text -> Eff es (Either Text MessageId)
  uploadFile driver _ _ _ =
    pure (Left [i|#{driverPlatform driver} does not support file uploads.|])

  editMessage :: ChatDriverEffects driver es => driver -> IncomingMessage -> MessageId -> Text -> Eff es Bool
  editMessage _ _ _ _ =
    pure False

  completeMessageEdit :: ChatDriverEffects driver es => driver -> IncomingMessage -> MessageId -> Eff es Bool
  completeMessageEdit _ _ _ =
    pure True

  deleteMessage :: ChatDriverEffects driver es => driver -> IncomingMessage -> MessageId -> Eff es Bool
  deleteMessage _ _ _ =
    pure False

  recallRecentSelfMessages :: ChatDriverEffects driver es => driver -> IncomingMessage -> Int -> Eff es (Int, Int)
  recallRecentSelfMessages _ _ _ =
    pure (0, 0)

  messageOutPolicy :: ChatDriverEffects driver es => driver -> IncomingMessage -> Eff es Chat.MessageOutPolicy
  messageOutPolicy _ _ =
    pure (Chat.ChunkedMessage 4000)

  getMessageContent :: ChatDriverEffects driver es => driver -> IncomingMessage -> MessageId -> Eff es (Maybe ReferencedMessage)
  getMessageContent _ _ _ =
    pure Nothing

  getSenderMemberInfo :: ChatDriverEffects driver es => driver -> IncomingMessage -> Eff es (Maybe Aeson.Value)
  getSenderMemberInfo _ _ =
    pure Nothing

  getMemberInfo :: ChatDriverEffects driver es => driver -> IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
  getMemberInfo _ _ _ =
    pure Nothing

  getUserAvatar :: ChatDriverEffects driver es => driver -> IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
  getUserAvatar _ _ _ =
    pure Nothing

  listGroupMembers :: ChatDriverEffects driver es => driver -> IncomingMessage -> Eff es (Maybe Aeson.Value)
  listGroupMembers _ _ =
    pure Nothing

  normalizeMediaRef :: ChatDriverEffects driver es => driver -> Text -> Eff es Text
  normalizeMediaRef _ =
    pure

  normalizeMediaRefForMessage :: ChatDriverEffects driver es => driver -> IncomingMessage -> Text -> Eff es Text
  normalizeMediaRefForMessage driver _ =
    normalizeMediaRef driver

  mentionUser :: ChatDriverEffects driver es => driver -> IncomingMessage -> Text -> Text -> Eff es (Either Text MessageId)
  mentionUser driver message _ body =
    sendReplyMessage driver message body

  setMemberTitle :: ChatDriverEffects driver es => driver -> IncomingMessage -> Text -> Text -> Eff es Bool
  setMemberTitle _ _ _ _ =
    pure False

  setTyping :: ChatDriverEffects driver es => driver -> IncomingMessage -> Int -> Eff es ()
  setTyping _ _ _ =
    pure ()

uploadFileName :: FilePath -> Maybe Text -> Text
uploadFileName path requested =
  fromMaybe fallback (requested >>= nonEmptyBaseName)
  where
    fallback = nonEmptyOrFile (Text.pack (takeFileName path))
    nonEmptyBaseName raw =
      let name = Text.pack (takeFileName (Text.unpack (Text.strip raw)))
      in name <$ guard (not (Text.null name))
    nonEmptyOrFile name
      | Text.null name = "file"
      | otherwise = name
