{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.Effect.ChatDriver
Description : Unified chat capability facade
Stability   : experimental
-}

module Bot.Effect.ChatDriver
  ( -- * Effect
    ChatDriver (..)
  , ChatDriverHandler
  , sendReplyMessage
  , sendMergedForward
  , sendStreamingReplyMessage
  , replyAudio
  , uploadFile
  , editMessage
  , completeMessageEdit
  , deleteMessage
  , recallRecentSelfMessages
  , messageOutPolicy
  , getMessageContent
  , getSenderMemberInfo
  , getMemberInfo
  , getUserAvatar
  , listGroupMembers
  , normalizeMediaRef
  , mentionUser
  , setMemberTitle
  , setTyping
  , incomingMessages
  , runChatDriverWithHandler
  , chatDriverEffectHandler
  , runChatMappingReplies
  , runChatRecordingSelfMessages
  , runChatRecordingExtraMessages

  )
where

import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Chat.Driver.Types as Driver
import Bot.Chat.Types
import qualified Data.Aeson as Aeson

-- | Platform-independent chat operations used by handlers and tools.
data ChatDriver :: Effect where
  SendReplyMessage
    :: IncomingMessage
    -> Text
    -> ChatDriver m [Either Text MessageId]
  SendMergedForward
    :: IncomingMessage
    -> [Aeson.Value]
    -> ChatDriver m (Either Text MessageId)
  SendStreamingReplyMessage
    :: IncomingMessage
    -> Text
    -> ChatDriver m (Either Text MessageId)
  ReplyAudio
    :: IncomingMessage
    -> Text
    -> Maybe Text
    -> ChatDriver m (Either Text MessageId)
  UploadFile
    :: IncomingMessage
    -> FilePath
    -> Maybe Text
    -> ChatDriver m (Either Text MessageId)
  EditMessage
    :: IncomingMessage
    -> MessageId
    -> Text
    -> ChatDriver m Bool
  CompleteMessageEdit
    :: IncomingMessage
    -> MessageId
    -> ChatDriver m Bool
  DeleteMessage
    :: IncomingMessage
    -> MessageId
    -> ChatDriver m Bool
  RecallRecentSelfMessages
    :: IncomingMessage
    -> Int
    -> ChatDriver m (Int, Int)
  MessageOutPolicy
    :: IncomingMessage
    -> ChatDriver m MessageOutPolicy
  GetMessageContent
    :: IncomingMessage
    -> MessageId
    -> ChatDriver m (Maybe ReferencedMessage)
  GetSenderMemberInfo
    :: IncomingMessage
    -> ChatDriver m (Maybe Aeson.Value)
  GetMemberInfo
    :: IncomingMessage
    -> Text
    -> ChatDriver m (Maybe Aeson.Value)
  GetUserAvatar
    :: IncomingMessage
    -> Text
    -> ChatDriver m (Maybe Aeson.Value)
  ListGroupMembers
    :: IncomingMessage
    -> ChatDriver m (Maybe Aeson.Value)
  NormalizeMediaRef
    :: IncomingMessage
    -> Text
    -> ChatDriver m Text
  MentionUser
    :: IncomingMessage
    -> Text
    -> Text
    -> ChatDriver m (Either Text MessageId)
  SetMemberTitle
    :: IncomingMessage
    -> Text
    -> Text
    -> ChatDriver m Bool
  SetTyping
    :: IncomingMessage
    -> Int
    -> ChatDriver m ()
  IncomingMessages
    :: ChatDriver m (Stream (Of IncomingMessage) m ())

type instance DispatchOf ChatDriver = Dynamic

type ChatDriverHandler es =
  EffectHandler ChatDriver es

sendReplyMessage :: ChatDriver :> es => IncomingMessage -> Text -> Eff es [Either Text MessageId]
sendReplyMessage message body =
  send (SendReplyMessage message body)

sendMergedForward :: ChatDriver :> es => IncomingMessage -> [Aeson.Value] -> Eff es (Either Text MessageId)
sendMergedForward message nodes =
  send (SendMergedForward message nodes)

sendStreamingReplyMessage :: ChatDriver :> es => IncomingMessage -> Text -> Eff es (Either Text MessageId)
sendStreamingReplyMessage message body =
  send (SendStreamingReplyMessage message body)

-- | Send an audio reference to the chat containing the incoming message.
replyAudio :: ChatDriver :> es => IncomingMessage -> Text -> Maybe Text -> Eff es (Either Text MessageId)
replyAudio message audioRef caption =
  send (ReplyAudio message audioRef caption)

-- | Upload a local file to the chat containing the incoming message.
uploadFile :: ChatDriver :> es => IncomingMessage -> FilePath -> Maybe Text -> Eff es (Either Text MessageId)
uploadFile message path fileName =
  send (UploadFile message path fileName)

-- | Edit a previously sent message when the platform supports it.
editMessage :: ChatDriver :> es => IncomingMessage -> MessageId -> Text -> Eff es Bool
editMessage message messageId body =
  send (EditMessage message messageId body)

-- | Mark an editable streaming message complete without changing its content.
completeMessageEdit :: ChatDriver :> es => IncomingMessage -> MessageId -> Eff es Bool
completeMessageEdit message messageId =
  send (CompleteMessageEdit message messageId)

-- | Delete or recall a previously sent message when the platform supports it.
deleteMessage :: ChatDriver :> es => IncomingMessage -> MessageId -> Eff es Bool
deleteMessage message messageId =
  send (DeleteMessage message messageId)

-- | Recall the bot's own recent messages in the current chat.
-- Returns successful and failed deletion counts.
recallRecentSelfMessages :: ChatDriver :> es => IncomingMessage -> Int -> Eff es (Int, Int)
recallRecentSelfMessages message count =
  send (RecallRecentSelfMessages message count)

messageOutPolicy :: ChatDriver :> es => IncomingMessage -> Eff es MessageOutPolicy
messageOutPolicy message =
  send (MessageOutPolicy message)

-- | Fetch content for a referenced platform message id.
getMessageContent :: ChatDriver :> es => IncomingMessage -> MessageId -> Eff es (Maybe ReferencedMessage)
getMessageContent message messageId =
  send (GetMessageContent message messageId)

-- | Fetch member info for the sender of the current message.
getSenderMemberInfo :: ChatDriver :> es => IncomingMessage -> Eff es (Maybe Aeson.Value)
getSenderMemberInfo message =
  send (GetSenderMemberInfo message)

-- | Fetch member info for a user id in the current chat.
getMemberInfo :: ChatDriver :> es => IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
getMemberInfo message userId =
  send (GetMemberInfo message userId)

-- | Fetch avatar information for a platform user id.
getUserAvatar :: ChatDriver :> es => IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
getUserAvatar message userId =
  send (GetUserAvatar message userId)

-- | List group members when the platform exposes such an API.
listGroupMembers :: ChatDriver :> es => IncomingMessage -> Eff es (Maybe Aeson.Value)
listGroupMembers message =
  send (ListGroupMembers message)

normalizeMediaRef :: ChatDriver :> es => IncomingMessage -> Text -> Eff es Text
normalizeMediaRef message ref =
  send (NormalizeMediaRef message ref)

-- | Send a reply that mentions a platform user id.
mentionUser :: ChatDriver :> es => IncomingMessage -> Text -> Text -> Eff es (Either Text MessageId)
mentionUser message userId body =
  send (MentionUser message userId body)

-- | Set a platform-specific title for a group member when supported.
setMemberTitle :: ChatDriver :> es => IncomingMessage -> Text -> Text -> Eff es Bool
setMemberTitle message userId title =
  send (SetMemberTitle message userId title)

setTyping :: ChatDriver :> es => IncomingMessage -> Int -> Eff es ()
setTyping message timeout =
  send (SetTyping message timeout)

incomingMessages :: ChatDriver :> es => Stream (Of IncomingMessage) (Eff es) ()
incomingMessages = do
  stream <- lift (send IncomingMessages)
  stream

runChatDriverWithHandler
  :: ChatDriverHandler es
  -> Eff (ChatDriver : es) a
  -> Eff es a
runChatDriverWithHandler =
  interpret

chatDriverEffectHandler
  :: forall driver es.
     (Driver.ChatDriver driver, Driver.ChatDriverEffects driver es)
  => driver
  -> ChatDriverHandler es
chatDriverEffectHandler driver _ = \case
  SendReplyMessage message body ->
    Driver.sendReplyMessages driver message body
  SendMergedForward message nodes ->
    Driver.sendMergedForward driver message nodes
  SendStreamingReplyMessage message body ->
    Driver.sendStreamingReplyMessage driver message body
  ReplyAudio message audioRef caption ->
    Driver.replyAudio driver message audioRef caption
  UploadFile message path fileName ->
    Driver.uploadFile driver message path fileName
  EditMessage message messageId body ->
    Driver.editMessage driver message messageId body
  CompleteMessageEdit message messageId ->
    Driver.completeMessageEdit driver message messageId
  DeleteMessage message messageId ->
    Driver.deleteMessage driver message messageId
  RecallRecentSelfMessages message count ->
    Driver.recallRecentSelfMessages driver message count
  MessageOutPolicy message ->
    Driver.messageOutPolicy driver message
  GetMessageContent message messageId ->
    Driver.getMessageContent driver message messageId
  GetSenderMemberInfo message ->
    Driver.getSenderMemberInfo driver message
  GetMemberInfo message userId ->
    Driver.getMemberInfo driver message userId
  GetUserAvatar message userId ->
    Driver.getUserAvatar driver message userId
  ListGroupMembers message ->
    Driver.listGroupMembers driver message
  NormalizeMediaRef message ref ->
    Driver.normalizeMediaRefForMessage driver message ref
  MentionUser message userId body ->
    Driver.mentionUser driver message userId body
  SetMemberTitle message userId title ->
    Driver.setMemberTitle driver message userId title
  SetTyping message timeout ->
    Driver.setTyping driver message timeout
  IncomingMessages ->
    pure (pure ())

-- | Locally rewrite ordinary reply bodies while all platform operations still
-- delegate to the outer 'Chat' interpreter.
runChatMappingReplies
  :: ChatDriver :> es
  => (Text -> Eff es (Either Text Text))
  -> Eff es a
  -> Eff es a
runChatMappingReplies rewrite =
  interpose $ \localEnv -> \case
    SendReplyMessage message body -> do
      rewrite body >>= \case
        Left err ->
          pure [Left err]
        Right rewritten ->
          passthrough localEnv (SendReplyMessage message rewritten)
    SendStreamingReplyMessage message body -> do
      rewrite body >>= \case
        Left err ->
          pure (Left err)
        Right rewritten ->
          passthrough localEnv (SendStreamingReplyMessage message rewritten)
    operation ->
      passthrough localEnv operation

-- | Locally override chat sending so self messages can be recorded while all
-- platform operations still delegate to the outer 'Chat' interpreter.
runChatRecordingSelfMessages
  :: ChatDriver :> es
  => (Text -> Eff es ())
  -> Eff es a
  -> Eff es a
runChatRecordingSelfMessages recordSelf =
  interpose $ \localEnv -> \case
    operation@(SendReplyMessage _ body) -> do
      sent <- passthrough localEnv operation
      recordSelf body
      pure sent
    operation@(SendStreamingReplyMessage _ body) -> do
      sent <- passthrough localEnv operation
      recordSelf body
      pure sent
    operation@(MentionUser _ _ body) -> do
      sent <- passthrough localEnv operation
      recordSelf body
      pure sent
    operation ->
      passthrough localEnv operation

runChatRecordingExtraMessages
  :: ChatDriver :> es
  => (Maybe MessageId -> Eff es ())
  -> Eff es a
  -> Eff es a
runChatRecordingExtraMessages record =
  interpose $ \localEnv -> \case
    operation@SendReplyMessage{} -> do
      results <- passthrough localEnv operation
      traverse_ (record . rightToMaybe) results
      pure results
    operation@SendStreamingReplyMessage{} -> do
      result <- passthrough localEnv operation
      record (rightToMaybe result)
      pure result
    operation@MentionUser{} -> do
      result <- passthrough localEnv operation
      record (rightToMaybe result)
      pure result
    operation@ReplyAudio{} -> do
      result <- passthrough localEnv operation
      record (rightToMaybe result)
      pure result
    operation@UploadFile{} -> do
      result <- passthrough localEnv operation
      record (rightToMaybe result)
      pure result
    operation ->
      passthrough localEnv operation
