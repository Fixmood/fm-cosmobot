{-|
Module      : Bot.Chat.Adapter
Description : Adapter-level outgoing chat message delivery
Stability   : experimental
-}

module Bot.Chat.Adapter
  ( replyTo
  , streamReplyTo
  , streamMultipleRepliesTo
  )
where

import Bot.Core.Message (IncomingMessage(..), MessageId)
import Bot.Chat.Types
import qualified Bot.Effect.ChatDriver as ChatDriver
import Bot.Prelude hiding (state)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Streaming
import qualified Streaming.Prelude as S

-- | Reply to the chat containing the incoming message.
replyTo :: ChatDriver.ChatDriver :> es => IncomingMessage -> Text -> Eff es [Either Text MessageId]
replyTo _ body | Text.null body =
  pure []
replyTo message body = do
  policy <- ChatDriver.messageOutPolicy message
  let messageLimit = messageOutPolicyLimit policy
      state = appendAnswer body emptyMessageOutState
  (_, sent) <- sendTextChunks messageLimit message state (answerText state)
  pure sent

streamReplyTo
  :: ChatDriver.ChatDriver :> es
  => IncomingMessage
  -> Stream (Of Text) (Eff es) r
  -> Stream (Of MessageOutResult) (Eff es) (MessageOutResult, r)
streamReplyTo message input = do
  policy <- lift (ChatDriver.messageOutPolicy message)
  (lastResult, result) <- streamOneReply message policy input
  pure (fromMaybe emptyMessageOutResult lastResult, result)

streamMultipleRepliesTo
  :: ChatDriver.ChatDriver :> es
  => IncomingMessage
  -> Stream (Stream (Of Text) (Eff es)) (Eff es) r
  -> Stream (Of MessageOutResult) (Eff es) (MessageOutResult, r)
streamMultipleRepliesTo message input = do
  policy <- lift (ChatDriver.messageOutPolicy message)
  go policy Nothing input
  where
    go policy lastResult segments = do
      inspected <- lift (Streaming.inspect segments)
      case inspected of
        Left result ->
          pure (fromMaybe emptyMessageOutResult lastResult, result)
        Right segment -> do
          (segmentResult, rest) <- streamOneReply message policy segment
          go policy (segmentResult <|> lastResult) rest

data MessageOutState = MessageOutState
  { answerBuilder :: !TextBuilder.Builder
  , sentOffset :: !Int
  , firstMessageId :: !(Maybe MessageId)
  , lastChunkMessageId :: !(Maybe MessageId)
  , lastEditOffset :: !Int
  , lastEditedBody :: !Text
  }

emptyMessageOutState :: MessageOutState
emptyMessageOutState =
  MessageOutState
    { answerBuilder = mempty
    , sentOffset = 0
    , firstMessageId = Nothing
    , lastChunkMessageId = Nothing
    , lastEditOffset = 0
    , lastEditedBody = ""
    }

emptyMessageOutResult :: MessageOutResult
emptyMessageOutResult =
  MessageOutResult
    { responseId = Nothing
    , sentMessageResults = []
    , answer = ""
    }

messageOutResult :: MessageOutState -> [Either Text MessageId] -> MessageOutResult
messageOutResult state sentMessageResults =
  MessageOutResult
    { responseId = state.firstMessageId
    , sentMessageResults
    , answer = answerText state
    }

messageOutPolicyLimit :: MessageOutPolicy -> Int
messageOutPolicyLimit = \case
  EditableMessage _ messageLimit ->
    messageLimit
  ChunkedMessage messageLimit ->
    messageLimit

streamOneReply
  :: ChatDriver.ChatDriver :> es
  => IncomingMessage
  -> MessageOutPolicy
  -> Stream (Of Text) (Eff es) r
  -> Stream (Of MessageOutResult) (Eff es) (Maybe MessageOutResult, r)
streamOneReply message = \case
  EditableMessage editChunkChars messageLimit ->
    streamReply
      (pushEditableReplyChunk message editChunkChars messageLimit)
      (finishEditableReply message messageLimit)
      emptyMessageOutState
      False
  ChunkedMessage messageLimit ->
    streamReply
      (pushChunkedReplyChunk message messageLimit)
      (finishChunkedReply message messageLimit)
      emptyMessageOutState
      False

streamReply
  :: ChatDriver.ChatDriver :> es
  => (MessageOutState -> Text -> Text -> Eff es (MessageOutState, [Either Text MessageId]))
  -> (MessageOutState -> Eff es (MessageOutState, [Either Text MessageId]))
  -> MessageOutState
  -> Bool
  -> Stream (Of Text) (Eff es) r
  -> Stream (Of MessageOutResult) (Eff es) (Maybe MessageOutResult, r)
streamReply push finish state hasChunks input = do
  next <- lift (S.next input)
  case next of
    Left result
      | hasChunks -> do
          (finished, sent) <- lift (finish state)
          let update = messageOutResult finished sent
          S.yield update
          pure (Just update, result)
      | otherwise ->
          pure (Nothing, result)
    Right (chunk, rest)
      | Text.null chunk ->
          streamReply push finish state hasChunks rest
    Right (chunk, rest) -> do
      let stateWithAnswer = appendAnswer chunk state
          body = answerText stateWithAnswer
      (updated, sent) <- lift (push stateWithAnswer chunk body)
      S.yield (messageOutResult updated sent)
      streamReply push finish updated True rest

pushEditableReplyChunk
  :: ChatDriver.ChatDriver :> es
  => IncomingMessage
  -> Int
  -> Int
  -> MessageOutState
  -> Text
  -> Text
  -> Eff es (MessageOutState, [Either Text MessageId])
pushEditableReplyChunk message editChunkChars messageLimit state _ body = do
  (stateWithMessage, sent) <- ensureEditableReply message state (editableBody messageLimit body)
  case stateWithMessage.firstMessageId of
    Just messageId
      | Text.length body - stateWithMessage.lastEditOffset >= editChunkChars -> do
          edited <- editReplyIfChanged message stateWithMessage messageId (editableBody messageLimit body)
          pure (edited{lastEditOffset = Text.length body}, sent)
    _ ->
      pure (stateWithMessage, sent)

finishEditableReply
  :: ChatDriver.ChatDriver :> es
  => IncomingMessage
  -> Int
  -> MessageOutState
  -> Eff es (MessageOutState, [Either Text MessageId])
finishEditableReply message messageLimit state = do
  let finalBody = answerText state
      (editableText, overflow) = Text.splitAt messageLimit finalBody
  (stateWithMessage, sentFirst) <- ensureEditableReply message state editableText
  stateAfterEdit <- case stateWithMessage.firstMessageId of
    Nothing ->
      pure stateWithMessage
    Just messageId ->
      editReplyIfChanged message stateWithMessage messageId editableText
        >>= completeEditableReply message messageId
  if Text.null overflow
    then pure (stateAfterEdit, sentFirst)
    else do
      let target = maybe (chunkTarget message stateAfterEdit) (editableTailTarget message stateAfterEdit) stateWithMessage.firstMessageId
      (finished, sentTail) <- sendTextChunks messageLimit target stateAfterEdit overflow
      pure (finished, sentFirst <> sentTail)

ensureEditableReply
  :: ChatDriver.ChatDriver :> es
  => IncomingMessage
  -> MessageOutState
  -> Text
  -> Eff es (MessageOutState, [Either Text MessageId])
ensureEditableReply message state body =
  case state.firstMessageId of
    Just{} ->
      pure (state, [])
    Nothing -> do
      sent <- ChatDriver.sendStreamingReplyMessage message (initialEditableBody body)
      pure (recordInitialEdit state (initialEditableBody body) sent, [sent])

recordInitialEdit :: MessageOutState -> Text -> Either Text MessageId -> MessageOutState
recordInitialEdit state body sent =
  case sent of
    Left{} ->
      state
    Right messageId ->
      state
        { firstMessageId = Just messageId
        , lastChunkMessageId = Just messageId
        , lastEditOffset = Text.length body
        , lastEditedBody = body
        }

editReplyIfChanged
  :: ChatDriver.ChatDriver :> es
  => IncomingMessage
  -> MessageOutState
  -> MessageId
  -> Text
  -> Eff es MessageOutState
editReplyIfChanged message state messageId body
  | body == state.lastEditedBody =
      pure state
  | otherwise = do
      edited <- ChatDriver.editMessage message messageId body
      pure if edited then state{lastEditedBody = body} else state

completeEditableReply
  :: ChatDriver.ChatDriver :> es
  => IncomingMessage
  -> MessageId
  -> MessageOutState
  -> Eff es MessageOutState
completeEditableReply message messageId state = do
  _ <- ChatDriver.completeMessageEdit message messageId
  pure state

editableBody :: Int -> Text -> Text
editableBody messageLimit =
  Text.take messageLimit . initialEditableBody

initialEditableBody :: Text -> Text
initialEditableBody body
  | Text.null body = "..."
  | otherwise = body

pushChunkedReplyChunk
  :: ChatDriver.ChatDriver :> es
  => IncomingMessage
  -> Int
  -> MessageOutState
  -> Text
  -> Text
  -> Eff es (MessageOutState, [Either Text MessageId])
pushChunkedReplyChunk message messageLimit state _ _ =
  sendReadyChunks message messageLimit state

finishChunkedReply
  :: ChatDriver.ChatDriver :> es
  => IncomingMessage
  -> Int
  -> MessageOutState
  -> Eff es (MessageOutState, [Either Text MessageId])
finishChunkedReply message messageLimit state
  | Text.null pending =
      pure (state, [])
  | otherwise =
      sendTextChunks messageLimit (chunkTarget message state) state pending
  where
    pending =
      Text.drop state.sentOffset (answerText state)

sendReadyChunks
  :: ChatDriver.ChatDriver :> es
  => IncomingMessage
  -> Int
  -> MessageOutState
  -> Eff es (MessageOutState, [Either Text MessageId])
sendReadyChunks message messageLimit state =
  sendChunksWhile
    (\pending -> Text.length pending >= messageLimit)
    messageLimit
    (chunkTarget message state)
    state
    (Text.drop state.sentOffset (answerText state))

sendTextChunks
  :: ChatDriver.ChatDriver :> es
  => Int
  -> IncomingMessage
  -> MessageOutState
  -> Text
  -> Eff es (MessageOutState, [Either Text MessageId])
sendTextChunks =
  sendChunksWhile (\pending -> Text.length pending > 0)

sendChunksWhile
  :: ChatDriver.ChatDriver :> es
  => (Text -> Bool)
  -> Int
  -> IncomingMessage
  -> MessageOutState
  -> Text
  -> Eff es (MessageOutState, [Either Text MessageId])
sendChunksWhile shouldSend messageLimit target state pending
  | shouldSend pending = do
      let (body, rest) = Text.splitAt messageLimit pending
      sent <- ChatDriver.sendReplyMessage target body
      let stateAfterSend = (recordSentMessages state sent){sentOffset = state.sentOffset + Text.length body}
          nextTarget = chunkTarget target stateAfterSend
      (finished, later) <- sendChunksWhile shouldSend messageLimit nextTarget stateAfterSend rest
      pure (finished, sent <> later)
  | otherwise =
      pure (state, [])

recordSentMessage :: MessageOutState -> Either Text MessageId -> MessageOutState
recordSentMessage state = \case
  Left{} ->
    state
  Right messageId ->
    state
      { firstMessageId = state.firstMessageId <|> Just messageId
      , lastChunkMessageId = Just messageId
      }

recordSentMessages :: MessageOutState -> [Either Text MessageId] -> MessageOutState
recordSentMessages =
  foldl' recordSentMessage

appendAnswer :: Text -> MessageOutState -> MessageOutState
appendAnswer chunk state =
  state{answerBuilder = state.answerBuilder <> TextBuilder.fromText chunk}

answerText :: MessageOutState -> Text
answerText =
  builderText . (.answerBuilder)

builderText :: TextBuilder.Builder -> Text
builderText =
  LazyText.toStrict . TextBuilder.toLazyText

chunkTarget :: IncomingMessage -> MessageOutState -> IncomingMessage
chunkTarget message state =
  maybe message (\messageId -> message{messageId = Just messageId}) state.lastChunkMessageId

editableTailTarget :: IncomingMessage -> MessageOutState -> MessageId -> IncomingMessage
editableTailTarget message state messageId =
  message{messageId = Just (fromMaybe messageId state.lastChunkMessageId)}
