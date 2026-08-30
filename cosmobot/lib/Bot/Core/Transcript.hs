{-|
Module      : Bot.Core.Transcript
Description : Core LLM transcript value
Stability   : experimental
-}

module Bot.Core.Transcript
  ( Transcript (..)
  , startWithUser
  , startWithUserContext
  , startWithUserInput
  , startWithSystemAndUser
  , startWithSystemAndUserContext
  , startWithSystemAndUserInput
  , startWithIncomingMessage
  , appendUser
  , appendUserContext
  , appendUserInput
  , appendIncomingMessage
  , appendAssistant
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import Bot.Util.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

-- | Platform-neutral LLM message history.
--
-- A 'Transcript' is the exact ordered message context that will be sent back
-- to the LLM when a user continues a thread. It deliberately does not contain
-- chat ids, message ids, parent links, active stream handles, or persistence
-- details; those belong to the storage layer that indexes and reloads threads
-- for a concrete bot runtime.
newtype Transcript = Transcript
  { messages :: Seq.Seq LLM.ChatMessage
  }
  deriving (Show, Generic)
    deriving (Aeson.ToJSON, Aeson.FromJSON) via (SnakeJSON Transcript)

-- | Start a transcript from a single text-only user prompt.
startWithUser :: Text -> Transcript
startWithUser prompt =
  startWithSystemAndUserContext "" prompt []

-- | Start a transcript from user text plus image URLs.
--
-- Image URLs are encoded into the initial user message because the LLM API sees
-- them as part of the same user turn, not as independent bot state.
startWithUserContext :: Text -> [Text] -> Transcript
startWithUserContext prompt imageUrls =
  startWithUserInput (inputWithImages prompt imageUrls)

startWithUserInput :: MessageInput -> Transcript
startWithUserInput =
  startWithSystemAndUserInput ""

-- | Start a transcript with a program-generated identity envelope.
startWithIncomingMessage :: IncomingMessage -> MessageInput -> Transcript
startWithIncomingMessage message input =
  startWithUserInput (inputWithIdentityEnvelope message input)

-- | Start with a system prompt followed by a text-only user prompt.
--
-- The system prompt is omitted entirely when it is empty, so callers do not
-- create blank system messages by accident.
startWithSystemAndUser :: Text -> Text -> Transcript
startWithSystemAndUser systemPrompt prompt =
  startWithSystemAndUserContext systemPrompt prompt []

-- | Start with optional system instructions plus the first user turn.
--
-- This is the most general constructor. The resulting sequence is always
-- system messages first, then exactly one user message containing the prompt
-- and any image context.
startWithSystemAndUserContext :: Text -> Text -> [Text] -> Transcript
startWithSystemAndUserContext systemPrompt prompt imageUrls =
  startWithSystemAndUserInput systemPrompt (inputWithImages prompt imageUrls)

startWithSystemAndUserInput :: Text -> MessageInput -> Transcript
startWithSystemAndUserInput systemPrompt input =
  Transcript (Seq.fromList (systemMessages <> [LLM.userWithImages input.text (messageInputImageUrls input)]))
  where
    systemMessages
      | Text.null systemPrompt = []
      | otherwise         = [LLM.systemText systemPrompt]

-- | Append a text-only user turn to an existing transcript.
appendUser :: Text -> Transcript -> Transcript
appendUser prompt =
  appendUserContext prompt []

-- | Append a user turn with optional image context.
--
-- Continuations use this when a reply adds a new prompt to an already persisted
-- transcript. Parent/child thread linkage is intentionally handled outside this
-- value.
appendUserContext :: Text -> [Text] -> Transcript -> Transcript
appendUserContext prompt imageUrls (Transcript history) =
  appendUserInput (inputWithImages prompt imageUrls) (Transcript history)

appendUserInput :: MessageInput -> Transcript -> Transcript
appendUserInput input (Transcript history) =
  Transcript (history Seq.|> LLM.userWithImages input.text (messageInputImageUrls input))

-- | Append a real incoming turn while preserving who sent it.
appendIncomingMessage :: IncomingMessage -> MessageInput -> Transcript -> Transcript
appendIncomingMessage message input transcript =
  appendUserInput (inputWithIdentityEnvelope message input) transcript

inputWithIdentityEnvelope :: IncomingMessage -> MessageInput -> MessageInput
inputWithIdentityEnvelope message input =
  input { text = renderIdentityEnvelope message input.text }

renderIdentityEnvelope :: IncomingMessage -> Text -> Text
renderIdentityEnvelope message body =
  Text.intercalate "\n"
    [ "<fm_message_context>"
    , "platform=" <> metadataValue (Text.pack (show message.platform))
    , "chat_kind=" <> metadataValue (Text.pack (show message.kind))
    , "chat_id=" <> metadataMaybe (Text.pack . show <$> message.chatId)
    , "sender_id=" <> metadataMaybe message.senderId
    , "sender_name=" <> metadataMaybe message.senderUsername
    , "message_id=" <> metadataMaybe (messageIdText <$> message.messageId)
    , "reply_to_message_id=" <> metadataMaybe (messageIdText <$> message.replyToMessageId)
    , "mentions_bot=" <> metadataValue (if message.digest.mentionsBot then "true" else "false")
    , "mention_count=" <> metadataValue (Text.pack (show (length message.mentions)))
    , "has_images=" <> metadataValue (if null message.imageUrls then "false" else "true")
    , "image_count=" <> metadataValue (Text.pack (show (length message.imageUrls)))
    , "has_files=" <> metadataValue (if null message.files then "false" else "true")
    , "file_count=" <> metadataValue (Text.pack (show (length message.files)))
    , "</fm_message_context>"
    , "<fm_user_message>"
    , body
    , "</fm_user_message>"
    ]
  where
    metadataMaybe = maybe "unavailable" metadataValue
    metadataValue =
      Text.replace "\\" "\\\\"
      . Text.replace "\r" "\\r"
      . Text.replace "\n" "\\n"
      . Text.replace "<" "\\<"
      . Text.replace ">" "\\>"

-- | Append an assistant reply.
--
-- Generated image references are preserved as a synthetic follow-up user
-- context message. That makes image results available to later turns even
-- though the assistant message itself is text-oriented in the OpenAI-compatible
-- chat format we use here.
appendAssistant :: Text -> Transcript -> Transcript
appendAssistant answer (Transcript history) =
  Transcript (history <> Seq.fromList (assistantContext answer))

assistantContext :: Text -> [LLM.ChatMessage]
assistantContext answer =
  assistantTextContext <> imageContext
  where
    answerText = Chat.renderReplyBody answer
    imageUrls = Chat.replyImageUrls answer
    contextImageUrls = filter (not . Chat.isBase64ImageRef) imageUrls
    assistantTextContext =
      [ LLM.assistantText answerText | not (Text.null answerText) ] <>
      [ LLM.assistantText "Generated image." | Text.null answerText && not (null imageUrls) ]
    imageContext =
      [ LLM.userWithImages "The previous assistant response generated this image. Use it as visual context for follow-up questions." contextImageUrls
      | not (null contextImageUrls)
      ]
