{-|
Module      : Bot.Effect.LLM
Description : LLM capability facade
Stability   : experimental
-}

module Bot.Effect.LLM
  ( -- * Effect
    LLM (..)
  , ask
  , askWithHistory
  , askStreamingWithHistory
  , askImageWithHistory
  , askImageWithHistoryWithOptions
  , askImageStreamingWithHistory
  , askImageStreamingWithHistoryWithOptions
  , askImageEditStreaming
  , askImageEditStreamingWithOptions
  , askAudioWithHistory
  , askAudioWithHistoryWithOptions
  , askAudioStreamingWithHistory
  , askAudioStreamingWithHistoryWithOptions
  , askWithTools
  , askWithToolsStreaming
  , listChatModels
  , currentChatModel
  , queryAccountBalance
  , selectChatModel
  , probeChatModel
  , resetChatModel
  , addChatModel
  , editChatModel
  , deleteChatModel
  , liftLocalStream
  , LLMException (..)
  , llmExceptionSummary
  , ChatModelInfo (..)
  , ChatModelConfig (..)
  , ChatModelPatch (..)
  , AccountBalance (..)
  , AccountBalanceResult (..)

    -- * Transcript values
  , ImageRequestOptions (..)
  , defaultImageRequestOptions
  , AudioRequestOptions (..)
  , defaultAudioRequestOptions
  , ChatMessage (..)
  , MessageContent (..)
  , ContentPart (..)
  , ChatAnswer (..)
  , TokenUsage (..)
  , FunctionTool (..)
  , ToolCall (..)
  , chatAnswer
  , chatAnswerTokenUsage
  , withChatAnswerTokenUsage
  , userText
  , userWithImages
  , systemText
  , contextSystemPrompt
  , memorySystemPrompt
  , assistantText
  , assistantAnswer
  , toolResult
  )
where

import Bot.Prelude hiding (ask)
import Bot.LLM.Types
import qualified Streaming.Prelude as S

-- | Effect for text, image, and tool-calling LLM requests.
data LLM :: Effect where
  AskStream :: [ChatMessage] -> LLM m (Stream (Of Text) m Text)
  AskImageStream :: ImageRequestOptions -> [ChatMessage] -> LLM m (Stream (Of Text) m Text)
  AskImageEditStream :: ImageRequestOptions -> Text -> [Text] -> Maybe Text -> LLM m (Stream (Of Text) m Text)
  AskAudioStream :: AudioRequestOptions -> [ChatMessage] -> LLM m (Stream (Of Text) m Text)
  AskToolsStream :: [FunctionTool] -> [ChatMessage] -> LLM m (Stream (Of Text) m ChatAnswer)
  ListChatModels :: LLM m [ChatModelInfo]
  CurrentChatModel :: LLM m (Maybe ChatModelInfo)
  QueryAccountBalance :: Maybe Text -> LLM m [AccountBalanceResult]
  SelectChatModel :: Text -> LLM m (Either Text ChatModelInfo)
  ProbeChatModel :: Text -> LLM m (Either Text ())
  ResetChatModel :: LLM m (Maybe ChatModelInfo)
  AddChatModel :: ChatModelConfig -> LLM m (Either Text ())
  EditChatModel :: Text -> ChatModelPatch -> LLM m (Either Text ())
  DeleteChatModel :: Text -> LLM m (Either Text ())

type instance DispatchOf LLM = Dynamic

data ChatModelInfo = ChatModelInfo
  { provider :: !Text
  , model :: !Text
  , current :: !Bool
  , configuredDefault :: !Bool
  }
  deriving (Eq, Show)

data ChatModelConfig = ChatModelConfig
  { profileName :: !Text
  , baseUrl :: !Text
  , apiKey :: !Text
  , model :: !Text
  , reasoningEffort :: !Text
  , requestTimeout :: !Int
  }
  deriving (Eq, Show)

data ChatModelPatch = ChatModelPatch
  { newProfileName :: !(Maybe Text)
  , newBaseUrl :: !(Maybe Text)
  , newApiKey :: !(Maybe Text)
  , newModel :: !(Maybe Text)
  , newReasoningEffort :: !(Maybe Text)
  , newRequestTimeout :: !(Maybe Int)
  }
  deriving (Eq, Show)

data AccountBalance = AccountBalance
  { available :: !Bool
  , fields :: ![(Text, Text)]
  }
  deriving (Eq, Show)

data AccountBalanceResult = AccountBalanceResult
  { sourceName :: !Text
  , queryResult :: !(Either Text AccountBalance)
  }
  deriving (Eq, Show)

-- | Optional per-request image controls supported by image-generation providers.
data ImageRequestOptions = ImageRequestOptions
  { quality :: !(Maybe Text)
  , size :: !(Maybe Text)
  , background :: !(Maybe Text)
  , moderation :: !(Maybe Text)
  }
  deriving (Eq, Show)

defaultImageRequestOptions :: ImageRequestOptions
defaultImageRequestOptions =
  ImageRequestOptions
    { quality = Nothing
    , size = Nothing
    , background = Nothing
    , moderation = Nothing
    }

-- | Optional per-request audio controls supported by audio-generation providers.
data AudioRequestOptions = AudioRequestOptions
  { voice :: !(Maybe Text)
  , responseFormat :: !(Maybe Text)
  , speed :: !(Maybe Double)
  , instructions :: !(Maybe Text)
  }
  deriving (Eq, Show)

defaultAudioRequestOptions :: AudioRequestOptions
defaultAudioRequestOptions =
  AudioRequestOptions
    { voice = Nothing
    , responseFormat = Nothing
    , speed = Nothing
    , instructions = Nothing
    }

-- | Ask a one-shot text question without preserving history.
ask :: LLM :> es => Text -> Eff es Text
ask prompt = askWithHistory [userText prompt]

-- | Ask for a text answer using an explicit chat history.
askWithHistory :: LLM :> es => [ChatMessage] -> Eff es Text
askWithHistory messages =
  S.effects (askStreamingWithHistory messages)

-- | Ask for a text answer while receiving response chunks.
askStreamingWithHistory :: LLM :> es => [ChatMessage] -> Stream (Of Text) (Eff es) Text
askStreamingWithHistory messages = do
  stream <- lift (send (AskStream messages))
  stream

-- | Ask the configured image model to generate an image response.
askImageWithHistory :: LLM :> es => [ChatMessage] -> Eff es Text
askImageWithHistory messages =
  askImageWithHistoryWithOptions defaultImageRequestOptions messages

-- | Ask the configured image model to generate an image response with per-call options.
askImageWithHistoryWithOptions :: LLM :> es => ImageRequestOptions -> [ChatMessage] -> Eff es Text
askImageWithHistoryWithOptions options messages =
  S.effects (askImageStreamingWithHistoryWithOptions options messages)

-- | Ask the configured image model to generate an image response over the streaming transport.
askImageStreamingWithHistory :: LLM :> es => [ChatMessage] -> Stream (Of Text) (Eff es) Text
askImageStreamingWithHistory messages = do
  askImageStreamingWithHistoryWithOptions defaultImageRequestOptions messages

-- | Ask the configured image model to generate an image response over the streaming transport with per-call options.
askImageStreamingWithHistoryWithOptions :: LLM :> es => ImageRequestOptions -> [ChatMessage] -> Stream (Of Text) (Eff es) Text
askImageStreamingWithHistoryWithOptions options messages = do
  stream <- lift (send (AskImageStream options messages))
  stream

-- | Ask the configured image model to edit one or more input images over the streaming transport.
askImageEditStreaming :: LLM :> es => Text -> [Text] -> Maybe Text -> Stream (Of Text) (Eff es) Text
askImageEditStreaming prompt imageRefs maskRef =
  askImageEditStreamingWithOptions defaultImageRequestOptions prompt imageRefs maskRef

-- | Ask the configured image model to edit one or more input images over the streaming transport with per-call options.
askImageEditStreamingWithOptions :: LLM :> es => ImageRequestOptions -> Text -> [Text] -> Maybe Text -> Stream (Of Text) (Eff es) Text
askImageEditStreamingWithOptions options prompt imageRefs maskRef = do
  stream <- lift (send (AskImageEditStream options prompt imageRefs maskRef))
  stream

-- | Ask the configured audio model to generate an audio response.
askAudioWithHistory :: LLM :> es => [ChatMessage] -> Eff es Text
askAudioWithHistory messages =
  askAudioWithHistoryWithOptions defaultAudioRequestOptions messages

-- | Ask the configured audio model to generate an audio response with per-call options.
askAudioWithHistoryWithOptions :: LLM :> es => AudioRequestOptions -> [ChatMessage] -> Eff es Text
askAudioWithHistoryWithOptions options messages =
  S.effects (askAudioStreamingWithHistoryWithOptions options messages)

-- | Ask the configured audio model to generate an audio response over the streaming transport.
askAudioStreamingWithHistory :: LLM :> es => [ChatMessage] -> Stream (Of Text) (Eff es) Text
askAudioStreamingWithHistory messages = do
  askAudioStreamingWithHistoryWithOptions defaultAudioRequestOptions messages

-- | Ask the configured audio model to generate an audio response over the streaming transport with per-call options.
askAudioStreamingWithHistoryWithOptions :: LLM :> es => AudioRequestOptions -> [ChatMessage] -> Stream (Of Text) (Eff es) Text
askAudioStreamingWithHistoryWithOptions options messages = do
  stream <- lift (send (AskAudioStream options messages))
  stream

-- | Ask with function tools and return both text and tool calls.
askWithTools :: LLM :> es => [FunctionTool] -> [ChatMessage] -> Eff es ChatAnswer
askWithTools tools messages =
  S.effects (askWithToolsStreaming tools messages)

-- | Ask with function tools while receiving assistant content chunks.
askWithToolsStreaming :: LLM :> es => [FunctionTool] -> [ChatMessage] -> Stream (Of Text) (Eff es) ChatAnswer
askWithToolsStreaming tools messages = do
  stream <- lift (send (AskToolsStream tools messages))
  stream

listChatModels :: LLM :> es => Eff es [ChatModelInfo]
listChatModels =
  send ListChatModels

currentChatModel :: LLM :> es => Eff es (Maybe ChatModelInfo)
currentChatModel =
  send CurrentChatModel

queryAccountBalance :: LLM :> es => Maybe Text -> Eff es [AccountBalanceResult]
queryAccountBalance =
  send . QueryAccountBalance

selectChatModel :: LLM :> es => Text -> Eff es (Either Text ChatModelInfo)
selectChatModel =
  send . SelectChatModel

probeChatModel :: LLM :> es => Text -> Eff es (Either Text ())
probeChatModel =
  send . ProbeChatModel

resetChatModel :: LLM :> es => Eff es (Maybe ChatModelInfo)
resetChatModel =
  send ResetChatModel

addChatModel :: LLM :> es => ChatModelConfig -> Eff es (Either Text ())
addChatModel =
  send . AddChatModel

editChatModel :: LLM :> es => Text -> ChatModelPatch -> Eff es (Either Text ())
editChatModel target patch =
  send (EditChatModel target patch)

deleteChatModel :: LLM :> es => Text -> Eff es (Either Text ())
deleteChatModel =
  send . DeleteChatModel

liftLocalStream
  :: Monad m
  => (forall x. Eff es x -> m x)
  -> Stream (Of a) (Eff es) r
  -> Stream (Of a) m r
liftLocalStream liftLocal stream = do
  next <- lift (liftLocal (S.next stream))
  case next of
    Left result ->
      pure result
    Right (chunk, rest) -> do
      S.yield chunk
      liftLocalStream liftLocal rest
