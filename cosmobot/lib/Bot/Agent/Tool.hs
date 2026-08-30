{-|
Module      : Bot.Agent.Tool
Description : Dynamic agent tools, schemas, and argument decoding
Stability   : experimental
-}
module Bot.Agent.Tool
  ( Tool
  , NamedTag (..)
  , ToolTag (..)
  , ToolRunner
  , ToolArgument
  , ParsedArguments
  , NoArguments
  , noArguments
  , parsedArguments
  , requiredArgument
  , optionalArgument
  , mapArgument
  , validateArgument
  , withDefault
  , askToolContext
  , askToolCallMetadata
  , allowWhen
  , hideUnlessM
  , mapSchemaM
  , noisy
  , resolveToolSchema
  , startTool
  , tagged
  , tool
  , toolEnableName
  , toolEnableTagRequests
  , toolAllowed
  , toolIsNoisy
  , toolName
  , toolTags
  , toolWithRunState
  , withDescription
  , withDescriptionBy
  )
where

import Bot.Agent.Types
import Bot.Core.Transcript (Transcript (..))
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Foldable as Foldable
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Reader.Static as Reader

type ToolRunner m =
  ToolCallMetadata -> Aeson.Value -> m ToolResult

data ToolCallContext = ToolCallContext
  { agentContext :: !Context
  , metadata :: !ToolCallMetadata
  }

type ToolAction es =
  Eff (Reader.Reader ToolCallContext : es) ToolResult

askToolContext
  :: Reader.Reader ToolCallContext :> es
  => Eff es Context
askToolContext =
  Reader.ask @ToolCallContext <&> (.agentContext)

askToolCallMetadata
  :: Reader.Reader ToolCallContext :> es
  => Eff es ToolCallMetadata
askToolCallMetadata =
  Reader.ask @ToolCallContext <&> (.metadata)

data NamedTag = NamedTag
  { tagName :: !Text
  , tagDescription :: !Text
  }
  deriving (Eq, Ord, Show)

data ToolTag
  = Named !NamedTag
  | Essential
  deriving (Eq, Ord, Show)

toolEnableName :: Text
toolEnableName =
  "tool_enable"

toolEnableTagRequests :: Transcript -> [[Text]]
toolEnableTagRequests (Transcript messages) =
  [ tags
    | message <- Foldable.toList messages
    , message.role == "assistant"
    , call <- message.toolCalls
    , call.name == toolEnableName
    , Just tags <- [decodeTags call.arguments]
    ]
  where
    decodeTags =
      Aeson.decodeStrict' . TextEncoding.encodeUtf8
        >=> AesonTypes.parseMaybe
          (Aeson.withObject "tool_enable arguments" (Aeson..: Key.fromText "tags"))

data ToolArgument a = ToolArgument
  { argumentName :: !Text
  , argumentSchema :: !Aeson.Value
  , argumentRequired :: !Bool
  , argumentParser :: AesonTypes.Object -> AesonTypes.Parser a
  }

data ParsedArguments a = ParsedArguments
  { parsedSchema :: !Aeson.Value
  , parsedParser :: Aeson.Value -> AesonTypes.Parser a
  }

data NoArguments = NoArguments

noArguments :: NoArguments
noArguments =
  NoArguments

requiredArgument :: Aeson.FromJSON a => (Text, Aeson.Value) -> ToolArgument a
requiredArgument (name, schema) =
  ToolArgument
    { argumentName = name
    , argumentSchema = schema
    , argumentRequired = True
    , argumentParser = (Aeson..: Key.fromText name)
    }

optionalArgument :: Aeson.FromJSON a => (Text, Aeson.Value) -> ToolArgument (Maybe a)
optionalArgument (name, schema) =
  ToolArgument
    { argumentName = name
    , argumentSchema = schema
    , argumentRequired = False
    , argumentParser = (Aeson..:? Key.fromText name)
    }

mapArgument
  :: (a -> AesonTypes.Parser b)
  -> ToolArgument a
  -> ToolArgument b
mapArgument transform argument =
  ToolArgument
    { argumentName = argument.argumentName
    , argumentSchema = argument.argumentSchema
    , argumentRequired = argument.argumentRequired
    , argumentParser = argument.argumentParser >=> transform
    }

validateArgument
  :: (a -> Either Text b)
  -> ToolArgument a
  -> ToolArgument b
validateArgument validate =
  mapArgument (either (fail . toString) pure . validate)

withDefault :: a -> ToolArgument (Maybe a) -> ToolArgument a
withDefault defaultValue =
  mapArgument (pure . fromMaybe defaultValue)

parsedArguments
  :: Aeson.Value
  -> (Aeson.Value -> AesonTypes.Parser a)
  -> ParsedArguments a
parsedArguments schema parser =
  ParsedArguments{parsedSchema = schema, parsedParser = parser}

class ToolArguments arguments where
  type ToolHandler arguments (es :: [Effect])

  toolArgumentsSchema :: arguments -> Aeson.Value

  parseToolHandler
    :: arguments
    -> ToolHandler arguments es
    -> Aeson.Value
    -> AesonTypes.Parser (ToolAction es)

instance ToolArguments NoArguments where
  type ToolHandler NoArguments es = ToolAction es

  toolArgumentsSchema _ =
    emptyObjectSchema

  parseToolHandler _ handler =
    Aeson.withObject "tool arguments" (const (pure handler))

instance ToolArguments (ToolArgument a) where
  type ToolHandler (ToolArgument a) es = a -> ToolAction es

  toolArgumentsSchema argument =
    argumentsObjectSchema [argumentField argument] (maybeToList (requiredName argument))

  parseToolHandler argument handler =
    Aeson.withObject "tool arguments" \object ->
      handler <$> argument.argumentParser object

instance ToolArguments (ToolArgument a, ToolArgument b) where
  type ToolHandler (ToolArgument a, ToolArgument b) es = a -> b -> ToolAction es

  toolArgumentsSchema (a, b) =
    argumentsObjectSchema
      [argumentField a, argumentField b]
      (catMaybes [requiredName a, requiredName b])

  parseToolHandler (a, b) handler =
    Aeson.withObject "tool arguments" \object ->
      handler <$> a.argumentParser object <*> b.argumentParser object

instance ToolArguments (ToolArgument a, ToolArgument b, ToolArgument c) where
  type ToolHandler (ToolArgument a, ToolArgument b, ToolArgument c) es =
    a -> b -> c -> ToolAction es

  toolArgumentsSchema (a, b, c) =
    argumentsObjectSchema
      [argumentField a, argumentField b, argumentField c]
      (catMaybes [requiredName a, requiredName b, requiredName c])

  parseToolHandler (a, b, c) handler =
    Aeson.withObject "tool arguments" \object ->
      handler
        <$> a.argumentParser object
        <*> b.argumentParser object
        <*> c.argumentParser object

instance ToolArguments (ToolArgument a, ToolArgument b, ToolArgument c, ToolArgument d) where
  type ToolHandler (ToolArgument a, ToolArgument b, ToolArgument c, ToolArgument d) es =
    a -> b -> c -> d -> ToolAction es

  toolArgumentsSchema (a, b, c, d) =
    argumentsObjectSchema
      [argumentField a, argumentField b, argumentField c, argumentField d]
      (catMaybes [requiredName a, requiredName b, requiredName c, requiredName d])

  parseToolHandler (a, b, c, d) handler =
    Aeson.withObject "tool arguments" \object ->
      handler
        <$> a.argumentParser object
        <*> b.argumentParser object
        <*> c.argumentParser object
        <*> d.argumentParser object

instance ToolArguments (ToolArgument a, ToolArgument b, ToolArgument c, ToolArgument d, ToolArgument e) where
  type ToolHandler (ToolArgument a, ToolArgument b, ToolArgument c, ToolArgument d, ToolArgument e) es =
    a -> b -> c -> d -> e -> ToolAction es

  toolArgumentsSchema (a, b, c, d, e) =
    argumentsObjectSchema
      [argumentField a, argumentField b, argumentField c, argumentField d, argumentField e]
      (catMaybes [requiredName a, requiredName b, requiredName c, requiredName d, requiredName e])

  parseToolHandler (a, b, c, d, e) handler =
    Aeson.withObject "tool arguments" \object ->
      handler
        <$> a.argumentParser object
        <*> b.argumentParser object
        <*> c.argumentParser object
        <*> d.argumentParser object
        <*> e.argumentParser object

instance ToolArguments (ToolArgument a, ToolArgument b, ToolArgument c, ToolArgument d, ToolArgument e, ToolArgument f) where
  type ToolHandler (ToolArgument a, ToolArgument b, ToolArgument c, ToolArgument d, ToolArgument e, ToolArgument f) es =
    a -> b -> c -> d -> e -> f -> ToolAction es

  toolArgumentsSchema (a, b, c, d, e, f) =
    argumentsObjectSchema
      [argumentField a, argumentField b, argumentField c, argumentField d, argumentField e, argumentField f]
      (catMaybes [requiredName a, requiredName b, requiredName c, requiredName d, requiredName e, requiredName f])

  parseToolHandler (a, b, c, d, e, f) handler =
    Aeson.withObject "tool arguments" \object ->
      handler
        <$> a.argumentParser object
        <*> b.argumentParser object
        <*> c.argumentParser object
        <*> d.argumentParser object
        <*> e.argumentParser object
        <*> f.argumentParser object

instance ToolArguments (ToolArgument a, ToolArgument b, ToolArgument c, ToolArgument d, ToolArgument e, ToolArgument f, ToolArgument g) where
  type ToolHandler (ToolArgument a, ToolArgument b, ToolArgument c, ToolArgument d, ToolArgument e, ToolArgument f, ToolArgument g) es =
    a -> b -> c -> d -> e -> f -> g -> ToolAction es

  toolArgumentsSchema (a, b, c, d, e, f, g) =
    argumentsObjectSchema
      [argumentField a, argumentField b, argumentField c, argumentField d, argumentField e, argumentField f, argumentField g]
      (catMaybes [requiredName a, requiredName b, requiredName c, requiredName d, requiredName e, requiredName f, requiredName g])

  parseToolHandler (a, b, c, d, e, f, g) handler =
    Aeson.withObject "tool arguments" \object ->
      handler
        <$> a.argumentParser object
        <*> b.argumentParser object
        <*> c.argumentParser object
        <*> d.argumentParser object
        <*> e.argumentParser object
        <*> f.argumentParser object
        <*> g.argumentParser object

instance ToolArguments (ParsedArguments a) where
  type ToolHandler (ParsedArguments a) es = a -> ToolAction es

  toolArgumentsSchema =
    (.parsedSchema)

  parseToolHandler arguments handler value =
    handler <$> arguments.parsedParser value

argumentField :: ToolArgument a -> (Text, Aeson.Value)
argumentField argument =
  (argument.argumentName, argument.argumentSchema)

requiredName :: ToolArgument a -> Maybe Text
requiredName argument =
  if argument.argumentRequired
    then Just argument.argumentName
    else Nothing

emptyObjectSchema :: Aeson.Value
emptyObjectSchema =
  argumentsObjectSchema [] []

argumentsObjectSchema :: [(Text, Aeson.Value)] -> [Text] -> Aeson.Value
argumentsObjectSchema fields required =
  Aeson.object
    [ "type" Aeson..= Aeson.String "object"
    , "properties" Aeson..= Aeson.object
        [ Key.fromText fieldName Aeson..= schema
        | (fieldName, schema) <- fields
        ]
    , "required" Aeson..= required
    , "additionalProperties" Aeson..= False
    ]

-- | A tool whose model-visible schema may vary between model turns.
--
-- Use the smart constructor and combinators rather than constructing this
-- value directly. A tool's name is its stable dispatch identity; schema
-- resolution may hide the tool or change its description and parameters.
data Tool m = Tool
  { name :: !Text
  , tags :: ![ToolTag]
  , schemaResolver :: Context -> Transcript -> Int -> m (Maybe LLM.FunctionTool)
  , noisyFlag :: !Bool
  , allowedPredicate :: Context -> Bool
  , runnerFactory :: Context -> m (ToolRunner m)
  }

tool
  :: ToolArguments arguments
  => Text
  -> arguments
  -> ToolHandler arguments es
  -> Tool (Eff es)
tool name arguments handler =
  toolWithRunState name arguments (const (pure ())) (const handler)

toolWithRunState
  :: ToolArguments arguments
  => Text
  -> arguments
  -> (Context -> Eff es state)
  -> (state -> ToolHandler arguments es)
  -> Tool (Eff es)
toolWithRunState name arguments initialize handler =
  Tool
    { name
    , tags = [Essential]
    , schemaResolver = \_ _ _ -> pure (Just LLM.FunctionTool
        { name
        , description = ""
        , parameters = toolArgumentsSchema arguments
        })
    , noisyFlag = False
    , allowedPredicate = const True
    , runnerFactory = \context -> do
        initialized <- initialize context
        pure \metadata rawArguments ->
          case AesonTypes.parseEither
            (parseToolHandler arguments (handler initialized))
            rawArguments of
              Left err ->
                pure (argumentFailure (toText err))
              Right action ->
                Reader.runReader ToolCallContext
                  { agentContext = context
                  , metadata
                  }
                  action
    }
  where
    argumentFailure err =
      toolFailure (permanentArgumentFailure err err)

allowWhen :: (Context -> Bool) -> Tool m -> Tool m
allowWhen predicate definition =
  definition
    { allowedPredicate = \context ->
        definition.allowedPredicate context && predicate context
    }

tagged :: [NamedTag] -> Tool m -> Tool m
tagged tags definition =
  definition{tags = map Named tags}

noisy :: Tool m -> Tool m
noisy definition =
  definition{noisyFlag = True}

mapSchemaM
  :: Monad m
  => (Context -> Transcript -> Int -> LLM.FunctionTool -> m (Maybe LLM.FunctionTool))
  -> Tool m
  -> Tool m
mapSchemaM transform definition =
  definition
    { schemaResolver = \context transcript turn -> do
        definition.schemaResolver context transcript turn >>= \case
          Nothing ->
            pure Nothing
          Just schema ->
            transform context transcript turn schema
    }

hideUnlessM
  :: Monad m
  => (Context -> Transcript -> Int -> m Bool)
  -> Tool m
  -> Tool m
hideUnlessM predicate =
  mapSchemaM \context transcript turn schema ->
    predicate context transcript turn <&> \visible ->
      if visible then Just schema else Nothing

withDescription :: Monad m => Text -> Tool m -> Tool m
withDescription =
  withDescriptionBy . const

withDescriptionBy :: Monad m => (Context -> Text) -> Tool m -> Tool m
withDescriptionBy description =
  mapSchemaM \context _ _ schema ->
    pure (Just schema{LLM.description = description context})

toolName :: Tool m -> Text
toolName Tool{name} =
  name

toolTags :: Tool m -> [ToolTag]
toolTags Tool{tags} =
  tags

resolveToolSchema
  :: Functor m
  => Tool m
  -> Context
  -> Transcript
  -> Int
  -> m (Maybe LLM.FunctionTool)
resolveToolSchema Tool{name, schemaResolver} context transcript turn =
  fmap (fmap \schema -> LLM.FunctionTool
    { name
    , description = schema.description
    , parameters = schema.parameters
    })
    (schemaResolver context transcript turn)

toolIsNoisy :: Tool m -> Bool
toolIsNoisy Tool{noisyFlag} =
  noisyFlag

toolAllowed :: Tool m -> Context -> Bool
toolAllowed Tool{allowedPredicate} =
  allowedPredicate

startTool :: Tool m -> Context -> m (ToolRunner m)
startTool Tool{runnerFactory} =
  runnerFactory
