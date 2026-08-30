{-|
Module      : Bot.Agent.ToolRegistry
Description : Per-run tool registry and tool-call dispatch
Stability   : experimental
-}

module Bot.Agent.ToolRegistry
  ( RunningTool (..)
  , claimToolAnnouncement
  , enabledToolGroups
  , resolveToolSchemas
  , runToolCall
  , startToolRun
  )
where

import Bot.Agent.Tool
import Bot.Agent.Types
import Bot.Core.Transcript (Transcript)
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Effectful.Concurrent.MVar as MVar
import System.IO.Error (userError)

-- | A tool runner bound to one agent run.
data RunningTool m = RunningTool
  { name  :: !Text
  , tags :: ![ToolTag]
  , noisy :: !Bool
  , announcementClaimed :: !(MVar.MVar Bool)
  , currentSchema :: !(MVar.MVar (Maybe LLM.FunctionTool))
  , resolveSchema :: Transcript -> Int -> m (Maybe LLM.FunctionTool)
  , run  :: ToolCallMetadata -> Aeson.Value -> m ToolResult
  }

-- | Start a tool for this agent run.
startToolRun :: Concurrent :> es => Context -> Tool (Eff es) -> Eff es (RunningTool (Eff es))
startToolRun context definition = do
  announcementClaimed <- MVar.newMVar False
  currentSchema <- MVar.newMVar Nothing
  run <- startTool definition context
  let name = toolName definition
      tags = toolTags definition
      toolNoisy = toolIsNoisy definition
      resolveSchema = resolveToolSchema definition context
  pure RunningTool{name, tags, noisy = toolNoisy, announcementClaimed, currentSchema, resolveSchema, run}

-- | Atomically grant the first visible progress announcement for this tool in
-- one agent runtime. The runtime is reused by bounded automatic continuation,
-- so repeated calls remain quiet across continuation boundaries as well.
claimToolAnnouncement :: Concurrent :> es => RunningTool (Eff es) -> Eff es Bool
claimToolAnnouncement runningTool =
  MVar.modifyMVar runningTool.announcementClaimed \alreadyClaimed ->
    pure (True, not alreadyClaimed)

-- | Resolve and freeze the schemas visible in one model turn.
resolveToolSchemas
  :: Concurrent :> es
  => Transcript
  -> Int
  -> [RunningTool (Eff es)]
  -> Eff es [LLM.FunctionTool]
resolveToolSchemas transcript turn runningTools = do
  schemas <- catMaybes <$> traverse resolve runningTools
  case duplicateNames schemas of
    [] ->
      pure schemas
    duplicates ->
      throwIO (userError [i|Duplicate resolved tool names: #{Text.intercalate ", " duplicates}|])
  where
    enabledTags =
      enabledTagNames transcript runningTools

    resolve runningTool = do
      resolved <-
        if toolTagsVisible enabledTags runningTool.tags
          then runningTool.resolveSchema transcript turn
          else pure Nothing
      let schema
            | runningTool.name == toolEnableName =
                toolEnableSchema runningTools <$> resolved
            | otherwise =
                resolved
      MVar.modifyMVar_ runningTool.currentSchema (const (pure schema))
      pure schema

enabledToolGroups :: Transcript -> [RunningTool m] -> [(Text, Int)]
enabledToolGroups transcript runningTools =
  ("essential", countTag Essential)
    : [ (tag.tagName, countTag (Named tag))
      | tag <- availableNamedTags runningTools
      , Set.member tag.tagName enabledTags
      ]
  where
    enabledTags =
      enabledTagNames transcript runningTools

    countTag tag =
      length [() | runningTool <- runningTools, tag `elem` runningTool.tags]

enabledTagNames :: Transcript -> [RunningTool m] -> Set.Set Text
enabledTagNames transcript runningTools =
  Set.fromList
    [ tag
    | request <- toolEnableTagRequests transcript
    , not (null request)
    , all (`Set.member` availableTagNames runningTools) request
    , tag <- request
    ]

toolTagsVisible :: Set.Set Text -> [ToolTag] -> Bool
toolTagsVisible enabledTags =
  any \case
    Essential ->
      True
    Named tag ->
      Set.member tag.tagName enabledTags

availableNamedTags :: [RunningTool m] -> [NamedTag]
availableNamedTags runningTools =
  ordNub
    [ tag
    | runningTool <- runningTools
    , Named tag <- runningTool.tags
    ]

availableTagNames :: [RunningTool m] -> Set.Set Text
availableTagNames =
  Set.fromList . map (.tagName) . availableNamedTags

toolEnableSchema :: [RunningTool m] -> LLM.FunctionTool -> LLM.FunctionTool
toolEnableSchema runningTools schema =
  schema
    { LLM.description =
        Text.unlines
          ( "Enable additional tool tags for the current thread. Enable every tag you expect to need as early as possible, preferably in one call. Enabled tags remain active in later turns."
          : "Available tags:"
          : map renderTag tags
          )
    , LLM.parameters =
        Aeson.object
          [ "type" Aeson..= Aeson.String "object"
          , "properties" Aeson..= Aeson.object
              [ "tags" Aeson..= Aeson.object
                  [ "type" Aeson..= Aeson.String "array"
                  , "items" Aeson..= Aeson.object
                      [ "type" Aeson..= Aeson.String "string"
                      , "enum" Aeson..= map (.tagName) tags
                      ]
                  , "minItems" Aeson..= (1 :: Int)
                  , "description" Aeson..= ("Tool tags to enable." :: Text)
                  ]
              ]
          , "required" Aeson..= (["tags"] :: [Text])
          , "additionalProperties" Aeson..= False
          ]
    }
  where
    tags =
      availableNamedTags runningTools

    renderTag tag@NamedTag{tagName, tagDescription} =
      [i|- #{tagName}: #{tagDescription} Tools: #{Text.intercalate ", " (toolsFor tag)}|]

    toolsFor tag =
      [ runningTool.name
      | runningTool <- runningTools
      , Named tag `elem` runningTool.tags
      ]

duplicateNames :: [LLM.FunctionTool] -> [Text]
duplicateNames schemas =
  mapMaybe listToMaybe
    . filter ((> 1) . length)
    . group
    . sort
    $ map (.name) schemas

-- | Resolve a model tool call, decode its JSON arguments, and invoke the
-- per-run runner.
runToolCall
  :: Concurrent :> es
  => Context
  -> ToolCallMetadata
  -> [Tool (Eff es)]
  -> [RunningTool (Eff es)]
  -> LLM.ToolCall
  -> Eff es ToolResult
runToolCall context metadata tools runningTools call =
  findRunningTool call.name runningTools >>= \case
    Nothing ->
      case find ((== call.name) . toolName) tools of
        Just definition | not (toolAllowed definition context) ->
          pure (toolFailure (permissionDeniedFailure [i|Permission denied for tool: #{callName}|] [i|Tool #{callName} is not allowed in this agent context.|]))
        _ ->
          pure (toolFailure (permanentArgumentFailure [i|Unknown tool: #{callName}|] [i|The model requested an unknown tool: #{callName}|]))
    Just runningTool ->
      case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 call.arguments) of
        Left err ->
          pure (toolFailure (permanentArgumentFailure [i|Invalid JSON arguments for #{callName}: #{err}|] [i|Invalid JSON arguments for #{callName}: #{err}|]))
        Right args ->
          case toolEnableArgumentError runningTools callName args of
            Just err ->
              pure (toolFailure (permanentArgumentFailure err err))
            Nothing ->
              runningTool.run metadata args
  where
    callName = call.name

toolEnableArgumentError :: [RunningTool m] -> Text -> Aeson.Value -> Maybe Text
toolEnableArgumentError runningTools callName arguments
  | callName /= toolEnableName =
      Nothing
  | otherwise =
      case AesonTypes.parseEither parser arguments of
        Left err ->
          Just (toText err)
        Right [] ->
          Just "tags must not be empty."
        Right requested ->
          case filter (`Set.notMember` availableTagNames runningTools) requested of
            [] ->
              Nothing
            unknown ->
              Just [i|Unknown tool tags: #{Text.intercalate ", " (ordNub unknown)}|]
  where
    parser =
      Aeson.withObject "tool_enable arguments" (Aeson..: Key.fromText "tags")

findRunningTool :: Concurrent :> es => Text -> [RunningTool (Eff es)] -> Eff es (Maybe (RunningTool (Eff es)))
findRunningTool _ [] =
  pure Nothing
findRunningTool name (runningTool : rest) = do
  schema <- MVar.readMVar runningTool.currentSchema
  if runningTool.name == name && isJust schema
    then pure (Just runningTool)
    else findRunningTool name rest
