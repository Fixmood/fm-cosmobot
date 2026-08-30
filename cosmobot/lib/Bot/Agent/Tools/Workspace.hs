{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Bot.Agent.Tools.Workspace
Description : Agent tool for superuser workspaces
Stability   : experimental
-}
module Bot.Agent.Tools.Workspace
  ( workspaceTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Agent.Types
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Bot.Resource.Workspace as Workspace
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.Process.Typed as TypedProcess

workspaceTool
  :: (Resource.Resource :> es, FileSystem.FileSystem :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Tool (Eff es)
workspaceTool =
  tagged [workTag]
  . allowWhen (\context -> superuserOnly context && isRight (Resource.accessFromMessage context.message))
  . withDescription "Manage dedicated /work workspaces for multi-step work such as repositories, scripts, CI, research, or operations. list returns a JSON array of accessible workspace names. Create one before substantial work, read repository instructions before editing, and keep WORK.md current with the goal, paths, branches, commits, validation, environment notes, and blockers. Verify the authenticated account and required permissions before remote operations. Prefer a topic branch and a pull request with a summary and validation. Actions: create, list, query, update, rename, delete."
  $ tool "workspace"
      (parsedArguments
        (objectSchema
          [ fieldText "action" "One of: create, list, query, update, rename, delete."
          , fieldText "id" "Short stable descriptive id for create; letters, digits, dot, underscore, and hyphen only."
          , fieldText "name" "Optional globally unique resource name for create; required as the new name for rename."
          , fieldText "goal" "Initial work goal for create, or complete replacement WORK.md contents for update."
          , fieldText "resource" "Resource name returned by create; required for query, update, rename, and delete."
          , fieldInteger "ttl_minutes" "Resource inactivity lifetime in minutes; required for create, minimum 5."
          ]
          ["action"])
        parseWorkspaceCall)
      \call -> do
        context <- askToolContext
        metadata <- askToolCallMetadata
        runWorkspaceCall context metadata call

data WorkspaceCall
  = CreateWorkspace !(Maybe Text) !Workspace.WorkspaceArgs
  | ListWorkspaces
  | QueryWorkspace !Text
  | UpdateWorkspace !Text !Text
  | DestroyWorkspace !Text
  | RenameWorkspace !Text !Text

runWorkspaceCall
  :: forall es. (Resource.Resource :> es, FileSystem.FileSystem :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Context
  -> ToolCallMetadata
  -> WorkspaceCall
  -> Eff es ToolResult
runWorkspaceCall context metadata call =
  case Resource.accessFromMessage context.message of
    Left err -> pure (resourceToolFailure err)
    Right access -> case call of
      CreateWorkspace requestedName arguments ->
        createWorkspace requestedName Resource.Init{message = context.message, arguments} <&> \case
          Left err -> resourceToolFailure err
          Right resourceId -> toolText (jsonText (Aeson.object
            [ "resource" Aeson..= resourceId
            , "path" Aeson..= ("/work/" <> arguments.workId)
            ]))
      ListWorkspaces ->
        listResourceNames (Proxy @Workspace.Workspace) access
      QueryWorkspace resourceId ->
        use access resourceId Workspace.queryWorkspace <&> result
      UpdateWorkspace resourceId goal ->
        use access resourceId (\workspace -> Workspace.updateWorkspace workspace goal) <&> resultWith "Workspace updated."
      DestroyWorkspace resourceId -> use access resourceId (const (pure (Right ()))) >>= \case
        Left err -> pure (clientFailure err)
        Right () -> Resource.destroy access resourceId <&> first renderResourceError <&> resultWith "Workspace destroyed."
      RenameWorkspace resourceId newName ->
        Resource.rename access resourceId newName <&> first renderResourceError <&> resultWith "Workspace renamed."
  where
    createWorkspace = \case
      Nothing -> Resource.createForRun @Workspace.Workspace metadata.originRunId metadata.resourceOwner
      Just name -> Resource.createNamedForRun @Workspace.Workspace metadata.originRunId metadata.resourceOwner name

    use
      :: forall a. Resource.ResourceAccess
      -> Text
      -> (Workspace.Workspace -> Eff es (Either Text a))
      -> Eff es (Either Text a)
    use access resourceId action =
      Resource.withResource @Workspace.Workspace access resourceId metadata.resourceOwner action
        <&> first renderResourceError
        <&> join

    result = either clientFailure toolText
    resultWith message = either clientFailure (const (toolText message))

parseWorkspaceCall :: Aeson.Value -> AesonTypes.Parser WorkspaceCall
parseWorkspaceCall = Aeson.withObject "workspace arguments" \o -> do
  action <- o Aeson..: Key.fromText "action"
  case action :: Text of
    "create" -> CreateWorkspace
      <$> o Aeson..:? Key.fromText "name"
      <*> (Workspace.WorkspaceArgs
        <$> (o Aeson..: Key.fromText "id" >>= validWorkId)
        <*> (o Aeson..: Key.fromText "goal" >>= validNonEmpty "goal")
        <*> parseTTLMinutes o)
    "list" -> pure ListWorkspaces
    "query" -> QueryWorkspace <$> requiredResourceId o
    "update" -> UpdateWorkspace <$> requiredResourceId o <*> (o Aeson..: Key.fromText "goal" >>= validNonEmpty "goal")
    "delete" -> DestroyWorkspace <$> requiredResourceId o
    "destroy" -> DestroyWorkspace <$> requiredResourceId o
    "rename" -> RenameWorkspace <$> requiredResourceId o <*> (o Aeson..: Key.fromText "name" >>= validNonEmpty "name")
    _ -> fail "action must be one of: create, list, query, update, rename, delete."

requiredResourceId :: AesonTypes.Object -> AesonTypes.Parser Text
requiredResourceId o = o Aeson..: Key.fromText "resource" >>= validNonEmpty "resource"

validWorkId :: Text -> AesonTypes.Parser Text
validWorkId = either (fail . Text.unpack) pure . Workspace.validateWorkId

validNonEmpty :: String -> Text -> AesonTypes.Parser Text
validNonEmpty label value
  | Text.null (Text.strip value) = fail (label <> " must not be empty.")
  | otherwise = pure value

clientFailure :: Text -> ToolResult
clientFailure err = toolFailure (permanentArgumentFailure err err)
