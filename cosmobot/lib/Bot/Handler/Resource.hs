{-|
Module      : Bot.Handler.Resource
Description : Commands for chat-owned long-running resources
Stability   : experimental
-}
module Bot.Handler.Resource
  ( resourceHandlers
  , resourceIds
  , removeResources
  , renderResources
  )
where

import Bot.Core.Message
import Bot.Core.Route
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

resourceHandlers :: (Chat.Chat :> es, Resource.Resource :> es) => [RouteHandler es]
resourceHandlers =
  [ documented "!res/ls" "List resources accessible in this chat and their remaining life." $
      stopOn (command "!res/ls") handleList
  , documented "!res/detail <id>" "Show resource details, including its lifetime." $
      stopOn (command "!res/detail") handleDetail
  , documented "!res/rm <id>..." "Remove one or more resources." $
      stopOn (command "!res/rm") handleRemove
  , documented "!res/mv <id> <new-name>" "Rename a resource." $
      stopOn (command "!res/mv") handleRename
  , documented "!res/keepalive <id>..." "Refresh one or more resource lifetimes." $
      stopOn (command "!res/keepalive") (handleLifetime "keepalive" Resource.keepAlive)
  , documented "!res/permanent <id>..." "Make one or more resources permanent." $
      stopOn (command "!res/permanent") (handleLifetime "permanent" Resource.makePermanent)
  ]
  where
    documented label description = withHelp RouteHelp{label, description}

resourceIds :: Text -> [Resource.ResourceId]
resourceIds = List.nub . Text.words

handleList :: (Chat.Chat :> es, Resource.Resource :> es) => IncomingMessage -> Text -> Eff es ()
handleList message _ =
  withAccess message \access -> Resource.list access >>= \case
    [] -> reply message "No resources."
    resources -> reply message (renderResources resources)

handleDetail :: (Chat.Chat :> es, Resource.Resource :> es) => IncomingMessage -> Text -> Eff es ()
handleDetail message input =
  case Text.words input of
    [resourceId] ->
      withAccess message \access ->
        Resource.detail access resourceId >>= \case
          Right description -> reply message description
          Left _ -> reply message "Resource not found, not owned, or unavailable."
    _ -> reply message "Usage: !res/detail <resource_name>"

handleRemove :: (Chat.Chat :> es, Resource.Resource :> es) => IncomingMessage -> Text -> Eff es ()
handleRemove message input =
  case resourceIds input of
    [] -> reply message "Usage: !res/rm <resource_id>..."
    resourceIds_ ->
      withAccess message \access -> do
        results <- removeResources access resourceIds_
        reply message (Text.unlines results)

handleRename :: (Chat.Chat :> es, Resource.Resource :> es) => IncomingMessage -> Text -> Eff es ()
handleRename message input =
  case Text.words input of
    [resourceId, newName] ->
      withAccess message \access ->
        Resource.rename access resourceId newName >>= \case
          Right name -> reply message ("Resource renamed to `" <> name <> "`.")
          Left Resource.InvalidResourceName -> reply message "Invalid resource name."
          Left Resource.ResourceNameAlreadyExists -> reply message "Resource name already exists."
          Left _ -> reply message "Resource not found, not owned, or unavailable."
    _ -> reply message "Usage: !res/mv <resource_name> <new_name>"

handleLifetime
  :: (Chat.Chat :> es, Resource.Resource :> es)
  => Text
  -> (Resource.ResourceAccess -> Resource.ResourceId -> Eff es (Either Resource.ResourceError ()))
  -> IncomingMessage
  -> Text
  -> Eff es ()
handleLifetime operation action message input =
  case resourceIds input of
    [] -> reply message ("Usage: !res/" <> operation <> " <resource_id>...")
    resourceIds_ -> withAccess message \access -> do
      results <- forM resourceIds_ \resourceId ->
        action access resourceId <&> \case
          Right () -> "- `" <> resourceId <> "`: " <> operation <> " set"
          Left Resource.ResourceLifetimeUpdateFailed{} -> "- `" <> resourceId <> "`: persistence failure"
          Left _ -> "- `" <> resourceId <> "`: not found/not owned/unavailable"
      reply message (Text.unlines results)

removeResources :: Resource.Resource :> es => Resource.ResourceAccess -> [Resource.ResourceId] -> Eff es [Text]
removeResources access = traverse \resourceId ->
  Resource.destroy access resourceId <&> \case
    Right () -> "- `" <> resourceId <> "`: removed"
    Left Resource.ResourceCleanupFailed{} -> "- `" <> resourceId <> "`: cleanup failure"
    Left _ -> "- `" <> resourceId <> "`: not found/not owned"

withAccess
  :: Chat.Chat :> es
  => IncomingMessage
  -> (Resource.ResourceAccess -> Eff es ())
  -> Eff es ()
withAccess message action =
  either (const (reply message identityRequired)) action (Resource.accessFromMessage message)

reply :: Chat.Chat :> es => IncomingMessage -> Text -> Eff es ()
reply message = void . Chat.replyTo message

identityRequired :: Text
identityRequired = "Resource operations require chat and sender identity."

renderResources :: [Resource.SomeResourceObject] -> Text
renderResources resources =
  Text.intercalate "\n" $ map renderGroup $ Map.toAscList $ Map.fromListWith (<>)
    [(resource.resourceType, [resource]) | resource <- resources]
  where
    renderGroup (resourceType, entries) =
      Text.unlines $
        ("Resource type " <> resourceType <> ":")
          : map renderEntry (List.sortOn (.resourceId) entries)
    renderEntry resource =
      "- `" <> resource.resourceId <> "`: " <> resource.description
        <> " (life: " <> maybe "permanent" ((<> "m") . show) resource.remainingLifeMinutes <> ")"
