{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Bot.Resource
Description : Lifecycle and durable registrations for chat-owned long-running objects
Stability   : experimental
-}
module Bot.Resource
  ( ResourceId
  , ResourceOwner (..)
  , ResourceAccess (..)
  , Init (..)
  , ResourceObject (..)
  , SomeResourceObject (..)
  , ResourceError (..)
  , ownerFromMessage
  , accessFromMessage
  , runResourceManager
  , runResourceManagerWith
  , resourceLoader
  )
where

import Bot.Core.Message
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Resource as Resource
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude hiding (state)
import Bot.Resource.Types
import qualified Bot.Storage.Resource as ResourceStorage
import qualified Data.Dynamic as Dynamic
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import qualified Effectful.Concurrent.MVar as MVar

data Availability
  = Available !(Map Concurrency.Handle Int) !Int
  | Destroying

data SomeResource es = SomeResource
  { owner :: !ResourceOwner
  , scope :: !ResourceScope
  , createdBy :: !(Maybe Concurrency.Handle)
  , creatorRunId :: !(Maybe Text)
  , sessionId :: !(Maybe Text)
  , resourceType :: !Text
  , value :: !Dynamic.Dynamic
  , describe :: Either Text Text -> Eff es Text
  , probe :: Eff es (Either Text Text)
  , detail :: Eff es Text
  , destroy :: Eff es (Either Text ())
  , persistent :: !Bool
  , listed :: !Bool
  , ttlSeconds :: !(Maybe Int)
  , expiresAt :: !(Maybe UTCTime)
  , availability :: !Availability
  }

data ManagerState es = ManagerState
  { nextId :: !Integer
  , nextCreate :: !Integer
  , accepting :: !Bool
  , creating :: !(Map Integer (ResourceId, MVar.MVar ()))
  , resources :: !(Map ResourceId (SomeResource es))
  }

runResourceManager
  :: (Storage.Storage :> es, Concurrency.Concurrency :> es, Concurrent :> es, IOE :> es, Prim :> es)
  => Eff (Resource.Resource : es) a
  -> Eff es a
runResourceManager = runResourceManagerWith []

runResourceManagerWith
  :: (Storage.Storage :> es, Concurrency.Concurrency :> es, Concurrent :> es, IOE :> es, Prim :> es)
  => [ResourceLoader (Eff es)]
  -> Eff (Resource.Resource : es) a
  -> Eff es a
runResourceManagerWith loaders inner = do
  stored <- ResourceStorage.loadResources
  lifetimes <- Map.fromList <$> ResourceStorage.loadResourceLifetimes
  loaded <- traverse (\resource -> restoreStoredResource loaders (Map.lookup resource.resourceId lifetimes) resource) stored
  let resources = Map.fromList [(resource.resourceId, value) | (resource, value) <- zip stored loaded]
      nextId = List.maximum (1 : map ((+ 1) . resourceIdNumber . (.resourceId)) stored)
  stateRef <- newIORef ManagerState{nextId, nextCreate = 1, accepting = True, creating = Map.empty, resources}
  let runInner = Concurrency.withWorker "resource expiry" (reclaimExpired stateRef) $
        interpret (runResourceOperation stateRef) inner
  runInner `finally` shutdown stateRef

restoreStoredResource
  :: forall es. (Concurrent :> es, IOE :> es)
  => [ResourceLoader (Eff es)]
  -> Maybe (Int, UTCTime)
  -> ResourceStorage.StoredResource
  -> Eff es (SomeResource es)
restoreStoredResource loaders lifetime stored =
  case List.find matches loaders of
    Nothing -> pure (unavailableResource "No loader is registered for this resource type.")
    Just loader -> restoreWith loader
  where
    matches (ResourceLoader (proxy :: Proxy a)) =
      resourceTypeName @(Eff es) @a proxy == stored.resourceType

    restoreWith (ResourceLoader (proxy :: Proxy a)) =
      case resourcePersistence @(Eff es) @a proxy of
        EphemeralResource ->
          pure (unavailableResource "The registered resource type is not persistent.")
        PersistentResource{restoreResource} ->
          trySync (restoreResource stored.payload) <&> \case
            Right (Right object) -> restoredResource proxy object
            Right (Left err) -> unavailableResource err
            Left err -> unavailableResource (conciseException err)

    restoredResource
      :: forall a. ResourceObject (Eff es) a
      => Proxy a
      -> a
      -> SomeResource es
    restoredResource proxy object = SomeResource
        { owner = stored.owner
        , scope = resourceScope @(Eff es) @a proxy
        , createdBy = Nothing
        , creatorRunId = stored.creatorRunId
        , sessionId = stored.sessionId
        , resourceType = resourceTypeName @(Eff es) @a proxy
        , value = Dynamic.toDyn object
        , describe = describeResourceObject object
        , probe = probeResourceObject object
        , detail = detailResourceObject object
        , destroy = destroyResourceObject object
        , persistent = True
        , listed = resourceListed @(Eff es) @a proxy
        , ttlSeconds = fst <$> lifetime
        , expiresAt = snd <$> lifetime
        , availability = Available Map.empty 0
        }

    unavailableResource err = SomeResource
      { owner = stored.owner
      , scope = PersonResource
      , createdBy = Nothing
      , creatorRunId = stored.creatorRunId
      , sessionId = stored.sessionId
      , resourceType = stored.resourceType
      , value = Dynamic.toDyn ()
      , describe = const (pure "unavailable")
      , probe = pure (Left err)
      , detail = pure ("unavailable: " <> err)
      , destroy = pure (Right ())
      , persistent = True
      , listed = True
      , ttlSeconds = fst <$> lifetime
      , expiresAt = snd <$> lifetime
      , availability = Available Map.empty 0
      }

resourceIdNumber :: ResourceId -> Integer
resourceIdNumber resourceId =
  fromMaybe 0 (readMaybe (Text.unpack (snd (Text.breakOnEnd "-" resourceId))))

conciseException :: Show e => e -> Text
conciseException = Text.take 500 . show

runResourceOperation
  :: (Storage.Storage :> es, Concurrency.Concurrency :> es, Concurrent :> es, IOE :> es, Prim :> es)
  => IORef (ManagerState es)
  -> EffectHandler Resource.Resource es
runResourceOperation stateRef localEnv operation =
  localUnlift localEnv (ConcUnlift Persistent Unlimited) \unlift ->
    case operation of
      Resource.Create proxy parent creatorRunId requestedName initValue -> createIn stateRef unlift proxy parent creatorRunId requestedName initValue
      Resource.With access resourceId user callback -> withIn stateRef unlift access resourceId user callback
      Resource.List access -> listIn stateRef access
      Resource.ListCreatedByRuns access runIds -> listCreatedByRunsIn stateRef access runIds
      Resource.Detail access resourceId -> detailIn stateRef access resourceId
      Resource.Destroy access resourceId -> destroyIn stateRef access resourceId
      Resource.Rename access resourceId newId -> renameIn stateRef access resourceId newId
      Resource.KeepAlive access resourceId -> updateLifetimeIn stateRef access resourceId False
      Resource.MakePermanent access resourceId -> updateLifetimeIn stateRef access resourceId True
      Resource.DestroyAssociated parent -> destroyAssociatedIn stateRef parent

createIn
  :: forall localEs es a. (ResourceObject (Eff localEs) a, Storage.Storage :> es, Concurrent :> es, Prim :> es, IOE :> es)
  => IORef (ManagerState es)
  -> (forall x. Eff localEs x -> Eff es x)
  -> Proxy a
  -> Maybe Concurrency.Handle
  -> Maybe Text
  -> Maybe ResourceId
  -> Init (CreationArgs a)
  -> Eff es (Either ResourceError ResourceId)
createIn stateRef unlift _ parent creatorRunId requestedName initValue =
  case ttlResult of
    Left err -> pure (Left (ResourceCreationFailed err))
    Right ttlSeconds -> createWith ttlSeconds
  where
    ttlResult = resourceTTLSeconds @(Eff localEs) @a initValue.arguments

    createWith ttlSeconds = case ownerFromMessage initValue.message of
      Left err -> pure (Left err)
      Right owner -> do
        gate <- MVar.newEmptyMVar
        reservation <- atomicModifyIORef' stateRef \state ->
          if state.accepting
            then reserveName state gate requestedName (resourceIdPrefix @(Eff localEs) @a (Proxy @a))
            else (state, Left ResourceUnavailable)
        case reservation of
          Left err -> pure (Left err)
          Right (createId, resourceId) ->
            (mask \restore -> createAndRegister ttlSeconds restore owner resourceId) `finally` finishCreate createId gate

    createAndRegister ttlSeconds restore owner resourceId =
      restore (unlift (createResourceObject @(Eff localEs) @a initValue)) >>= \case
        Left err -> pure (Left (ResourceCreationFailed err))
        Right object -> do
          expiresAt <- expiryFromNow ttlSeconds
          let persistence = resourcePersistence @(Eff localEs) @a (Proxy @a)
              isPersistent = case persistence of
                EphemeralResource -> False
                PersistentResource{} -> True
              resource = SomeResource
                { owner
                , scope = resourceScope @(Eff localEs) @a (Proxy @a)
                , createdBy = parent
                , creatorRunId
                , sessionId = sessionIdFromMessage initValue.message
                , resourceType = resourceTypeName @(Eff localEs) @a (Proxy @a)
                , value = Dynamic.toDyn object
                , describe = unlift . describeResourceObject object
                , probe = unlift (probeResourceObject object)
                , detail = unlift (detailResourceObject object)
                , destroy = unlift (destroyResourceObject object)
                , persistent = isPersistent
                , listed = resourceListed @(Eff localEs) @a (Proxy @a)
                , ttlSeconds
                , expiresAt
                , availability = Available Map.empty 0
                }
              discard = do
                when isPersistent $ void (trySync (ResourceStorage.deleteResource resourceId))
                void (trySync resource.destroy)
          persisted <- case persistence of
            EphemeralResource -> pure (Right ())
            PersistentResource{encodeResource} ->
              trySync
                ((do
                    ResourceStorage.saveResource ResourceStorage.StoredResource
                      { resourceId
                      , resourceType = resource.resourceType
                      , owner
                      , sessionId = resource.sessionId
                      , creatorRunId
                      , payload = encodeResource object
                      }
                    ResourceStorage.setResourceLifetime resourceId ((,) <$> ttlSeconds <*> expiresAt)
                 ) `onException` discard)
          case persisted of
            Left err -> discard $> Left (ResourceCreationFailed ("Failed to persist resource: " <> conciseException err))
            Right () -> do
              atomicModifyIORef' stateRef \state ->
                (state{resources = Map.insert resourceId resource state.resources}, ())
              pure (Right resourceId)

    finishCreate createId gate = do
      atomicModifyIORef' stateRef \state -> (state{creating = Map.delete createId state.creating}, ())
      void (MVar.tryPutMVar gate ())

reserveName
  :: ManagerState es
  -> MVar.MVar ()
  -> Maybe ResourceId
  -> Text
  -> (ManagerState es, Either ResourceError (Integer, ResourceId))
reserveName state gate requestedName prefix =
  case traverse validateResourceName requestedName of
    Left err -> (state, Left err)
    Right validName ->
      let occupied name = Map.member name state.resources || any ((== name) . fst) state.creating
          nextAvailable candidate =
            let name = prefix <> "-" <> show candidate
            in if occupied name then nextAvailable (candidate + 1) else (candidate, name)
          (nextId, resourceId) = case validName of
            Just name -> (state.nextId, name)
            Nothing -> first (+ 1) (nextAvailable state.nextId)
      in if occupied resourceId
          then (state, Left ResourceNameAlreadyExists)
          else
            let createId = state.nextCreate
            in ( state
                  { nextId
                  , nextCreate = createId + 1
                  , creating = Map.insert createId (resourceId, gate) state.creating
                  }
               , Right (createId, resourceId)
               )

validateResourceName :: ResourceId -> Either ResourceError ResourceId
validateResourceName name
  | Text.null name || Text.length name > 64 = Left InvalidResourceName
  | name == "." || name == ".." = Left InvalidResourceName
  | Text.all validCharacter name = Right name
  | otherwise = Left InvalidResourceName
  where
    validCharacter character =
      Char.isAlphaNum character || character `elem` ("._-" :: String)

withIn
  :: forall localEs es a b. (ResourceObject (Eff localEs) a, Storage.Storage :> es, Concurrency.Concurrency :> es, Prim :> es, IOE :> es)
  => IORef (ManagerState es)
  -> (forall x. Eff localEs x -> Eff es x)
  -> ResourceAccess
  -> ResourceId
  -> Maybe Concurrency.Handle
  -> (a -> Eff localEs b)
  -> Eff es (Either ResourceError b)
withIn stateRef unlift access resourceId user callback =
  acquire stateRef access resourceId user >>= \case
    Left err -> pure (Left err)
    Right (dynamicValue, persistent, previousLifetime, currentLifetime) ->
      case Dynamic.fromDynamic dynamicValue of
        Nothing -> do
          rollbackLifetime stateRef resourceId currentLifetime previousLifetime
          releaseUser stateRef resourceId user
          pure (Left ResourceTypeMismatch)
        Just object ->
          persistRefreshedLifetime persistent resourceId currentLifetime >>= \case
            Left err -> do
              rollbackLifetime stateRef resourceId currentLifetime previousLifetime
              releaseUser stateRef resourceId user
              pure (Left err)
            Right () -> Right <$> unlift (callback object) `finally` releaseUser stateRef resourceId user

listIn
  :: (Prim :> es, IOE :> es)
  => IORef (ManagerState es)
  -> ResourceAccess
  -> Eff es [SomeResourceObject]
listIn stateRef access =
  listMatchingIn stateRef access (const True)

listCreatedByRunsIn
  :: (Prim :> es, IOE :> es)
  => IORef (ManagerState es)
  -> ResourceAccess
  -> [Text]
  -> Eff es [SomeResourceObject]
listCreatedByRunsIn stateRef access runIds =
  listMatchingIn stateRef access \resource ->
    maybe False (`elem` runIds) resource.creatorRunId

listMatchingIn
  :: (Prim :> es, IOE :> es)
  => IORef (ManagerState es)
  -> ResourceAccess
  -> (SomeResource es -> Bool)
  -> Eff es [SomeResourceObject]
listMatchingIn stateRef access matches = do
  now <- liftIO getCurrentTime
  entries <- Map.toAscList . Map.filter (\resource -> resource.listed && matches resource && mayAccess access resource && isAvailable resource.availability) . (.resources) <$> readIORef stateRef
  traverse (snapshot now) entries
  where
    snapshot now (resourceId, resource) = do
      probeResult <- resource.probe
      description <- resource.describe probeResult
      pure $ SomeResourceObject resourceId resource.resourceType resource.sessionId description probeResult (remainingMinutes now resource.expiresAt)

    isAvailable = \case
      Available{} -> True
      Destroying -> False

detailIn
  :: (Prim :> es, IOE :> es)
  => IORef (ManagerState es)
  -> ResourceAccess
  -> ResourceId
  -> Eff es (Either ResourceError Text)
detailIn stateRef access resourceId = do
  now <- liftIO getCurrentTime
  resources <- (.resources) <$> readIORef stateRef
  case Map.lookup resourceId resources of
    Just resource
      | mayAccess access resource, Available{} <- resource.availability ->
          resource.detail <&> Right . (<> ("\nlife: " <> renderLife (remainingMinutes now resource.expiresAt)))
      | mayAccess access resource -> pure (Left ResourceUnavailable)
    _ -> pure (Left ResourceNotFoundOrNotOwned)

sessionIdFromMessage :: IncomingMessage -> Maybe Text
sessionIdFromMessage message
  | message.platform `elem` [PlatformACP, PlatformRPC] = listToMaybe message.chatAliases
  | otherwise = Nothing

destroyIn
  :: (Storage.Storage :> es, Concurrency.Concurrency :> es, Prim :> es, IOE :> es)
  => IORef (ManagerState es)
  -> ResourceAccess
  -> ResourceId
  -> Eff es (Either ResourceError ())
destroyIn stateRef access resourceId =
  beginDestroy stateRef access resourceId >>= \case
    Left err -> pure (Left err)
    Right (users, cleanup, persistent) -> do
      traverse_ cancelAndAwait users
      finishDestroy stateRef resourceId cleanup persistent

renameIn
  :: (Storage.Storage :> es, Prim :> es, IOE :> es)
  => IORef (ManagerState es)
  -> ResourceAccess
  -> ResourceId
  -> ResourceId
  -> Eff es (Either ResourceError ResourceId)
renameIn stateRef access resourceId newId =
  case validateResourceName newId of
    Left err -> pure (Left err)
    Right validId -> mask \_ -> beginRename stateRef access resourceId validId >>= \case
      Left err -> pure (Left err)
      Right Nothing -> pure (Right validId)
      Right (Just persistent) -> do
        let rollback = do
              when persistent $ void $ trySync (ResourceStorage.renameResource validId resourceId)
              rollbackRename stateRef resourceId validId
            persist
              | persistent = ResourceStorage.renameResource resourceId validId
              | otherwise = pure ()
        trySync (persist `onException` rollback) >>= \case
          Left err -> rollbackRename stateRef resourceId validId $> Left (ResourceRenameFailed (conciseException err))
          Right () -> finishRename stateRef validId $> Right validId

beginRename
  :: Prim :> es
  => IORef (ManagerState es)
  -> ResourceAccess
  -> ResourceId
  -> ResourceId
  -> Eff es (Either ResourceError (Maybe Bool))
beginRename stateRef access resourceId newId =
  atomicModifyIORef' stateRef \state ->
    case Map.lookup resourceId state.resources of
      Nothing -> (state, Left ResourceNotFoundOrNotOwned)
      Just resource
        | not (mayAccess access resource) -> (state, Left ResourceNotFoundOrNotOwned)
        | resourceId == newId, Available{} <- resource.availability -> (state, Right Nothing)
        | not (isAvailableWithoutUsers resource.availability) -> (state, Left ResourceUnavailable)
        | Map.member newId state.resources || any ((== newId) . fst) state.creating ->
            (state, Left ResourceNameAlreadyExists)
        | otherwise ->
            let renamed = resource{availability = Destroying}
                resources = Map.insert newId renamed (Map.delete resourceId state.resources)
            in (state{resources}, Right (Just resource.persistent))
  where
    isAvailableWithoutUsers = \case
      Available _ activeUses -> activeUses == 0
      Destroying -> False

finishRename :: Prim :> es => IORef (ManagerState es) -> ResourceId -> Eff es ()
finishRename stateRef resourceId =
  atomicModifyIORef' stateRef \state ->
    let finish resource = resource{availability = Available Map.empty 0}
    in (state{resources = Map.adjust finish resourceId state.resources}, ())

rollbackRename :: Prim :> es => IORef (ManagerState es) -> ResourceId -> ResourceId -> Eff es ()
rollbackRename stateRef resourceId newId =
  atomicModifyIORef' stateRef \state ->
    case Map.lookup newId state.resources of
      Nothing -> (state, ())
      Just resource ->
        let restored = resource{availability = Available Map.empty 0}
            resources = Map.insert resourceId restored (Map.delete newId state.resources)
        in (state{resources}, ())

destroyAssociatedIn
  :: (Storage.Storage :> es, Concurrency.Concurrency :> es, Prim :> es, IOE :> es)
  => IORef (ManagerState es)
  -> Concurrency.Handle
  -> Eff es [Either ResourceError ()]
destroyAssociatedIn stateRef parent = do
  resources <- Map.toList . Map.filter ((== Just parent) . (.createdBy)) . (.resources) <$> readIORef stateRef
  traverse (\(resourceId, resource) -> destroyIn stateRef (ResourceAccess resource.owner False) resourceId) resources

acquire
  :: (Prim :> es, IOE :> es)
  => IORef (ManagerState es)
  -> ResourceAccess
  -> ResourceId
  -> Maybe Concurrency.Handle
  -> Eff es (Either ResourceError (Dynamic.Dynamic, Bool, (Maybe Int, Maybe UTCTime), (Maybe Int, Maybe UTCTime)))
acquire stateRef access resourceId user = do
  now <- liftIO getCurrentTime
  atomicModifyIORef' stateRef \state ->
    case Map.lookup resourceId state.resources of
      Nothing -> (state, Left ResourceNotFoundOrNotOwned)
      Just resource
        | not (mayAccess access resource) -> (state, Left ResourceNotFoundOrNotOwned)
        | Available users activeUses <- resource.availability ->
            let expiresAt = (`addUTCTime` now) . fromIntegral <$> resource.ttlSeconds
                updated = resource
                  { availability = Available (maybe users (\userHandle -> Map.insertWith (+) userHandle 1 users) user) (activeUses + 1)
                  , expiresAt
                  }
                previousLifetime = (resource.ttlSeconds, resource.expiresAt)
                currentLifetime = (resource.ttlSeconds, expiresAt)
            in (state{resources = Map.insert resourceId updated state.resources}, Right (resource.value, resource.persistent, previousLifetime, currentLifetime))
        | otherwise -> (state, Left ResourceUnavailable)

persistLifetime
  :: (Storage.Storage :> es, IOE :> es)
  => Bool
  -> ResourceId
  -> Maybe (Int, UTCTime)
  -> Eff es (Either ResourceError ())
persistLifetime False _ _ = pure (Right ())
persistLifetime True resourceId lifetime =
  trySync (ResourceStorage.setResourceLifetime resourceId lifetime)
    <&> first (ResourceLifetimeUpdateFailed . conciseException)

persistRefreshedLifetime
  :: (Storage.Storage :> es, IOE :> es)
  => Bool
  -> ResourceId
  -> (Maybe Int, Maybe UTCTime)
  -> Eff es (Either ResourceError ())
persistRefreshedLifetime persistent resourceId (ttlSeconds, expiresAt) =
  case (,) <$> ttlSeconds <*> expiresAt of
    Nothing -> pure (Right ())
    lifetime -> persistLifetime persistent resourceId lifetime

updateLifetimeIn
  :: (Storage.Storage :> es, Prim :> es, IOE :> es)
  => IORef (ManagerState es)
  -> ResourceAccess
  -> ResourceId
  -> Bool
  -> Eff es (Either ResourceError ())
updateLifetimeIn stateRef access resourceId permanent = do
  now <- liftIO getCurrentTime
  updated <- atomicModifyIORef' stateRef \state ->
    case Map.lookup resourceId state.resources of
      Nothing -> (state, Left ResourceNotFoundOrNotOwned)
      Just resource
        | not (mayOwn access resource) -> (state, Left ResourceNotFoundOrNotOwned)
        | Available{} <- resource.availability ->
            let ttlSeconds = if permanent then Nothing else resource.ttlSeconds
                expiresAt = (`addUTCTime` now) . fromIntegral <$> ttlSeconds
                resource' = resource{ttlSeconds, expiresAt}
            in ( state{resources = Map.insert resourceId resource' state.resources}
               , Right (resource.persistent, (resource.ttlSeconds, resource.expiresAt), (ttlSeconds, expiresAt))
               )
        | otherwise -> (state, Left ResourceUnavailable)
  case updated of
    Left err -> pure (Left err)
    Right (persistent, previousLifetime, currentLifetime) ->
      persistLifetime persistent resourceId ((,) <$> fst currentLifetime <*> snd currentLifetime) >>= \case
        Left err -> rollbackLifetime stateRef resourceId currentLifetime previousLifetime $> Left err
        Right () -> pure (Right ())

rollbackLifetime
  :: Prim :> es
  => IORef (ManagerState es)
  -> ResourceId
  -> (Maybe Int, Maybe UTCTime)
  -> (Maybe Int, Maybe UTCTime)
  -> Eff es ()
rollbackLifetime stateRef resourceId expected previous =
  atomicModifyIORef' stateRef \state ->
    let rollback resource
          | (resource.ttlSeconds, resource.expiresAt) == expected =
              resource{ttlSeconds = fst previous, expiresAt = snd previous}
          | otherwise = resource
    in (state{resources = Map.adjust rollback resourceId state.resources}, ())

mayOwn :: ResourceAccess -> SomeResource es -> Bool
mayOwn access resource = access.superuser || access.owner == resource.owner

remainingMinutes :: UTCTime -> Maybe UTCTime -> Maybe Int
remainingMinutes now = fmap (max 0 . ceiling . (/ 60) . (`diffUTCTime` now))

renderLife :: Maybe Int -> Text
renderLife = maybe "permanent" (<> "m") . fmap show

beginDestroy
  :: Prim :> es
  => IORef (ManagerState es)
  -> ResourceAccess
  -> ResourceId
  -> Eff es (Either ResourceError ([Concurrency.Handle], Eff es (Either Text ()), Bool))
beginDestroy stateRef access resourceId =
  atomicModifyIORef' stateRef \state ->
    case Map.lookup resourceId state.resources of
      Nothing -> (state, Left ResourceNotFoundOrNotOwned)
      Just resource
        | not (mayAccess access resource) -> (state, Left ResourceNotFoundOrNotOwned)
        | Available users activeUses <- resource.availability
        , activeUses == sum (Map.elems users) ->
            let updated = resource{availability = Destroying}
            in (state{resources = Map.insert resourceId updated state.resources}, Right (Map.keys users, resource.destroy, resource.persistent))
        | otherwise -> (state, Left ResourceUnavailable)

mayAccess :: ResourceAccess -> SomeResource es -> Bool
mayAccess access resource = access.superuser || case resource.scope of
  PersonResource -> access.owner == resource.owner
  ChatResource ->
    access.owner.platform == resource.owner.platform
      && access.owner.chatId == resource.owner.chatId

releaseUser :: Prim :> es => IORef (ManagerState es) -> ResourceId -> Maybe Concurrency.Handle -> Eff es ()
releaseUser stateRef resourceId user =
  atomicModifyIORef' stateRef \state ->
    let clear resource = case resource.availability of
          Available users activeUses -> resource
            { availability = Available (maybe users (\userHandle -> Map.update decrement userHandle users) user) (max 0 (activeUses - 1))
            }
          Destroying -> resource
    in (state{resources = Map.adjust clear resourceId state.resources}, ())
  where
    decrement count = if count <= 1 then Nothing else Just (count - 1)

cancelAndAwait :: Concurrency.Concurrency :> es => Concurrency.Handle -> Eff es ()
cancelAndAwait userHandle = do
  void (Concurrency.cancel userHandle.handleId)
  Concurrency.await userHandle

removeDestroyed :: Prim :> es => IORef (ManagerState es) -> ResourceId -> Eff es ()
removeDestroyed stateRef resourceId =
  atomicModifyIORef' stateRef \state -> (state{resources = Map.delete resourceId state.resources}, ())

restoreDestroyed
  :: (Storage.Storage :> es, Prim :> es, IOE :> es)
  => IORef (ManagerState es)
  -> ResourceId
  -> Eff es ()
restoreDestroyed stateRef resourceId = do
  now <- liftIO getCurrentTime
  restored <- atomicModifyIORef' stateRef \state ->
    case Map.lookup resourceId state.resources of
      Nothing -> (state, Nothing)
      Just resource ->
        let expiresAt = (`addUTCTime` now) . fromIntegral <$> resource.ttlSeconds
            resource' = resource{availability = Available Map.empty 0, expiresAt}
        in (state{resources = Map.insert resourceId resource' state.resources}, Just (resource.persistent, (,) <$> resource.ttlSeconds <*> expiresAt))
  for_ restored \(persistent, lifetime) -> void (persistLifetime persistent resourceId lifetime)

expiryFromNow :: IOE :> es => Maybe Int -> Eff es (Maybe UTCTime)
expiryFromNow ttlSeconds = do
  now <- liftIO getCurrentTime
  pure $ (`addUTCTime` now) . fromIntegral <$> ttlSeconds

reclaimExpired
  :: (Storage.Storage :> es, Concurrency.Concurrency :> es, Prim :> es, IOE :> es)
  => IORef (ManagerState es)
  -> Eff es ()
reclaimExpired stateRef = forever do
  -- ponytail: polling is sufficient for the small resource set; use a deadline queue if scan cost becomes measurable.
  Concurrency.sleepMicroseconds 100_000
  now <- liftIO getCurrentTime
  resourceIds <- Map.keys . (.resources) <$> readIORef stateRef
  for_ resourceIds \resourceId ->
    beginExpire stateRef now resourceId >>= traverse_ \(cleanup, persistent) ->
      void (finishDestroy stateRef resourceId cleanup persistent)

beginExpire
  :: Prim :> es
  => IORef (ManagerState es)
  -> UTCTime
  -> ResourceId
  -> Eff es (Maybe (Eff es (Either Text ()), Bool))
beginExpire stateRef now resourceId =
  atomicModifyIORef' stateRef \state ->
    case Map.lookup resourceId state.resources of
      Just resource
        | Just expiresAt <- resource.expiresAt
        , expiresAt <= now
        , Available _ 0 <- resource.availability ->
            let updated = resource{availability = Destroying}
            in (state{resources = Map.insert resourceId updated state.resources}, Just (resource.destroy, resource.persistent))
      _ -> (state, Nothing)

finishDestroy
  :: (Storage.Storage :> es, Prim :> es, IOE :> es)
  => IORef (ManagerState es)
  -> ResourceId
  -> Eff es (Either Text ())
  -> Bool
  -> Eff es (Either ResourceError ())
finishDestroy stateRef resourceId cleanup persistent =
  trySync cleanup >>= \case
    Right (Right ()) ->
      if persistent
        then trySync (ResourceStorage.deleteResource resourceId) >>= \case
          Right () -> removeDestroyed stateRef resourceId $> Right ()
          Left err -> restoreDestroyed stateRef resourceId $> Left (ResourceCleanupFailed (conciseException err))
        else removeDestroyed stateRef resourceId $> Right ()
    Right (Left err) -> restoreDestroyed stateRef resourceId $> Left (ResourceCleanupFailed err)
    Left err -> restoreDestroyed stateRef resourceId $> Left (ResourceCleanupFailed (conciseException err))

shutdown :: (Concurrency.Concurrency :> es, Concurrent :> es, Prim :> es) => IORef (ManagerState es) -> Eff es ()
shutdown stateRef = do
  createGates <- atomicModifyIORef' stateRef \state ->
    (state{accepting = False}, map snd (Map.elems state.creating))
  traverse_ MVar.takeMVar createGates
  entries <- atomicModifyIORef' stateRef \state ->
    let mark resource = resource{availability = Destroying}
        entries = Map.elems state.resources
    in (state{resources = Map.map mark state.resources}, entries)
  for_ entries \resource -> do
    case resource.availability of
      Available users _ -> traverse_ cancelAndAwait (Map.keys users)
      Destroying -> pure ()
    unless resource.persistent $ void (trySync resource.destroy)
