{-# LANGUAGE AllowAmbiguousTypes #-}

module Bot.Resource.Types
  ( ResourceId
  , ResourceOwner (..)
  , ResourceAccess (..)
  , ResourceScope (..)
  , Init (..)
  , ResourceObject (..)
  , ResourcePersistence (..)
  , ResourceLoader (..)
  , resourceLoader
  , SomeResourceObject (..)
  , ResourceError (..)
  , ttlFromMinutes
  , ownerFromMessage
  , accessFromMessage
  )
where

import Bot.Core.Message
import Bot.Prelude

type ResourceId = Text

data ResourceOwner = ResourceOwner
  { platform :: !ChatPlatform
  , chatId :: !Text
  , senderId :: !Text
  }
  deriving stock (Eq, Ord, Show)

data ResourceAccess = ResourceAccess
  { owner :: !ResourceOwner
  , superuser :: !Bool
  }
  deriving stock (Eq, Show)

data ResourceScope = PersonResource | ChatResource
  deriving stock (Eq, Show)

data Init a = Init
  { message :: !IncomingMessage
  , arguments :: !a
  }

class Typeable a => ResourceObject m a where
  type CreationArgs a
  resourceTypeName :: proxy a -> Text
  resourceScope :: proxy a -> ResourceScope
  resourceScope _ = PersonResource
  resourceIdPrefix :: proxy a -> Text
  resourceIdPrefix _ = "res"
  resourcePersistence :: proxy a -> ResourcePersistence m a
  resourcePersistence _ = EphemeralResource
  resourceListed :: proxy a -> Bool
  resourceListed _ = True
  resourceTTLSeconds :: CreationArgs a -> Either Text (Maybe Int)
  resourceTTLSeconds _ = Right Nothing
  createResourceObject :: Init (CreationArgs a) -> m (Either Text a)
  destroyResourceObject :: a -> m (Either Text ())
  describeResourceObject :: a -> Either Text Text -> m Text
  probeResourceObject :: a -> m (Either Text Text)
  detailResourceObject :: Monad m => a -> m Text
  detailResourceObject object = probeResourceObject object >>= describeResourceObject object

data ResourcePersistence m a
  = EphemeralResource
  | PersistentResource
      { encodeResource :: a -> Text
      , restoreResource :: Text -> m (Either Text a)
      }

data ResourceLoader m where
  ResourceLoader :: ResourceObject m a => Proxy a -> ResourceLoader m

resourceLoader :: forall a m. ResourceObject m a => ResourceLoader m
resourceLoader = ResourceLoader (Proxy @a)

data SomeResourceObject = SomeResourceObject
  { resourceId :: !ResourceId
  , resourceType :: !Text
  , sessionId :: !(Maybe Text)
  , description :: !Text
  , probeResult :: !(Either Text Text)
  , remainingLifeMinutes :: !(Maybe Int)
  }
  deriving stock (Eq, Show)

data ResourceError
  = MissingResourceIdentity
  | ResourceNotFoundOrNotOwned
  | ResourceTypeMismatch
  | ResourceUnavailable
  | InvalidResourceName
  | ResourceNameAlreadyExists
  | ResourceCreationFailed !Text
  | ResourceRenameFailed !Text
  | ResourceLifetimeUpdateFailed !Text
  | ResourceCleanupFailed !Text
  deriving stock (Eq, Show)

ttlFromMinutes :: Int -> Either Text (Maybe Int)
ttlFromMinutes minutes
  | minutes < 5 = Left "TTL must be at least 5 minutes."
  | minutes > maxBound `div` 60 = Left "TTL is too large."
  | otherwise = Right (Just (minutes * 60))

ownerFromMessage :: IncomingMessage -> Either ResourceError ResourceOwner
ownerFromMessage message =
  ResourceOwner message.platform
    <$> maybeToRight MissingResourceIdentity (show <$> message.chatId <|> listToMaybe message.chatAliases)
    <*> maybeToRight MissingResourceIdentity message.senderId

accessFromMessage :: IncomingMessage -> Either ResourceError ResourceAccess
accessFromMessage message =
  ResourceAccess <$> ownerFromMessage message <*> pure message.digest.senderIsSuperuser
