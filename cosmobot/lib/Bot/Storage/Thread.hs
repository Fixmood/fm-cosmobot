{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.Storage.Thread
Description : Persistent platform thread graph
Stability   : experimental
-}

module Bot.Storage.Thread
  ( ThreadStore
  , ActiveThreadHandle
  , ThreadRow (..)
  , ActiveThreadInfo (..)
  , newThreadStore
  , lookupThreadTranscript
  , lookupRecentUserThread
  , lookupThreadMessageIds
  , lookupActiveThreadRunId
  , lookupActiveThreadReply
  , lookupActiveThreadPendingSteers
  , rememberThreadTranscript
  , rememberThreadTranscriptFrom
  , rememberActiveThread
  , addActiveThreadMessage
  , enqueueActiveThreadSteer
  , drainActiveThreadSteers
  , completeActiveThreadSteering
  , updateActiveThread
  , finishActiveThread
  , finishActiveThreadCurrent
  , haltThread
  , haltThreadForMessage
  , listActiveThreadsForMessage
  , haltActiveThreadsForMessage
  , loadThreadRows
  )
where

import Bot.Core.Message
import Bot.Core.Thread
import Bot.Core.Transcript
import Bot.Effect.Concurrency (Handle (..), Id)
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude hiding (Handle, newIORef, readIORef, atomicModifyIORef, writeIORef, atomicModifyIORef')
import Bot.Storage.Prelude
import qualified Effectful.Concurrent.MVar as MVar
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import qualified Data.Int as Int
import Effectful.Prim.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Database.Selda.Backend as SeldaBackend
import qualified Database.Selda.SQLite as SeldaSQLite

data ThreadStore = ThreadStore
  { unThreadStore :: IORef ThreadState
  , activeThreadStore :: IORef (Map ActiveThreadKey ActiveThread)
  }

data ActiveThreadKey
  = ActiveThreadId !Id
  | ActiveThreadMessage !ThreadMessageKey
  deriving (Eq, Ord)

data ThreadState = ThreadState
  { threadTree :: !ThreadTree
  , threadIds :: !(Map ThreadMessageKey Integer)
  , recentThreadIds :: ![ThreadMessageKey]
  , recentUserThreads :: !(Map RecentUserKey RecentUserThread)
  }

data RecentUserKey = RecentUserKey
  { recentPlatform :: !ChatPlatform
  , recentChatId :: !(Maybe Integer)
  , recentSenderId :: !Text
  }
  deriving (Eq, Ord, Show)

data RecentUserThread = RecentUserThread
  { recentMessageKey :: !ThreadMessageKey
  , recentTranscript :: !Transcript
  , recentAt :: !Time.UTCTime
  }

data StoredThreadNode = StoredThreadNode
  { threadStorageId :: !Integer
  , treeNode :: !ThreadNode
  }

data ActiveThread = ActiveThread
  { activeChatScope :: !(Maybe ActiveChatScope)
  , activeSenderId :: !(Maybe Text)
  , activeRequestMessageKey :: !(Maybe ThreadMessageKey)
  , activeRunId :: !Text
  , activePrompt :: !Text
  , activeParentMessageKey :: !(Maybe ThreadMessageKey)
  , activeReplyMessageKeys :: !(IORef [ThreadMessageKey])
  , activeSteering :: !(MVar.MVar SteeringState)
  , activeCurrent :: !(IORef Transcript)
  , activeDone :: !(MVar.MVar Transcript)
  , activeHandle :: !Handle
  }

newtype ActiveThreadHandle = ActiveThreadHandle ActiveThread

data SteeringState
  = SteeringOpen !(Seq MessageInput)
  | SteeringCompleted
  | SteeringFinishing
  deriving (Eq)

data ActiveChatScope = ActiveChatScope !ChatPlatform !(Either Integer Text)
  deriving (Eq)

data ActiveThreadInfo = ActiveThreadInfo
  { id :: !Id
  , prompt :: !Text
  }
  deriving (Eq, Show)

data ThreadRow = ThreadRow
  { messageKey :: !ThreadMessageKey
  , threadStorageId :: !(Maybe Integer)
  , parentMessageKey :: !(Maybe ThreadMessageKey)
  , messagesJson :: !Text
  }
  deriving (Eq, Show)

data ThreadStorageRow = ThreadStorageRow
  { id :: ID ThreadStorageRow
  , platform_key :: Text
  , chat_id :: Maybe Int.Int64
  , sender_id :: Maybe Text
  , message_id :: Text
  , thread_id :: Maybe Int.Int64
  , parent_chat_id :: Maybe Int.Int64
  , parent_message_id :: Maybe Text
  , messages_json :: Text
  }
  deriving (Generic)

instance SqlRow ThreadStorageRow

data RecentThreadStorageRow = RecentThreadStorageRow
  { id :: ID RecentThreadStorageRow
  , platform_key :: Text
  , chat_id :: Maybe Int.Int64
  , sender_id :: Text
  , message_id :: Text
  , recent_at :: Int.Int64
  }
  deriving (Generic)

instance SqlRow RecentThreadStorageRow

threadRows :: Table ThreadStorageRow
threadRows =
  table "threads"
    [ #id :- autoPrimary
    , #platform_key :- index
    , #chat_id :- index
    , #sender_id :- index
    , #message_id :- index
    , #thread_id :- index
    , #parent_message_id :- index
    ]

recentThreadRows :: Table RecentThreadStorageRow
recentThreadRows =
  table "recent_user_threads"
    [ #id :- autoPrimary
    , #platform_key :- index
    , #chat_id :- index
    , #sender_id :- index
    , #message_id :- index
    , #recent_at :- index
    ]

newThreadStore :: Prim :> es => Eff es ThreadStore
newThreadStore = do
  ref <- newIORef ThreadState{threadTree = emptyThreadTree, threadIds = Map.empty, recentThreadIds = [], recentUserThreads = Map.empty}
  activeRef <- newIORef Map.empty
  pure ThreadStore{unThreadStore = ref, activeThreadStore = activeRef}

lookupThreadTranscript :: (Prim :> es, Concurrent :> es, Storage.Storage :> es) => ThreadStore -> ThreadMessageKey -> Eff es (Maybe Transcript)
lookupThreadTranscript store@ThreadStore{activeThreadStore = activeRef} messageKey = do
  finished <- fmap (.treeNode.transcript) <$> lookupStoredThreadNode store messageKey
  case finished of
    Just transcript ->
      pure (Just transcript)
    Nothing -> do
      active <- Map.lookup (ActiveThreadMessage messageKey) <$> readIORef activeRef
      traverse (MVar.readMVar . (.activeDone)) active

lookupRecentUserThread
  :: (Prim :> es, IOE :> es, KatipE :> es, Concurrent :> es, Storage.Storage :> es)
  => ThreadStore
  -> IncomingMessage
  -> Int
  -> Eff es (Maybe (ThreadMessageKey, Transcript, Int))
lookupRecentUserThread store@ThreadStore{unThreadStore = ref} message windowSeconds = do
  now <- liftIO Time.getCurrentTime
  let lookupKey = RecentUserKey
        { recentPlatform = message.platform
        , recentChatId = message.chatId
        , recentSenderId = fromMaybe "-" message.senderId
        }
  memoryCandidate <- case message.senderId of
    Nothing -> pure Nothing
    Just senderId -> do
      state <- readIORef ref
      pure (Map.lookup lookupKey{recentSenderId = senderId} state.recentUserThreads)
  persistedCandidate <- case memoryCandidate of
    Just candidate -> pure (Just candidate)
    Nothing -> loadPersistedRecentUserThread store message
  let result = persistedCandidate >>= \RecentUserThread{recentMessageKey, recentTranscript, recentAt} -> do
        let age = max 0 (floor (Time.diffUTCTime now recentAt) :: Int)
        guard (age <= windowSeconds)
        pure (recentMessageKey, recentTranscript, age)
      lookupUserId = fromMaybe "-" message.senderId
      lookupPlatform = show message.platform :: String
      lookupChat = show message.chatId :: String
      lookupFound = isJust result
  logInfo [i|thread_lookup_recent user_id=#{lookupUserId} platform=#{lookupPlatform} chat=#{lookupChat} found=#{lookupFound}|]
  for_ result \(messageKey, transcript, _) ->
    case message.senderId of
      Just senderId -> atomicModifyIORef' ref \state ->
        (state{recentUserThreads = Map.insert lookupKey{recentSenderId = senderId}
          RecentUserThread{recentMessageKey = messageKey, recentTranscript = transcript, recentAt = now}
          state.recentUserThreads}, ())
      Nothing -> pure ()
  pure result

lookupThreadMessageIds :: (Prim :> es, Storage.Storage :> es) => ThreadStore -> ThreadMessageKey -> Eff es [MessageId]
lookupThreadMessageIds store@ThreadStore{activeThreadStore = activeRef} =
  go []
  where
    go visited messageKey
      | messageKey `elem` visited =
          pure []
      | otherwise = do
          active <- Map.lookup (ActiveThreadMessage messageKey) <$> readIORef activeRef
          (parentMessageKey, messageIds) <- case active of
            Just activeThread -> do
              activeMap <- readIORef activeRef
              let ids =
                    [ aliasKey.messageId
                    | (ActiveThreadMessage aliasKey, candidate) <- Map.toList activeMap
                    , candidate.activeHandle.handleId == activeThread.activeHandle.handleId
                    ]
              pure (activeThread.activeParentMessageKey, ids)
            Nothing -> do
              node <- lookupStoredThreadNode store messageKey
              case node of
                Nothing ->
                  loadThreadMessageIdsFromStorage messageKey <&> \ids -> (Nothing, ids)
                Just target -> do
                  ids <- loadThreadMessageIdsFromStorage messageKey
                  pure (target.treeNode.parentMessageKey, ids)
          parentIds <- maybe (pure []) (go (messageKey : visited)) parentMessageKey
          pure (ordNub (parentIds <> messageIds))

lookupActiveThreadReply :: Prim :> es => ThreadStore -> IncomingMessage -> ThreadMessageKey -> Eff es (Maybe (Bool, Transcript))
lookupActiveThreadReply ThreadStore{activeThreadStore = activeRef} message messageKey = do
  active <- Map.lookup (ActiveThreadMessage messageKey) <$> readIORef activeRef
  traverse (replyState message) active
  where
    replyState reply thread = do
      transcript <- readIORef thread.activeCurrent
      let isOwner = maybe False ((== thread.activeSenderId) . Just) reply.senderId
      pure (isOwner, transcript)

lookupActiveThreadRunId :: Prim :> es => ThreadStore -> ThreadMessageKey -> Eff es (Maybe Text)
lookupActiveThreadRunId ThreadStore{activeThreadStore = activeRef} messageKey =
  fmap (.activeRunId) . Map.lookup (ActiveThreadMessage messageKey) <$> readIORef activeRef

lookupActiveThreadPendingSteers :: (Prim :> es, Concurrent :> es) => ThreadStore -> ThreadMessageKey -> Eff es (Maybe Int)
lookupActiveThreadPendingSteers ThreadStore{activeThreadStore = activeRef} messageKey = do
  active <- Map.lookup (ActiveThreadMessage messageKey) <$> readIORef activeRef
  traverse pendingSteers active
  where
    pendingSteers activeThread =
      MVar.readMVar activeThread.activeSteering <&> \case
        SteeringOpen queued -> Seq.length queued
        SteeringCompleted -> 0
        SteeringFinishing -> 0

rememberActiveThread
  :: (Prim :> es, Concurrent :> es)
  => ThreadStore
  -> Text
  -> Maybe ThreadMessageKey
  -> Maybe ThreadMessageKey
  -> IncomingMessage
  -> Text
  -> Handle
  -> Transcript
  -> Eff es (Maybe ActiveThreadHandle)
rememberActiveThread ThreadStore{activeThreadStore = activeRef} activeRunId parentMessageKey messageKey message prompt activeHandle transcript = do
  replyMessageKeys <- newIORef []
  steering <- MVar.newMVar (SteeringOpen Seq.empty)
  current <- newIORef transcript
  done <- MVar.newEmptyMVar
  let active = ActiveThread
        { activeChatScope = activeChatScopeFromMessage message
        , activeSenderId = message.senderId
        , activeRequestMessageKey = messageKey
        , activeRunId
        , activePrompt = prompt
        , activeParentMessageKey = parentMessageKey
        , activeReplyMessageKeys = replyMessageKeys
        , activeSteering = steering
        , activeCurrent = current
        , activeDone = done
        , activeHandle
        }
  atomicModifyIORef' activeRef \activeMap ->
    let keys = ActiveThreadId activeHandle.handleId : map ActiveThreadMessage (maybeToList messageKey)
    in (foldl' (\next key -> Map.insert key active next) activeMap keys, ())
  pure (Just (ActiveThreadHandle active))

addActiveThreadMessage :: (Prim :> es, Concurrent :> es) => ThreadStore -> ActiveThreadHandle -> ThreadMessageKey -> Eff es ()
addActiveThreadMessage ThreadStore{activeThreadStore = activeRef} (ActiveThreadHandle active) messageKey =
  MVar.modifyMVar_ active.activeSteering \steeringState -> do
    unless (steeringState == SteeringFinishing) do
      addMessageAlias activeRef active messageKey
      atomicModifyIORef' active.activeReplyMessageKeys \messageKeys ->
        let next = if messageKey `elem` messageKeys then messageKeys else messageKey : messageKeys
        in (next, ())
    pure steeringState

enqueueActiveThreadSteer
  :: (Prim :> es, Concurrent :> es)
  => ThreadStore
  -> IncomingMessage
  -> MessageInput
  -> Eff es Bool
enqueueActiveThreadSteer ThreadStore{activeThreadStore = activeRef} message steer =
  case threadMessageKey message <$> message.replyToMessageId of
    Nothing ->
      pure False
    Just replyKey -> do
      active <- Map.lookup (ActiveThreadMessage replyKey) <$> readIORef activeRef
      case active of
        Just activeThread
          | maybe False ((== activeThread.activeSenderId) . Just) message.senderId ->
              MVar.modifyMVar activeThread.activeSteering \case
                SteeringOpen queued -> do
                  traverse_ (addMessageAlias activeRef activeThread . threadMessageKey message) message.messageId
                  pure (SteeringOpen (queued Seq.|> steer), True)
                steeringState ->
                  pure (steeringState, False)
        _ ->
          pure False

drainActiveThreadSteers :: Concurrent :> es => ActiveThreadHandle -> Eff es [MessageInput]
drainActiveThreadSteers (ActiveThreadHandle active) =
  MVar.modifyMVar active.activeSteering \case
    SteeringOpen queued ->
      pure (SteeringOpen Seq.empty, Foldable.toList queued)
    steeringState ->
      pure (steeringState, [])

completeActiveThreadSteering :: Concurrent :> es => ActiveThreadHandle -> Eff es (Maybe [MessageInput])
completeActiveThreadSteering (ActiveThreadHandle active) =
  MVar.modifyMVar active.activeSteering \case
    SteeringOpen queued
      | Seq.null queued ->
          pure (SteeringCompleted, Nothing)
      | otherwise ->
          pure (SteeringOpen Seq.empty, Just (Foldable.toList queued))
    steeringState ->
      pure (steeringState, Nothing)

addMessageAlias
  :: Prim :> es
  => IORef (Map ActiveThreadKey ActiveThread)
  -> ActiveThread
  -> ThreadMessageKey
  -> Eff es ()
addMessageAlias activeRef active messageKey = do
  atomicModifyIORef' activeRef \activeMap ->
    (Map.insert (ActiveThreadMessage messageKey) active activeMap, ())

updateActiveThread :: Prim :> es => ActiveThreadHandle -> Transcript -> Eff es ()
updateActiveThread (ActiveThreadHandle active) transcript =
  writeIORef active.activeCurrent transcript

finishActiveThread
  :: (Prim :> es, IOE :> es, KatipE :> es, Concurrent :> es, Storage.Storage :> es)
  => ThreadStore
  -> ActiveThreadHandle
  -> Transcript
  -> Eff es ()
finishActiveThread store@ThreadStore{activeThreadStore = activeRef} (ActiveThreadHandle active) transcript = do
  replyMessageKeys <- MVar.modifyMVar active.activeSteering \case
    SteeringFinishing ->
      pure (SteeringFinishing, Nothing)
    _ -> do
      keys <- readIORef active.activeReplyMessageKeys
      pure (SteeringFinishing, Just keys)
  let persistenceKeys = fromMaybe [] replyMessageKeys
      recentKey = listToMaybe persistenceKeys <|> active.activeRequestMessageKey
  updateActiveThread (ActiveThreadHandle active) transcript
  traverse_ (\messageKey -> rememberThreadTranscriptFrom store active.activeParentMessageKey (Just messageKey) transcript) persistenceKeys
  now <- liftIO Time.getCurrentTime
  for_ recentKey \messageKey -> do
    rememberRecentUserThread store active.activeChatScope active.activeSenderId messageKey transcript now
    let persistedUserId = fromMaybe "-" active.activeSenderId
        persistedMessageId = messageIdText messageKey.messageId
        persistedSource = if null persistenceKeys then "request" else "response" :: Text
    logInfo [i|thread_persist_recent user_id=#{persistedUserId} thread_id=#{persistedMessageId} source=#{persistedSource}|]
  void $ MVar.tryPutMVar active.activeDone transcript
  atomicModifyIORef' activeRef \activeMap ->
    (Map.filter ((/= active.activeHandle.handleId) . (.activeHandle.handleId)) activeMap, ())

rememberRecentUserThread
  :: (Prim :> es, Storage.Storage :> es)
  => ThreadStore
  -> Maybe ActiveChatScope
  -> Maybe Text
  -> ThreadMessageKey
  -> Transcript
  -> Time.UTCTime
  -> Eff es ()
rememberRecentUserThread ThreadStore{unThreadStore = ref} chatScope senderId messageKey transcript now =
  for_ (recentUserKey chatScope senderId) \key ->
    do
      let recent = RecentUserThread
            { recentMessageKey = messageKey
            , recentTranscript = transcript
            , recentAt = now
            }
      atomicModifyIORef' ref \state ->
        (state{recentUserThreads = Map.insert key recent state.recentUserThreads}, ())
      persistRecentUserThread key recent

loadPersistedRecentUserThread
  :: (Prim :> es, Concurrent :> es, Storage.Storage :> es)
  => ThreadStore
  -> IncomingMessage
  -> Eff es (Maybe RecentUserThread)
loadPersistedRecentUserThread store message = do
  ensureThreadTable
  case (message.senderId, activeChatScopeFromMessage message) of
    (Just senderId, Just (ActiveChatScope platform chatScope)) -> do
      rows <- runSelda $ query do
        row <- select recentThreadRows
        restrict (row ! #platform_key .== literal (chatPlatformKey platform))
        restrict (row ! #sender_id .== literal senderId)
        case chatScope of
          Left chatId -> restrict (row ! #chat_id .== literal (Just (fromIntegral chatId :: Int.Int64)))
          Right alias -> restrict (row ! #chat_id .== literal (Nothing :: Maybe Int.Int64))
        order (row ! #recent_at) descending
        pure row
      case listToMaybe rows of
        Nothing -> pure Nothing
        Just row -> do
          let key = ThreadMessageKey
                { platform = platform
                , chatId = fromIntegral <$> row.chat_id
                , senderId = Just row.sender_id
                , messageId = textMessageId row.message_id
                }
          lookupThreadTranscript store key >>= \case
            Just transcript -> pure (Just RecentUserThread
              { recentMessageKey = key
              , recentTranscript = transcript
              , recentAt = recentTimestampToUtc row.recent_at
              })
            Nothing -> pure Nothing
    _ -> pure Nothing

persistRecentUserThread
  :: Storage.Storage :> es
  => RecentUserKey
  -> RecentUserThread
  -> Eff es ()
persistRecentUserThread key RecentUserThread{recentMessageKey, recentAt} = do
  ensureThreadTable
  runSelda $ transaction do
    deleteFrom_ recentThreadRows \row ->
      row ! #platform_key .== literal (chatPlatformKey key.recentPlatform)
        .&& row ! #chat_id .== literal (fromIntegral <$> key.recentChatId)
        .&& row ! #sender_id .== literal key.recentSenderId
    insert_ recentThreadRows [RecentThreadStorageRow
      { id = def
      , platform_key = chatPlatformKey key.recentPlatform
      , chat_id = fromIntegral <$> key.recentChatId
      , sender_id = key.recentSenderId
      , message_id = messageIdText recentMessageKey.messageId
      , recent_at = recentTimestamp recentAt
      }]
    pure ()

recentTimestamp :: Time.UTCTime -> Int.Int64
recentTimestamp timestamp =
  floor (POSIX.utcTimeToPOSIXSeconds timestamp)

recentTimestampToUtc :: Int.Int64 -> Time.UTCTime
recentTimestampToUtc = POSIX.posixSecondsToUTCTime . fromIntegral

recentUserKey :: Maybe ActiveChatScope -> Maybe Text -> Maybe RecentUserKey
recentUserKey (Just (ActiveChatScope platform chatScope)) (Just senderId) =
  Just RecentUserKey
    { recentPlatform = platform
    , recentChatId = either Just (const Nothing) chatScope
    , recentSenderId = senderId
    }
recentUserKey _ _ = Nothing

finishActiveThreadCurrent
  :: (Prim :> es, IOE :> es, KatipE :> es, Storage.Storage :> es, Concurrent :> es)
  => ThreadStore
  -> ActiveThreadHandle
  -> Eff es ()
finishActiveThreadCurrent store (ActiveThreadHandle active) = do
  transcript <- readIORef active.activeCurrent
  finishActiveThread store (ActiveThreadHandle active) transcript

haltThread
  :: (Prim :> es, IOE :> es, KatipE :> es, Storage.Storage :> es, Concurrent :> es)
  => ThreadStore
  -> (Id -> Eff es Bool)
  -> ThreadMessageKey
  -> Eff es Bool
haltThread store@ThreadStore{activeThreadStore = activeRef} cancel messageKey = do
  active <- Map.lookup (ActiveThreadMessage messageKey) <$> readIORef activeRef
  maybe (pure False) (haltActiveThread store cancel) active

haltActiveThread
  :: (Prim :> es, IOE :> es, KatipE :> es, Storage.Storage :> es, Concurrent :> es)
  => ThreadStore
  -> (Id -> Eff es Bool)
  -> ActiveThread
  -> Eff es Bool
haltActiveThread store cancel activeThread = do
  void $ cancel activeThread.activeHandle.handleId
  transcript <- readIORef activeThread.activeCurrent
  finishActiveThread store (ActiveThreadHandle activeThread) transcript
  pure True

haltThreadForMessage
  :: (Prim :> es, IOE :> es, KatipE :> es, Storage.Storage :> es, Concurrent :> es)
  => ThreadStore
  -> (Id -> Eff es Bool)
  -> IncomingMessage
  -> Eff es Bool
haltThreadForMessage store@ThreadStore{activeThreadStore = activeRef} cancel message =
  haltFirst (haltCandidateKeys message)
  where
    haltFirst [] =
      pure False
    haltFirst (messageKey : rest) = do
      active <- Map.lookup (ActiveThreadMessage messageKey) <$> readIORef activeRef
      case active of
        Just activeThread
          | mayManageActiveThread message activeThread ->
              haltThread store cancel messageKey >>= \case
                True -> pure True
                False -> haltFirst rest
        _ -> haltFirst rest

listActiveThreadsForMessage
  :: Prim :> es
  => ThreadStore
  -> IncomingMessage
  -> Eff es [ActiveThreadInfo]
listActiveThreadsForMessage ThreadStore{activeThreadStore = activeRef} message =
  case activeChatScopeFromMessage message of
    Nothing -> pure []
    Just scope -> do
      active <- Map.toList <$> readIORef activeRef
      pure
        [ ActiveThreadInfo activeThread.activeHandle.handleId activeThread.activePrompt
        | (ActiveThreadId{}, activeThread) <- active
        , activeThread.activeChatScope == Just scope
        , mayManageActiveThread message activeThread
        ]

mayManageActiveThread :: IncomingMessage -> ActiveThread -> Bool
mayManageActiveThread message activeThread =
  message.digest.senderIsSuperuser
    || maybe False ((== activeThread.activeSenderId) . Just) message.senderId

haltActiveThreadsForMessage
  :: (Prim :> es, IOE :> es, KatipE :> es, Storage.Storage :> es, Concurrent :> es)
  => ThreadStore
  -> (Id -> Eff es Bool)
  -> IncomingMessage
  -> [Id]
  -> Eff es [Id]
haltActiveThreadsForMessage store cancel message requestedIds = do
  active <- listActiveThreadsForMessage store message
  fmap catMaybes $ forM active \threadInfo ->
    if threadInfo.id `elem` requestedIds
      then haltThreadById store cancel threadInfo.id <&> \halted -> threadInfo.id <$ guard halted
      else pure Nothing

haltThreadById
  :: (Prim :> es, IOE :> es, KatipE :> es, Storage.Storage :> es, Concurrent :> es)
  => ThreadStore
  -> (Id -> Eff es Bool)
  -> Id
  -> Eff es Bool
haltThreadById store@ThreadStore{activeThreadStore = activeRef} cancel requestedId = do
  active <- Map.lookup (ActiveThreadId requestedId) <$> readIORef activeRef
  maybe (pure False) (haltActiveThread store cancel) active

activeChatScopeFromMessage :: IncomingMessage -> Maybe ActiveChatScope
activeChatScopeFromMessage message =
  ActiveChatScope message.platform
    <$> (Left <$> message.chatId <|> Right <$> listToMaybe message.chatAliases)

haltCandidateKeys :: IncomingMessage -> [ThreadMessageKey]
haltCandidateKeys message =
  ordNub (catMaybes [replyKey, currentKey])
  where
    replyKey =
      threadMessageKey message <$> message.replyToMessageId
    currentKey =
      threadMessageKey message <$> message.messageId

rememberThreadTranscript :: (Prim :> es, KatipE :> es, Storage.Storage :> es) => ThreadStore -> Maybe ThreadMessageKey -> Transcript -> Eff es ()
rememberThreadTranscript store =
  rememberThreadTranscriptFrom store Nothing

rememberThreadTranscriptFrom
  :: (Prim :> es, KatipE :> es, Storage.Storage :> es)
  => ThreadStore
  -> Maybe ThreadMessageKey
  -> Maybe ThreadMessageKey
  -> Transcript
  -> Eff es ()
rememberThreadTranscriptFrom _ _ Nothing _ =
  pure ()
rememberThreadTranscriptFrom store@ThreadStore{unThreadStore = ref} parentMessageKey (Just messageKey) transcript = do
  ensureThreadTable
  parentNode <- lookupStoredThreadNodeMaybe store parentMessageKey
  existingNode <- lookupStoredThreadNodeMaybe store (Just messageKey)
  let requestedThreadStorageId = (.threadStorageId) <$> (parentNode <|> existingNode)
      (storageParentMessageKey, storedMessages) = transcriptMessagesForStorage parentMessageKey parentNode transcript
  persistedThreadStorageId <-
    (Just <$> saveThreadMessages messageKey requestedThreadStorageId storageParentMessageKey (messagesJson storedMessages))
      `catchSync` \err ->
        logError [i|Failed to persist thread: #{show err :: String}|] $> Nothing
  for_ persistedThreadStorageId \threadStorageId -> do
    let persistedPlatform = show messageKey.platform :: String
        persistedChat = show messageKey.chatId :: String
        persistedMessageId = messageIdText messageKey.messageId
        persistedParentMessageId = maybe "-" (messageIdText . (.messageId)) parentMessageKey
    logInfo [i|FM thread persisted: platform=#{persistedPlatform} chat=#{persistedChat} message_id=#{persistedMessageId} thread_id=#{threadStorageId} parent_message_id=#{persistedParentMessageId}|]
    atomicModifyIORef' ref \threadState ->
      let node =
            StoredThreadNode
              { threadStorageId
              , treeNode = ThreadNode{messageKey, parentMessageKey, transcript}
              }
      in (cacheThreadNode messageKey node threadState, ())

lookupStoredThreadNode :: (Prim :> es, Storage.Storage :> es) => ThreadStore -> ThreadMessageKey -> Eff es (Maybe StoredThreadNode)
lookupStoredThreadNode store messageKey =
  lookupStoredThreadNodeMaybe store (Just messageKey)

lookupStoredThreadNodeMaybe :: (Prim :> es, Storage.Storage :> es) => ThreadStore -> Maybe ThreadMessageKey -> Eff es (Maybe StoredThreadNode)
lookupStoredThreadNodeMaybe _ Nothing =
  pure Nothing
lookupStoredThreadNodeMaybe store@ThreadStore{unThreadStore = ref} (Just messageKey) = do
  cached <- do
    threadState <- readIORef ref
    pure do
      treeNode <- lookupThreadNode messageKey threadState.threadTree
      threadStorageId <- Map.lookup messageKey threadState.threadIds
      pure StoredThreadNode{threadStorageId, treeNode}
  case cached of
    Just node ->
      pure (Just node)
    Nothing ->
      loadThreadNodeFromStorage store [] messageKey

loadThreadNodeFromStorage :: (Prim :> es, Storage.Storage :> es) => ThreadStore -> [ThreadMessageKey] -> ThreadMessageKey -> Eff es (Maybe StoredThreadNode)
loadThreadNodeFromStorage store@ThreadStore{unThreadStore = ref} visited messageKey
  | messageKey `elem` visited =
      pure Nothing
  | otherwise = do
      row <- loadThreadRow messageKey
      case row >>= decodeStoredThread of
        Nothing ->
          pure Nothing
        Just stored -> do
          parentNode <- case stored.storedParentMessageKey of
            Nothing ->
              pure Nothing
            Just parentMessageKey ->
              lookupStoredThreadNodeMaybe store (Just parentMessageKey)
                >>= maybe (loadThreadNodeFromStorage store (messageKey : visited) parentMessageKey) (pure . Just)
          let node = StoredThreadNode
                { threadStorageId = stored.storedThreadStorageId
                , treeNode = ThreadNode
                    { messageKey = messageKey
                    , parentMessageKey = stored.storedParentMessageKey
                    , transcript = storedTranscriptFromMessages parentNode stored.storedMessages
                    }
                }
          atomicModifyIORef' ref \threadState ->
            (cacheThreadNode messageKey node threadState, ())
          pure (Just node)

data StoredThread = StoredThread
  { storedThreadStorageId :: !Integer
  , storedParentMessageKey :: !(Maybe ThreadMessageKey)
  , storedMessages :: ![LLM.ChatMessage]
  }

decodeStoredThread :: ThreadRow -> Maybe StoredThread
decodeStoredThread row = do
  messages <- decodeMessages row.messagesJson
  let threadStorageId = fromMaybe 0 row.threadStorageId
  pure StoredThread{storedThreadStorageId = threadStorageId, storedParentMessageKey = row.parentMessageKey, storedMessages = messages}

storedTranscriptFromMessages :: Maybe StoredThreadNode -> [LLM.ChatMessage] -> Transcript
storedTranscriptFromMessages parentNode messages =
  case parentNode of
    Nothing ->
      Transcript (Seq.fromList messages)
    Just parent ->
      Transcript (parent.treeNode.transcript.messages <> Seq.fromList messages)

decodeMessages :: Text -> Maybe [LLM.ChatMessage]
decodeMessages =
  either (const Nothing) Just . Aeson.eitherDecodeStrict' . TextEncoding.encodeUtf8

ensureThreadTable :: Storage.Storage :> es => Eff es ()
ensureThreadTable =
  runSelda (transaction migrateThreadTable)

migrateThreadTable :: SeldaT SeldaSQLite.SQLite IO ()
migrateThreadTable =
  SeldaBackend.withBackend \backend -> liftIO do
    runStatement backend
      "CREATE TABLE IF NOT EXISTS threads (id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, platform_key TEXT NOT NULL, chat_id BIGINT NULL, sender_id TEXT NULL, message_id TEXT NOT NULL, thread_id BIGINT NULL, parent_chat_id BIGINT NULL, parent_message_id TEXT NULL, messages_json TEXT NOT NULL)"
    runStatement backend
      "CREATE TABLE IF NOT EXISTS recent_user_threads (id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, platform_key TEXT NOT NULL, chat_id BIGINT NULL, sender_id TEXT NOT NULL, message_id TEXT NOT NULL, recent_at BIGINT NOT NULL)"
    (_, rows) <- SeldaBackend.runStmt backend "PRAGMA table_info(threads)" []
    let columns = [name | _ : SeldaBackend.SqlString name : _ <- rows]
    unless ("sender_id" `elem` columns) $
      runStatement backend "ALTER TABLE threads ADD COLUMN sender_id TEXT NULL"
    traverse_ (runStatement backend)
      [ "CREATE INDEX IF NOT EXISTS threads_platform_key_idx ON threads(platform_key)"
      , "CREATE INDEX IF NOT EXISTS threads_chat_id_idx ON threads(chat_id)"
      , "CREATE INDEX IF NOT EXISTS threads_sender_id_idx ON threads(sender_id)"
      , "CREATE INDEX IF NOT EXISTS threads_message_id_idx ON threads(message_id)"
      , "CREATE INDEX IF NOT EXISTS threads_thread_id_idx ON threads(thread_id)"
      , "CREATE INDEX IF NOT EXISTS threads_parent_message_id_idx ON threads(parent_message_id)"
      , "CREATE INDEX IF NOT EXISTS threads_lookup_idx ON threads(platform_key, chat_id, sender_id, message_id)"
      ]
  where
    runStatement backend statement =
      void (SeldaBackend.runStmt backend statement [])

loadThreadRows :: Storage.Storage :> es => Eff es [ThreadRow]
loadThreadRows = do
  ensureThreadTable
  rows <- runSelda $
    query do
      row <- select threadRows
      order (row ! #id) ascending
      pure row
  pure (map threadRowFromStorage rows)

loadThreadRow :: Storage.Storage :> es => ThreadMessageKey -> Eff es (Maybe ThreadRow)
loadThreadRow targetMessageKey = do
  ensureThreadTable
  exact <- loadMatchingThreadRow targetMessageKey
  case exact of
    Just row -> pure (Just row)
    Nothing
      | isJust targetMessageKey.senderId ->
          loadMatchingThreadRow targetMessageKey{senderId = Nothing}
      | otherwise -> pure Nothing

loadMatchingThreadRow :: Storage.Storage :> es => ThreadMessageKey -> Eff es (Maybe ThreadRow)
loadMatchingThreadRow targetMessageKey = do
  rows <- runSelda $
    query $
      queryLimit 0 1 do
        row <- select threadRows
        restrict (threadKeyMatches targetMessageKey row)
        pure row
  pure (threadRowFromStorage <$> viaNonEmpty head rows)

loadThreadMessageIdsFromStorage :: Storage.Storage :> es => ThreadMessageKey -> Eff es [MessageId]
loadThreadMessageIdsFromStorage messageKey = do
  target <- loadThreadRow messageKey
  case target of
    Nothing ->
      pure []
    Just targetRow ->
      case targetRow.threadStorageId of
        Nothing ->
          pure []
        Just targetThreadStorageId -> do
          rows <- runSelda $
            query do
              row <- select threadRows
              restrict (row ! #thread_id .== literal (Just (fromIntegral targetThreadStorageId :: Int.Int64)))
              order (row ! #id) ascending
              pure row
          pure
            [ row.messageKey.messageId
            | row <- map threadRowFromStorage rows
            , row.parentMessageKey == targetRow.parentMessageKey
            , row.messagesJson == targetRow.messagesJson
            ]

saveThreadMessages :: Storage.Storage :> es => ThreadMessageKey -> Maybe Integer -> Maybe ThreadMessageKey -> Text -> Eff es Integer
saveThreadMessages messageKey requestedThreadStorageId parentMessageKey storedMessagesJson = do
  ensureThreadTable
  runSelda $ transaction do
    deleteFrom_ threadRows \row ->
      threadKeyMatches messageKey row
    case requestedThreadStorageId of
      Just threadStorageId ->
        insert_ threadRows [threadStorageRow (Just threadStorageId)] $> threadStorageId
      Nothing -> do
        insertedId <- insertWithPK threadRows [threadStorageRow Nothing]
        let threadStorageId = fromIntegral (fromId insertedId)
        update_ threadRows
          (\row -> row ! #id .== literal insertedId)
          (\row -> row `with` [#thread_id := literal (Just (fromIntegral threadStorageId :: Int.Int64))])
        pure threadStorageId
  where
    threadStorageRow threadStorageId = ThreadStorageRow
      { id = def
      , platform_key = chatPlatformKey messageKey.platform
      , chat_id = fromIntegral <$> messageKey.chatId
      , sender_id = messageKey.senderId
      , message_id = messageIdText messageKey.messageId
      , thread_id = fromIntegral <$> threadStorageId
      , parent_chat_id = fromIntegral <$> (parentMessageKey >>= (.chatId))
      , parent_message_id = messageIdText <$> (parentMessageKey <&> (.messageId))
      , messages_json = storedMessagesJson
      }

threadRowFromStorage :: ThreadStorageRow -> ThreadRow
threadRowFromStorage row =
  let messageKey = ThreadMessageKey{platform = platformFromKey row.platform_key, chatId = fromIntegral <$> row.chat_id, senderId = row.sender_id, messageId = textMessageId row.message_id}
  in ThreadRow
    { messageKey = messageKey
    , threadStorageId = fromIntegral <$> row.thread_id
    , parentMessageKey = do
        parentMessageId <- textMessageId <$> row.parent_message_id
        pure ThreadMessageKey
          { platform = messageKey.platform
          , chatId = fromIntegral <$> row.parent_chat_id
          , senderId = messageKey.senderId
          , messageId = parentMessageId
          }
    , messagesJson = row.messages_json
    }

threadKeyMatches :: forall (backend :: Type). ThreadMessageKey -> Row backend ThreadStorageRow -> Col backend Bool
threadKeyMatches key row =
  row ! #platform_key .== literal (chatPlatformKey key.platform)
    .&& nullableIntegerMatches key.chatId (row ! #chat_id)
    .&& nullableTextMatches key.senderId (row ! #sender_id)
    .&& row ! #message_id .== literal (messageIdText key.messageId)

nullableIntegerMatches :: forall (backend :: Type). Maybe Integer -> Col backend (Maybe Int.Int64) -> Col backend Bool
nullableIntegerMatches Nothing column =
  isNull column
nullableIntegerMatches (Just value) column =
  column .== literal (Just (fromIntegral value :: Int.Int64))

nullableTextMatches :: forall (backend :: Type). Maybe Text -> Col backend (Maybe Text) -> Col backend Bool
nullableTextMatches Nothing column =
  isNull column
nullableTextMatches (Just value) column =
  column .== literal (Just value)

platformFromKey :: Text -> ChatPlatform
platformFromKey = \case
  "telegram" ->
    PlatformTelegram
  "matrix" ->
    PlatformMatrix
  "discord" ->
    PlatformDiscord
  _ ->
    PlatformQQ

cacheThreadNode :: ThreadMessageKey -> StoredThreadNode -> ThreadState -> ThreadState
cacheThreadNode messageKey node threadState =
  threadState
    { threadTree = ThreadTree (Map.restrictKeys insertedTree retainedIds)
    , threadIds = Map.restrictKeys insertedIds retainedIds
    , recentThreadIds = retainedOrder
    }
  where
    insertedTree = (insertThreadNode node.treeNode threadState.threadTree).nodes
    insertedIds = Map.insert messageKey node.threadStorageId threadState.threadIds
    nextOrder = messageKey : filter (/= messageKey) threadState.recentThreadIds
    retainedOrder = take maxCachedThreads nextOrder
    retainedIds = Map.keysSet (Map.fromList [(key, ()) | key <- retainedOrder])

maxCachedThreads :: Int
maxCachedThreads =
  4

messagesJson :: [LLM.ChatMessage] -> Text
messagesJson =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

transcriptMessagesForStorage :: Maybe ThreadMessageKey -> Maybe StoredThreadNode -> Transcript -> (Maybe ThreadMessageKey, [LLM.ChatMessage])
transcriptMessagesForStorage parentMessageKey parentNode transcript =
  case parentNode of
    Just parent
      | Just suffix <- transcriptSuffix parent.treeNode.transcript transcript ->
          (parentMessageKey, suffix)
      | otherwise ->
          (Nothing, transcriptMessagesList transcript)
    Nothing ->
      (parentMessageKey, transcriptMessagesList transcript)

transcriptSuffix :: Transcript -> Transcript -> Maybe [LLM.ChatMessage]
transcriptSuffix parent child
  | parentJson == childPrefixJson =
      Just (drop parentLength childMessages)
  | otherwise =
      Nothing
  where
    parentMessages = transcriptMessagesList parent
    childMessages = transcriptMessagesList child
    parentLength = length parentMessages
    parentJson = map messageJson parentMessages
    childPrefixJson = map messageJson (take parentLength childMessages)

transcriptMessagesList :: Transcript -> [LLM.ChatMessage]
transcriptMessagesList =
  Foldable.toList . (.messages)

messageJson :: LLM.ChatMessage -> LazyByteString.ByteString
messageJson =
  Aeson.encode
