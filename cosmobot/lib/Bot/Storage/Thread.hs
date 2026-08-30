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
  }

data StoredThreadNode = StoredThreadNode
  { threadStorageId :: !Integer
  , treeNode :: !ThreadNode
  }

data ActiveThread = ActiveThread
  { activeChatScope :: !(Maybe ActiveChatScope)
  , activeSenderId :: !(Maybe Text)
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
  , message_id :: Text
  , thread_id :: Maybe Int.Int64
  , parent_chat_id :: Maybe Int.Int64
  , parent_message_id :: Maybe Text
  , messages_json :: Text
  }
  deriving (Generic)

instance SqlRow ThreadStorageRow

threadRows :: Table ThreadStorageRow
threadRows =
  table "threads"
    [ #id :- autoPrimary
    , #platform_key :- index
    , #chat_id :- index
    , #message_id :- index
    , #thread_id :- index
    , #parent_message_id :- index
    ]

newThreadStore :: Prim :> es => Eff es ThreadStore
newThreadStore = do
  ref <- newIORef ThreadState{threadTree = emptyThreadTree, threadIds = Map.empty, recentThreadIds = []}
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
  :: (Prim :> es, KatipE :> es, Concurrent :> es, Storage.Storage :> es)
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
  for_ replyMessageKeys \keys -> do
    updateActiveThread (ActiveThreadHandle active) transcript
    traverse_ (\messageKey -> rememberThreadTranscriptFrom store active.activeParentMessageKey (Just messageKey) transcript) keys
    void $ MVar.tryPutMVar active.activeDone transcript
    atomicModifyIORef' activeRef \activeMap ->
      (Map.filter ((/= active.activeHandle.handleId) . (.activeHandle.handleId)) activeMap, ())

finishActiveThreadCurrent
  :: (Prim :> es, KatipE :> es, Storage.Storage :> es, Concurrent :> es)
  => ThreadStore
  -> ActiveThreadHandle
  -> Eff es ()
finishActiveThreadCurrent store (ActiveThreadHandle active) = do
  transcript <- readIORef active.activeCurrent
  finishActiveThread store (ActiveThreadHandle active) transcript

haltThread
  :: (Prim :> es, KatipE :> es, Storage.Storage :> es, Concurrent :> es)
  => ThreadStore
  -> (Id -> Eff es Bool)
  -> ThreadMessageKey
  -> Eff es Bool
haltThread store@ThreadStore{activeThreadStore = activeRef} cancel messageKey = do
  active <- Map.lookup (ActiveThreadMessage messageKey) <$> readIORef activeRef
  maybe (pure False) (haltActiveThread store cancel) active

haltActiveThread
  :: (Prim :> es, KatipE :> es, Storage.Storage :> es, Concurrent :> es)
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
  :: (Prim :> es, KatipE :> es, Storage.Storage :> es, Concurrent :> es)
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
  :: (Prim :> es, KatipE :> es, Storage.Storage :> es, Concurrent :> es)
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
  :: (Prim :> es, KatipE :> es, Storage.Storage :> es, Concurrent :> es)
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
  for_ persistedThreadStorageId \threadStorageId ->
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
  runSelda (tryCreateTable threadRows)

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
      , message_id = messageIdText messageKey.messageId
      , thread_id = fromIntegral <$> threadStorageId
      , parent_chat_id = fromIntegral <$> (parentMessageKey >>= (.chatId))
      , parent_message_id = messageIdText <$> (parentMessageKey <&> (.messageId))
      , messages_json = storedMessagesJson
      }

threadRowFromStorage :: ThreadStorageRow -> ThreadRow
threadRowFromStorage row =
  let messageKey = ThreadMessageKey{platform = platformFromKey row.platform_key, chatId = fromIntegral <$> row.chat_id, messageId = textMessageId row.message_id}
  in ThreadRow
    { messageKey = messageKey
    , threadStorageId = fromIntegral <$> row.thread_id
    , parentMessageKey = do
        parentMessageId <- textMessageId <$> row.parent_message_id
        pure ThreadMessageKey
          { platform = messageKey.platform
          , chatId = fromIntegral <$> row.parent_chat_id
          , messageId = parentMessageId
          }
    , messagesJson = row.messages_json
    }

threadKeyMatches :: forall (backend :: Type). ThreadMessageKey -> Row backend ThreadStorageRow -> Col backend Bool
threadKeyMatches key row =
  row ! #platform_key .== literal (chatPlatformKey key.platform)
    .&& nullableIntegerMatches key.chatId (row ! #chat_id)
    .&& row ! #message_id .== literal (messageIdText key.messageId)

nullableIntegerMatches :: forall (backend :: Type). Maybe Integer -> Col backend (Maybe Int.Int64) -> Col backend Bool
nullableIntegerMatches Nothing column =
  isNull column
nullableIntegerMatches (Just value) column =
  column .== literal (Just (fromIntegral value :: Int.Int64))

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
