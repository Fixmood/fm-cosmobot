module Main (main) where

import qualified Bot.Concurrency.Manager as ConcurrencyManager
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Storage.SQLite as StorageSQLite
import Bot.Core.Message
import Bot.Core.Route
import Bot.Prelude
import qualified Bot.Util.Stream as StreamUtil
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Text as Text
import Effectful.Timeout (runTimeout)
import qualified Streaming.Prelude as S
import Test.Tasty.Bench

newtype BenchMessages = BenchMessages [IncomingMessage]

instance NFData BenchMessages where
  rnf (BenchMessages messages) =
    foldr (\message rest -> forceMessage message `seq` rest) () messages

forceMessage :: IncomingMessage -> ()
forceMessage IncomingMessage{eventKind, platform, kind, chatId, chatAliases, digest, senderId, senderUsername, messageId, replyToMessageId, mentions, mentionUsernames, imageUrls, files, text, raw} =
  forceEventKind eventKind
    `seq` forcePlatform platform
    `seq` forceKind kind
    `seq` rnf chatId
    `seq` rnf chatAliases
    `seq` forceDigest digest
    `seq` rnf senderId
    `seq` rnf senderUsername
    `seq` forceMaybeMessageId messageId
    `seq` forceMaybeMessageId replyToMessageId
    `seq` rnf mentions
    `seq` rnf mentionUsernames
    `seq` rnf imageUrls
    `seq` forceFiles files
    `seq` rnf text
    `seq` rnf raw

forcePlatform :: ChatPlatform -> ()
forcePlatform = \case
  PlatformQQ -> ()
  PlatformTelegram -> ()
  PlatformMatrix -> ()
  PlatformDiscord -> ()
  PlatformRPC -> ()
  PlatformACP -> ()

forceEventKind :: IncomingMessageEventKind -> ()
forceEventKind = \case
  IncomingMessageCreated -> ()
  IncomingMessageDeleted -> ()

forceKind :: ChatKind -> ()
forceKind = \case
  ChatPrivate -> ()
  ChatGroup -> ()
  ChatChannel -> ()
  ChatUnknown name -> rnf name

forceDigest :: MessageDigest -> ()
forceDigest MessageDigest{chatIsAllowed, senderIsAllowed, senderIsSuperuser, mentionsBot, botId} =
  rnf chatIsAllowed
    `seq` rnf senderIsAllowed
    `seq` rnf senderIsSuperuser
    `seq` rnf mentionsBot
    `seq` rnf botId

forceMaybeMessageId :: Maybe MessageId -> ()
forceMaybeMessageId =
  maybe () (rnf . messageIdText)

forceFiles :: [MessageFile] -> ()
forceFiles =
  foldr (\MessageFile{name, ref} rest -> rnf name `seq` rnf ref `seq` rest) ()

main :: IO ()
main =
  defaultMain
    [ microGroup 10000
    , pipelineGroup 1000
    , pipelineGroup 10000
    , pipelineGroup 50000
    ]

microGroup :: Int -> Benchmark
microGroup count =
  env (pure (BenchMessages (syntheticMessages count))) \(BenchMessages messages) ->
    bgroup (show count <> " message microbenchmarks")
      [ bench "route-filters" $
          nf routeFilterScore messages
      , bench "incoming-message-log-line" $
          nf (map incomingMessageLogLine) messages
      , bench "incoming-message-json-encode" $
          nf encodeMessages messages
      , bench "incoming-message-json-decode-value" $
          nf decodeJsonValues (encodeMessages messages)
      , bench "chatlog-record" $
          nfIO (chatLogRecord messages)
      , bench "chatlog-record-query" $
          nfIO (chatLogRecordQuery messages)
      , bench "stream-merge" $
          nfIO (mergeOnly messages)
      , bench "scheduler-due-messages" $
          nfIO (schedulerDueMessages messages)
      ]

pipelineGroup :: Int -> Benchmark
pipelineGroup count =
  env (pure (BenchMessages (syntheticMessages count))) \(BenchMessages messages) ->
    bgroup (show count <> " messages")
      [ bench "route-dispatch" $
          nfIO (routeDispatch messages)
      , bench "chatlog-route-dispatch" $
          nfIO (chatLogRouteDispatch messages)
      , bench "merged-chatlog-route-dispatch" $
          nfIO (mergedChatLogRouteDispatch messages)
      ]

routeDispatch :: [IncomingMessage] -> IO Int
routeDispatch messages = do
  handled <- IORef.newIORef 0
  runEff do
    consumeWith (benchmarkHandlers handled) (S.each messages)
    liftIO (IORef.readIORef handled)

chatLogRouteDispatch :: [IncomingMessage] -> IO Int
chatLogRouteDispatch messages = do
  handled <- IORef.newIORef 0
  runEff $
    runConcurrent $
      runTimeout $
      runBenchmarkLog $
        StorageSQLite.runStorageSQLitePath ":memory:" $
        ChatLog.runChatLog do
          consumeWith
            (benchmarkHandlers handled)
            (ChatLog.recordIncomingMessages (S.each messages))
          liftIO (IORef.readIORef handled)

mergedChatLogRouteDispatch :: [IncomingMessage] -> IO Int
mergedChatLogRouteDispatch messages = do
  handled <- IORef.newIORef 0
  runEff $
    runConcurrent $
      runTimeout $
      runPrim $
        ConcurrencyManager.runConcurrencyManager $
          runBenchmarkLog $
            StorageSQLite.runStorageSQLitePath ":memory:" $
            ChatLog.runChatLog do
              consumeWith
                (benchmarkHandlers handled)
                (ChatLog.recordIncomingMessages (StreamUtil.mergeStreams (map S.each (chunks 4 messages))))
              liftIO (IORef.readIORef handled)

routeFilterScore :: [IncomingMessage] -> Int
routeFilterScore =
  foldl' scoreMessage 0
  where
    scoreMessage score message =
      score
        + matched (command "!ask") message
        + matched (fromGroups [100, 101, 102, 103]) message
        + matched promptOrImages message
        + matched notReply message
    matched (MessageFilter filt) message =
      maybe 0 (const 1) (filt message)

encodeMessages :: [IncomingMessage] -> [LazyByteString.ByteString]
encodeMessages =
  map Aeson.encode

decodeJsonValues :: [LazyByteString.ByteString] -> Int
decodeJsonValues =
  foldl' countDecoded 0
  where
    countDecoded count encoded =
      case Aeson.eitherDecode encoded :: Either String Aeson.Value of
        Left _ ->
          count
        Right value ->
          value `seq` count + 1

chatLogRecord :: [IncomingMessage] -> IO Int
chatLogRecord messages = do
  runEff $
    runConcurrent $
      runBenchmarkLog $
        StorageSQLite.runStorageSQLitePath ":memory:" $
        ChatLog.runChatLog do
          traverse_ ChatLog.recordMessage messages
          pure (length messages)

chatLogRecordQuery :: [IncomingMessage] -> IO Int
chatLogRecordQuery messages = do
  runEff $
    runConcurrent $
      runBenchmarkLog $
        StorageSQLite.runStorageSQLitePath ":memory:" $
        ChatLog.runChatLog do
          traverse_ ChatLog.recordMessage messages
          entries <- ChatLog.queryChat (lastMessage messages) Nothing 100 True ChatLog.unboundedChatLogTimeRange
          pure (length entries)

mergeOnly :: [IncomingMessage] -> IO Int
mergeOnly messages = do
  seen <- IORef.newIORef 0
  runEff $
    runConcurrent $
      runPrim $
        ConcurrencyManager.runConcurrencyManager $
          runBenchmarkLog $
            S.mapM_
              (\_ -> liftIO $ IORef.modifyIORef' seen (+ 1))
              (StreamUtil.mergeStreams (map S.each (chunks 4 messages)))
  IORef.readIORef seen

schedulerDueMessages :: [IncomingMessage] -> IO Int
schedulerDueMessages messages =
  runEff $
    runTimeout $
      runConcurrent $
        runPrim $
          ConcurrencyManager.runConcurrencyManager $
            StorageSQLite.runStorageSQLitePath ":memory:" $
              Scheduler.runScheduler do
                traverse_ (Scheduler.scheduleMessage 0) messages
                ref <- liftIO (IORef.newIORef 0)
                S.mapM_
                  (\_ -> liftIO $ IORef.modifyIORef' ref (+ 1))
                  (S.take (length messages) Scheduler.scheduledMessages)
                liftIO (IORef.readIORef ref)

lastMessage :: [IncomingMessage] -> IncomingMessage
lastMessage [] =
  syntheticMessage 1
lastMessage messages =
  fromMaybe (syntheticMessage 1) (viaNonEmpty last messages)

benchmarkHandlers :: IOE :> es => IORef.IORef Int -> [RouteHandler es]
benchmarkHandlers handled =
  [ continueOn (command "!never") \_ _ ->
      liftIO $ IORef.modifyIORef' handled (+ 1000000)
  , continueOn (fromGroups [-1]) \_ _ ->
      liftIO $ IORef.modifyIORef' handled (+ 1000000)
  , continueOn (matching (const False)) \_ _ ->
      liftIO $ IORef.modifyIORef' handled (+ 1000000)
  , stopOn anything \_ _ ->
      liftIO $ IORef.modifyIORef' handled (+ 1)
  ]

runBenchmarkLog :: IOE :> es => Eff (KatipE : es) a -> Eff es a
runBenchmarkLog action =
  startKatipE "message-pipeline-bench" "bench" action

syntheticMessages :: Int -> [IncomingMessage]
syntheticMessages count =
  map syntheticMessage [1 .. count]

syntheticMessage :: Int -> IncomingMessage
syntheticMessage index =
  IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = platform
    , kind = kind
    , chatId = Just (fromIntegral (100 + index `mod` 32))
    , chatAliases = []
    , digest = emptyMessageDigest
        { chatIsAllowed = kind == ChatGroup
        , senderIsAllowed = kind == ChatPrivate
        , senderIsSuperuser = index `mod` 97 == 0
        , mentionsBot = index `mod` 11 == 0
        }
    , senderId = Just (show (1000 + index `mod` 256))
    , senderUsername = Just ("user" <> show (index `mod` 256))
    , messageId = Just (integerMessageId (fromIntegral index))
    , replyToMessageId = if index `mod` 13 == 0 then Just (integerMessageId (fromIntegral (max 1 (index - 1)))) else Nothing
    , mentions = if index `mod` 17 == 0 then ["42"] else []
    , mentionUsernames = if index `mod` 19 == 0 then ["cosmobot"] else []
    , imageUrls = if index `mod` 23 == 0 then ["https://example.test/image.png"] else []
    , files = []
    , text = "message " <> show index <> " " <> Text.replicate (index `mod` 8) "payload "
    , raw = Aeson.Null
    }
  where
    platform =
      case index `mod` 3 of
        0 -> PlatformQQ
        1 -> PlatformTelegram
        _ -> PlatformMatrix
    kind =
      if even index then ChatGroup else ChatPrivate

chunks :: Int -> [a] -> [[a]]
chunks chunkCount values =
  [ [value | (offset, value) <- indexed, offset `mod` chunkCount == bucket]
  | bucket <- [0 .. chunkCount - 1]
  ]
  where
    indexed = List.zip [0 :: Int ..] values
