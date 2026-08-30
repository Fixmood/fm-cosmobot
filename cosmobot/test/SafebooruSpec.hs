module Main (main) where

import Bot.Core.Message
import Bot.Core.Route
import Bot.Chat.Driver.Types (ChatDriverEffects)
import qualified Bot.Concurrency.Manager as ConcurrencyManager
import qualified Bot.Chat.Driver.Types as Driver
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Concurrency as ConcurrencyEffect
import qualified Bot.Effect.Storage as StorageEffect
import qualified Bot.Storage.SQLite as StorageSQLite
import Bot.Handler.Safebooru
import Bot.Prelude
import qualified Control.Concurrent as Concurrent
import qualified Data.Aeson as Aeson
import qualified Data.IORef as IORef
import qualified Data.Set as Set
import Test.Tasty
import Test.Tasty.HUnit

newtype ReplyChatDriver es =
  ReplyChatDriver (IncomingMessage -> Text -> Eff es (Either Text MessageId))

instance Driver.ChatDriver (ReplyChatDriver es0) where
  type ChatDriverEffects (ReplyChatDriver es0) es = es ~ es0
  driverPlatform _ = PlatformTelegram
  sendReplyMessage (ReplyChatDriver sendReply) =
    sendReply

main :: IO ()
main =
  defaultMain $
    testGroup "safebooru"
      [ testCase "ball command parser accepts keyword and optional count" testParseBallRequest
      , testCase "ball command parser reports invalid invocations" testParseBallRequestErrors
      , testCase "handler stores fetched links and drains random selections" testStoredLinksDrain
      , testCase "handler is public and ignores longer command prefixes" testPublicRouteAndCommandBoundary
      ]

testParseBallRequest :: IO ()
testParseBallRequest = do
  parseBallRequest "cat" @?= Right BallRequest{keyword = "cat", imageCount = 1}
  parseBallRequest "Cat 5" @?= Right BallRequest{keyword = "cat", imageCount = 5}

testParseBallRequestErrors :: IO ()
testParseBallRequestErrors = do
  parseBallRequest "" @?= Left "用法：!ball <keyword> [num]，num 为 1 到 5。"
  parseBallRequest "cat 0" @?= Left "num 必须是 1 到 5。"
  parseBallRequest "cat 6" @?= Left "num 必须是 1 到 5。"
  parseBallRequest "cat nope" @?= Left "num 必须是 1 到 5。"
  parseBallRequest "cat 2 extra" @?= Left "用法：!ball <keyword> [num]，num 为 1 到 5。"

testStoredLinksDrain :: IO ()
testStoredLinksDrain = do
  replies <- IORef.newIORef ([] :: [Text])
  fetches <- IORef.newIORef ([] :: [Text])
  let imageLinks = [ [i|https://example.test/#{n}.jpg|] | n <- [1 :: Int .. 20] ]
      search keyword = do
        liftIO $ IORef.modifyIORef' fetches (<> [keyword])
        pure imageLinks

  runSafebooruFlow replies do
    runHandlers (safebooruHandlersWith search) (message "!ball Cat 2")
    liftIO $ waitUntil "first reply" ((>= 1) . length <$> IORef.readIORef replies)
    runHandlers (safebooruHandlersWith search) (message "!ball cat 2")
    liftIO $ waitUntil "second reply" ((>= 2) . length <$> IORef.readIORef replies)

  IORef.readIORef fetches >>= (@?= ["cat"])
  bodies <- IORef.readIORef replies
  let sentLinks = concatMap Chat.replyImageUrls bodies
  length sentLinks @?= 4
  Set.size (Set.fromList sentLinks) @?= 4
  assertBool "all sent links came from the fetch result" (all (`elem` imageLinks) sentLinks)

testPublicRouteAndCommandBoundary :: IO ()
testPublicRouteAndCommandBoundary = do
  replies <- IORef.newIORef ([] :: [Text])
  let search _ =
        pure ["https://example.test/public.jpg"]

  runSafebooruFlow replies do
    runHandlers (safebooruHandlersWith search) (groupMessage "!ball cat")
    liftIO $ waitUntil "public group reply" ((>= 1) . length <$> IORef.readIORef replies)
    runHandlers (safebooruHandlersWith search) (groupMessage "!balloon cat")

  bodies <- IORef.readIORef replies
  case bodies of
    [body] ->
      Chat.replyImageUrls body @?= ["https://example.test/public.jpg"]
    _ ->
      assertFailure [i|expected one public route reply, got #{length bodies}|]

runSafebooruFlow
  :: IORef.IORef [Text]
  -> Eff '[StorageEffect.Storage, Chat.Chat, KatipE, ConcurrencyEffect.Concurrency, Concurrent, Prim, IOE] ()
  -> IO ()
runSafebooruFlow replies action =
  runEff $
    runPrim $
    runConcurrent $
    ConcurrencyManager.runConcurrencyManager $
    runTestLog $
      Chat.runChatWith (testChatDriver replies) $
        StorageSQLite.runStorageSQLitePath ":memory:" $
          action

testChatDriver :: IOE :> es => IORef.IORef [Text] -> ReplyChatDriver es
testChatDriver replies =
  ReplyChatDriver \_ body -> do
    liftIO $ IORef.modifyIORef' replies (<> [body])
    pure (Right "1")

runTestLog :: IOE :> es => Eff (KatipE : es) a -> Eff es a
runTestLog action = startKatipE "safebooru-spec" "test" action

waitUntil :: String -> IO Bool -> IO ()
waitUntil label predicate =
  go (50 :: Int)
  where
    go 0 =
      assertFailure [i|timed out waiting for #{label}|]
    go attempts = do
      done <- predicate
      unless done do
        Concurrent.threadDelay 10000
        go (attempts - 1)

message :: Text -> IncomingMessage
message body =
  IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just 100
    , chatAliases = []
    , digest = emptyMessageDigest
    , senderId = Just "200"
    , senderUsername = Just "alice"
    , messageId = Just "300"
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , files = []
    , text = body
    , raw = Aeson.Null
    }

groupMessage :: Text -> IncomingMessage
groupMessage body =
  (message body)
    { kind = ChatGroup
    , digest = emptyMessageDigest
    }
