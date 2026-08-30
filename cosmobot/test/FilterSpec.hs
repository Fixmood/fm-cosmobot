module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.IORef as IORef
import Bot.Core.Route
import Bot.Core.Message
import Bot.Handler.Help (renderHelp)
import Bot.Prelude
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "filter"
      [ testCase "command strips prefix and rejects other commands" testCommandFilter
      , testCase "prefixedText keeps the matched prefix in prompt" testPrefixedTextFilter
      , testCase "promptOrImages accepts images without text" testPromptOrImages
      , testCase "fromGroups matches only allowed group chats" testFromGroups
      , testCase "notReply rejects replies" testNotReply
      , testCase "stopping matched route prevents later handlers" testStoppingMatchedRoutePreventsLaterHandlers
      , testCase "route continues to later handlers" testRouteContinueRunsLaterHandlers
      , testCase "restricted route stops after access failure" testRestrictedRouteStopsAfterAccessFailure
      , testCase "route help is collected in parser order" testCollectRouteHelp
      , testCase "help renders collected route metadata" testRenderHelp
      ]

testCommandFilter :: IO ()
testCommandFilter = do
  let matched = applyFilter (command "!ask") (message "!ask hello")
      unmatched = applyFilter (command "!ask") (message "!draw hello")
  matched @?= Just "hello"
  unmatched @?= Nothing

testPrefixedTextFilter :: IO ()
testPrefixedTextFilter = do
  applyFilter (prefixedText "krkr") (message "krkr这是什么") @?= Just "krkr这是什么"
  applyFilter (prefixedText "krkr") (message "krkr 这是什么") @?= Just "krkr 这是什么"
  applyFilter (prefixedText "FM") (message "fm 取消静默，上线！") @?= Just "fm 取消静默，上线！"
  applyFilter (prefixedText "fm") (message "  Fm，能听见吗  ") @?= Just "Fm，能听见吗"
  applyFilter (prefixedText "krkr") (message "hello krkr") @?= Nothing

testPromptOrImages :: IO ()
testPromptOrImages = do
  applyFilter promptOrImages (message "") @?= Nothing
  applyFilter promptOrImages (message "  hello  ") @?= Just "hello"
  applyFilter promptOrImages (messageWithImages "" ["https://example.test/image.png"]) @?= Just ""

testFromGroups :: IO ()
testFromGroups = do
  assertBool "allowed group matches" (isJust (applyFilter (fromGroups [100]) (message "hello"){kind = ChatGroup, chatId = Just 100}))
  assertBool "other group does not match" (isNothing (applyFilter (fromGroups [100]) (message "hello"){kind = ChatGroup, chatId = Just 101}))
  assertBool "private chat does not match" (isNothing (applyFilter (fromGroups [100]) (message "hello"){kind = ChatPrivate, chatId = Just 100}))

testNotReply :: IO ()
testNotReply = do
  assertBool "non-reply matches" (isJust (applyFilter notReply (message "hello")))
  assertBool "reply does not match" (isNothing (applyFilter notReply (message "hello"){replyToMessageId = Just "1"}))

testStoppingMatchedRoutePreventsLaterHandlers :: IO ()
testStoppingMatchedRoutePreventsLaterHandlers = do
  calls <- IORef.newIORef ([] :: [Text])
  runEff $ runHandlers
    [ appendStopCall calls "first"
    , appendStopCall calls "second"
    ]
    (message "hello")
  IORef.readIORef calls >>= (@?= ["first"])

testRouteContinueRunsLaterHandlers :: IO ()
testRouteContinueRunsLaterHandlers = do
  calls <- IORef.newIORef ([] :: [Text])
  runEff $ runHandlers
    [ appendContinueCall calls "first"
    , appendStopCall calls "second"
    ]
    (message "hello")
  IORef.readIORef calls >>= (@?= ["first", "second"])

testRestrictedRouteStopsAfterAccessFailure :: IO ()
testRestrictedRouteStopsAfterAccessFailure = do
  calls <- IORef.newIORef ([] :: [Text])
  runEff $ runHandlers
    [ requireAuth
        (const False)
        (\_ -> liftIO $ IORef.modifyIORef' calls (<> ["denied"]))
        (stopOn (command "!exec") \_ _ ->
          liftIO $ IORef.modifyIORef' calls (<> ["exec"]))
    , appendStopCall calls "fallback"
    ]
    (message "!exec whoami")
  IORef.readIORef calls >>= (@?= ["denied"])

testCollectRouteHelp :: IO ()
testCollectRouteHelp = do
  collectRouteHelp normalMessage routes @?= [firstHelp]
  collectRouteHelp superuserMessage routes @?= [firstHelp, secondHelp]
  where
    normalMessage = message "!help"
    superuserMessage = normalMessage{digest = emptyMessageDigest{senderIsSuperuser = True}}
    routes =
      [ withHelp firstHelp (stopOn anything \_ _ -> pure ())
      , stopOn anything \_ _ -> pure ()
      , withHelp secondHelp $
          requireAuth isSuperuser (\_ -> pure ()) (stopOn anything \_ _ -> pure ())
      ]
    firstHelp = RouteHelp "!first" "First command."
    secondHelp = RouteHelp "!second <arg>" "Second command."

testRenderHelp :: IO ()
testRenderHelp =
  renderHelp [RouteHelp "!ping" "Check the bot."]
    @?= "Available commands:\n- `!ping` — Check the bot.\n"

appendContinueCall :: IORef.IORef [Text] -> Text -> RouteHandler '[IOE]
appendContinueCall calls label =
  appendCall continueOn calls label

appendStopCall :: IORef.IORef [Text] -> Text -> RouteHandler '[IOE]
appendStopCall calls label =
  appendCall stopOn calls label

appendCall
  :: (MessageFilter IncomingMessage -> (IncomingMessage -> IncomingMessage -> Eff '[IOE] ()) -> RouteHandler '[IOE])
  -> IORef.IORef [Text]
  -> Text
  -> RouteHandler '[IOE]
appendCall mkRoute calls label =
  mkRoute anything \_ _ ->
    liftIO $ IORef.modifyIORef' calls (<> [label])

applyFilter :: MessageFilter a -> IncomingMessage -> Maybe a
applyFilter (MessageFilter filt) =
  filt

message :: Text -> IncomingMessage
message =
  messageFrom "200"

messageFrom :: Text -> Text -> IncomingMessage
messageFrom senderId text =
  messageFromWithImages senderId text []

messageWithImages :: Text -> [Text] -> IncomingMessage
messageWithImages =
  messageFromWithImages "200"

messageFromWithImages :: Text -> Text -> [Text] -> IncomingMessage
messageFromWithImages senderId text imageUrls =
  IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = PlatformTelegram
    , kind = ChatPrivate
    , chatId = Just 100
    , chatAliases = []
    , digest = emptyMessageDigest
    , senderId = Just senderId
    , senderUsername = Just "alice"
    , messageId = Just "300"
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = imageUrls
    , files = []
    , text = text
    , raw = Aeson.Null
    }
