module Main (main) where

import Bot.Chat.Driver.Types (ChatDriverEffects)
import qualified Bot.Chat.Driver.Types as Driver
import Bot.Core.Message
import Bot.Core.Route
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Media as Media
import Bot.Handler.Media
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

data ReplyChatDriver es = ReplyChatDriver
  { sendReply :: Text -> Eff es ()
  , referenced :: Maybe ReferencedMessage
  }

instance Driver.ChatDriver (ReplyChatDriver es0) where
  type ChatDriverEffects (ReplyChatDriver es0) es = es ~ es0
  driverPlatform _ = PlatformTelegram
  sendReplyMessage driver _ body =
    driver.sendReply body $> Right "reply"
  getMessageContent driver _ _ =
    pure driver.referenced
  normalizeMediaRef _ "mxc://pfeiwu.com/EWNSIWtK9UyoJ4mDwi3sqn3sj8UFi70v" =
    pure "media:mf_known"
  normalizeMediaRef _ ref =
    pure ref

main :: IO ()
main =
  defaultMain $
    testGroup "media handler"
      [ testCase "info and get report each requested media id" testMediaCommands
      , testCase "info omits local media paths" testMediaInfoOmitsPath
      , testCase "info extracts media ids from the replied message" testRepliedMediaInfo
      , testCase "url extracts media ids from the replied message" testRepliedMediaUrl
      , testCase "cache reports downloaded media ids and failures" testMediaCache
      , testCase "get rejects non-superusers" testMediaGetRejectsNonSuperuser
      , testCase "commands require at least one media id" testMediaCommandUsage
      ]

testMediaCommands :: IO ()
testMediaCommands = do
  replies <- IORef.newIORef []
  runMediaHandlers replies do
    runHandlers mediaHandlers (message "!media/info mf_known media:mf_missing")
    runHandlers mediaHandlers (message "!media/url media:mf_known mf_missing")
  IORef.readIORef replies >>= \case
    [infoReply, getReply] -> do
      assertBool "info JSON is pretty-printed" ("\n" `Text.isInfixOf` infoReply)
      assertBool "info JSON is in a source block" ("```json\n" `Text.isPrefixOf` infoReply && "\n```" `Text.isSuffixOf` infoReply)
      assertBool "info includes cached file metadata" ("\"fileId\": \"mf_known\"" `Text.isInfixOf` infoReply)
      assertBool "info reports missing ids" (all (`Text.isInfixOf` infoReply) ["\"media_id\": \"media:mf_missing\"", "\"error\": \"not found\""])
      getReply @?=
        "- media:mf_known: https://media.example/mf_known\n- media:mf_missing: not found"
    actual ->
      assertFailure [i|expected two replies, got #{length actual}|]

testMediaInfoOmitsPath :: IO ()
testMediaInfoOmitsPath = do
  replies <- IORef.newIORef []
  runMediaHandlers replies $
    runHandlers mediaHandlers (message "!media/info mf_known")
  replyBody <- viaNonEmpty head <$> IORef.readIORef replies
  assertBool "info must not expose the local cache path" $
    maybe False (not . Text.isInfixOf "\"path\"") replyBody

testMediaCommandUsage :: IO ()
testMediaCommandUsage = do
  replies <- IORef.newIORef []
  runMediaHandlers replies do
    runHandlers mediaHandlers (message "!media/info")
    runHandlers mediaHandlers (message "!media/url")
    runHandlers mediaHandlers (superuserMessage "!media/get")
  IORef.readIORef replies >>= (@?=
    [ "Usage: !media/info <media_id>..., or reply to a message containing media."
    , "Usage: !media/url <media_id>..., or reply to a message containing media."
    , "Usage: !media/get <url>..."
    ])

testMediaCache :: IO ()
testMediaCache = do
  replies <- IORef.newIORef []
  runMediaHandlers replies do
    runHandlers mediaHandlers (superuserMessage "!media/get https://media.example/ok mxc://pfeiwu.com/EWNSIWtK9UyoJ4mDwi3sqn3sj8UFi70v https://media.example/fail not-a-url")
  replyBody <- viaNonEmpty head <$> IORef.readIORef replies
  case replyBody of
    Nothing ->
      assertFailure "expected a reply"
    Just body -> do
      assertBool "cache JSON is pretty-printed" ("\n" `Text.isInfixOf` body)
      assertBool "cache JSON is in a source block" ("```json\n" `Text.isPrefixOf` body && "\n```" `Text.isSuffixOf` body)
      assertBool "successful downloads return media ids" (all (`Text.isInfixOf` body) ["https://media.example/ok", "\"media_id\": \"media:mf_known\""])
      assertBool "successful downloads return public URLs" ("\"public_url\": \"https://media.example/mf_known\"" `Text.isInfixOf` body)
      assertBool "MXC URLs are normalized by the chat driver" ("mxc://pfeiwu.com/EWNSIWtK9UyoJ4mDwi3sqn3sj8UFi70v" `Text.isInfixOf` body)
      assertBool "failed downloads return errors" (all (`Text.isInfixOf` body) ["https://media.example/fail", "\"error\": \"download or cache failed\""])
      assertBool "invalid URLs return errors" ("\"error\": \"expected an HTTP, HTTPS, or MXC URL\"" `Text.isInfixOf` body)

testMediaGetRejectsNonSuperuser :: IO ()
testMediaGetRejectsNonSuperuser = do
  replies <- IORef.newIORef []
  runMediaHandlers replies $
    runHandlers mediaHandlers (message "!media/get https://media.example/ok")
  IORef.readIORef replies >>= (@?= ["Only superusers can download media."])

testRepliedMediaInfo :: IO ()
testRepliedMediaInfo = do
  replies <- IORef.newIORef []
  runMediaHandlersWithReply replies (Just referencedMessage) do
    runHandlers mediaHandlers ((message "!media/info"){replyToMessageId = Just "parent"})
  replyBody <- viaNonEmpty head <$> IORef.readIORef replies
  case replyBody of
    Nothing ->
      assertFailure "expected a reply"
    Just body -> do
      assertBool "info includes a media id from image URLs" ("\"media_id\": \"media:mf_known\"" `Text.isInfixOf` body)
      assertBool "duplicate media ids are removed" (Text.count "\"media_id\"" body == 1)

testRepliedMediaUrl :: IO ()
testRepliedMediaUrl = do
  replies <- IORef.newIORef []
  runMediaHandlersWithReply replies (Just referencedMessage) do
    runHandlers mediaHandlers ((message "!media/url"){replyToMessageId = Just "parent"})
  replyBody <- viaNonEmpty head <$> IORef.readIORef replies
  replyBody @?= Just "- media:mf_known: https://media.example/mf_known"

runMediaHandlers
  :: IORef.IORef [Text]
  -> Eff '[Chat.Chat, Media.Media, IOE] ()
  -> IO ()
runMediaHandlers replies action =
  runMediaHandlersWithReply replies Nothing action

runMediaHandlersWithReply
  :: IORef.IORef [Text]
  -> Maybe ReferencedMessage
  -> Eff '[Chat.Chat, Media.Media, IOE] ()
  -> IO ()
runMediaHandlersWithReply replies referenced action =
  runEff $
    runMedia $
    Chat.runChatWith
      ReplyChatDriver
        { sendReply = \body -> liftIO $ IORef.modifyIORef' replies (<> [body])
        , referenced
        }
      action

runMedia :: Eff (Media.Media : es) a -> Eff es a
runMedia =
  interpret \_ -> \case
    Media.GetMediaCacheEntry fileId ->
      pure (knownEntry <$ guard (fileId == "mf_known"))
    Media.PublicMediaRef ref ->
      pure ("https://media.example/" <> fromMaybe ref (Text.stripPrefix "media:" ref))
    Media.NormalizeMediaRef "https://media.example/ok" ->
      pure "media:mf_known"
    Media.NormalizeMediaRef ref ->
      pure ref
    _ ->
      error "unexpected media operation"

knownEntry :: Media.MediaCacheEntry
knownEntry =
  Media.MediaCacheEntry
    { file = Media.MediaFileInfo
        { fileId = "mf_known"
        , ref = "media:mf_known"
        , digest = "sha256"
        , mimeType = "image/png"
        , sourceName = Just "known.png"
        , path = "/cache/mf_known.png"
        , size = 123
        , createdAtUnix = 1
        , lastUsedAtUnix = 2
        , exists = True
        }
    , sourceRefs = ["https://source.example/known.png"]
    , platformRefs = []
    }

referencedMessage :: ReferencedMessage
referencedMessage =
  ReferencedMessage
    { messageId = Just "parent"
    , senderDisplayName = Nothing
    , senderIdentifier = Nothing
    , senderIsBot = True
    , text = "cached as media:mf_known."
    , imageUrls = ["media:mf_known"]
    , files = []
    }

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
    , senderUsername = Nothing
    , messageId = Just "300"
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , files = []
    , text = body
    , raw = Aeson.Null
    }

superuserMessage :: Text -> IncomingMessage
superuserMessage body =
  (message body){digest = emptyMessageDigest{senderIsSuperuser = True}}
