module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.IORef as IORef
import Bot.Core.Route
import Bot.Core.Message
import Bot.Handler.Help (renderHelp)
import qualified Bot.Handler.Humanlike as Humanlike
import qualified Bot.Handler.Humanlike.Archive as Archive
import qualified Bot.Agent.Tools.Sticker as Sticker
import Bot.Prelude
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
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
      , testCase "humanlike switch accepts a bare command in private chat" testHumanlikeBarePrivate
      , testCase "humanlike switch keeps the fm prefix working" testHumanlikePrefixed
      , testCase "humanlike switch needs fm or a mention in groups" testHumanlikeGroupNeedsAddress
      , testCase "humanlike switch ignores ordinary prose" testHumanlikeIgnoresProse
      , testCase "humanlike scope keys private and group separately" testHumanlikeScopeKeys
      , testCase "humanlike spans mark only their own interval" testHumanlikeSpans
      , testCase "humanlike archive keeps newest days and preserves the owner's part" testHumanlikeArchiveMerge
      , testCase "sticker catalog parses and searches by emotion and tag" testStickerCatalog
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

-- ── 拟人模式开关 ────────────────────────────────────────────────────────────
--
-- 这几条钉住的是「哪些话应该被当成开关命令」。措辞一旦改动而没有测试，
-- 命令会静默失效：线上表现是模型顺着用户的话说「已经开好了」，而实际上什么都没变。
-- （2026-11-07 实际发生过一次，当时私聊里说「开启拟人模式」被当成了普通聊天。）

qqMessage :: ChatKind -> Text -> IncomingMessage
qqMessage kind text =
  (messageFrom "2822751355" text)
    { platform = PlatformQQ
    , kind = kind
    , chatId = Just 100
    }

mentionedMessage :: ChatKind -> Text -> IncomingMessage
mentionedMessage kind text =
  let base = qqMessage kind text
  in base{digest = base.digest{mentionsBot = True}}

testHumanlikeBarePrivate :: IO ()
testHumanlikeBarePrivate = do
  -- 私聊里每条消息本来就是对着 FM 说的，不需要 fm 前缀。
  Humanlike.parseHumanlikeCommand (qqMessage ChatPrivate "开启拟人模式") @?= Just Humanlike.HumanlikeEnable
  Humanlike.parseHumanlikeCommand (qqMessage ChatPrivate "关闭拟人模式") @?= Just Humanlike.HumanlikeDisable
  Humanlike.parseHumanlikeCommand (qqMessage ChatPrivate "拟人模式") @?= Just Humanlike.HumanlikeStatus
  Humanlike.parseHumanlikeCommand (qqMessage ChatPrivate "  开启 拟人模式 ") @?= Just Humanlike.HumanlikeEnable

testHumanlikePrefixed :: IO ()
testHumanlikePrefixed = do
  Humanlike.parseHumanlikeCommand (qqMessage ChatPrivate "fm 开启拟人模式") @?= Just Humanlike.HumanlikeEnable
  Humanlike.parseHumanlikeCommand (qqMessage ChatGroup "fm 开启拟人模式") @?= Just Humanlike.HumanlikeEnable
  Humanlike.parseHumanlikeCommand (qqMessage ChatGroup "FM关闭拟人模式") @?= Just Humanlike.HumanlikeDisable

testHumanlikeGroupNeedsAddress :: IO ()
testHumanlikeGroupNeedsAddress = do
  -- 群里没人叫 FM 时，裸命令不能把群聊吞掉并回一句权限提示。
  Humanlike.parseHumanlikeCommand (qqMessage ChatGroup "开启拟人模式") @?= Nothing
  Humanlike.parseHumanlikeCommand (qqMessage ChatGroup "拟人模式") @?= Nothing
  -- 真 @ 了就算已经对话，可以裸写。
  Humanlike.parseHumanlikeCommand (mentionedMessage ChatGroup "开启拟人模式") @?= Just Humanlike.HumanlikeEnable

testHumanlikeIgnoresProse :: IO ()
testHumanlikeIgnoresProse = do
  Humanlike.parseHumanlikeCommand (qqMessage ChatPrivate "拟人模式是什么意思") @?= Nothing
  Humanlike.parseHumanlikeCommand (qqMessage ChatPrivate "你觉得拟人重要吗") @?= Nothing
  Humanlike.parseHumanlikeCommand (qqMessage ChatPrivate "hi") @?= Nothing
  Humanlike.parseHumanlikeCommand (qqMessage ChatPrivate "") @?= Nothing

testHumanlikeScopeKeys :: IO ()
testHumanlikeScopeKeys = do
  Humanlike.humanlikeScope (qqMessage ChatPrivate "x") @?= Just "qq:private:2822751355"
  Humanlike.humanlikeScope (qqMessage ChatGroup "x") @?= Just "qq:group:100"
  -- 非 QQ 平台不进这套开关。
  Humanlike.humanlikeScope (qqMessage ChatPrivate "x"){platform = PlatformTelegram} @?= Nothing

-- | 时段是"这条消息算不算拟人期间"的唯一依据，边界必须钉住。
testHumanlikeSpans :: IO ()
testHumanlikeSpans = do
  let spans = [(100.0, Just 200.0), (300.0, Nothing)]
      at = posixSecondsToUTCTime
      inside = flip (Humanlike.isInHumanlikeSpan) spans
  assertBool "start is inclusive" (inside (at 100))
  assertBool "middle of a closed span" (inside (at 150))
  assertBool "end is inclusive" (inside (at 200))
  assertBool "after a closed span" (not (inside (at 250)))
  assertBool "before the first span" (not (inside (at 50)))
  assertBool "inside the still-open span" (inside (at 1000000))
  assertBool "no spans means never" (not (Humanlike.isInHumanlikeSpan (at 150) []))

-- | 档案的保留规则必须可预测：同一天替换、只留最近 N 天、溢出的返回给调用方去归档、
-- 手写区一个字都不能动。
testHumanlikeArchiveMerge :: IO ()
testHumanlikeArchiveMerge = do
  let (file1, dropped1) = Archive.mergeAutoRegion 3 "" ("2026-11-07", "第一次")
  assertBool "day recorded" ("2026-11-07: 第一次" `Text.isInfixOf` file1)
  assertEqual "nothing dropped yet" [] dropped1
  assertBool "hand-written section exists" ("我手写" `Text.isInfixOf` file1)

  -- 同一天再关一次，替换而不是各占一条
  let (file2, _) = Archive.mergeAutoRegion 3 file1 ("2026-11-07", "改过")
  assertBool "replaced in place" ("改过" `Text.isInfixOf` file2)
  assertBool "old text gone" (not ("第一次" `Text.isInfixOf` file2))

  -- 超过保留天数时，最旧的被交出去
  let (file3, _) = Archive.mergeAutoRegion 2 file2 ("2026-11-08", "第二次")
      (file4, dropped4) = Archive.mergeAutoRegion 2 file3 ("2026-11-09", "第三次")
  assertEqual "one record pushed out" 1 (length dropped4)
  assertEqual "the oldest one" (Just "2026-11-07") (fst <$> viaNonEmpty head dropped4)
  assertBool "pushed-out day no longer injected" (not ("2026-11-07" `Text.isInfixOf` file4))

  -- 主人自己写的那段永远保留
  let withOwner = "## 我手写（模型不覆盖）\n我是我写的人设\n\n" <> file4
      (file5, _) = Archive.mergeAutoRegion 2 withOwner ("2026-11-10", "第四次")
  assertBool "owner text kept" ("我是我写的人设" `Text.isInfixOf` file5)

-- | 表情目录是模型挑图的唯一依据：解析失败或搜不到就等于没有表情包能力。
testStickerCatalog :: IO ()
testStickerCatalog = do
  let raw = TextEncoding.encodeUtf8 $ Text.unlines
        [ "{\"stickers\":["
        , "{\"id\":\"st_01\",\"file\":\"st_01.gif\",\"mime\":\"image/gif\","
        , "\"emotion\":\"开心\",\"description\":\"黄色圆脸欢呼\",\"tags\":[\"笑\",\"emoji\"]},"
        , "{\"id\":\"st_06\",\"file\":\"st_06.gif\",\"mime\":\"image/gif\","
        , "\"emotion\":\"躺平\",\"description\":\"橘猫四脚摊开\",\"tags\":[\"累\"]}"
        , "]}"
        ]
  case Sticker.parseStickerCatalog raw of
    Nothing -> assertFailure "catalog should parse"
    Just Sticker.StickerCatalog{stickers = catalog} -> do
      assertEqual "two stickers" 2 (length catalog)
      -- 空关键词返回全部，模型先看有什么
      assertEqual "empty keyword lists everything" 2 (length (Sticker.matchStickers "" catalog))
      -- 情绪命中
      assertEqual "emotion match" ["st_01"] (map (.stickerId) (Sticker.matchStickers "开心" catalog))
      -- 标签命中
      assertEqual "tag match" ["st_06"] (map (.stickerId) (Sticker.matchStickers "累" catalog))
      -- 无结果
      assertEqual "no match" ([] :: [Text]) (map (.stickerId) (Sticker.matchStickers "不存在的词" catalog))
