module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Bot.Chat.Driver.Discord as Discord
import qualified Bot.Chat.Bridge.FM as FMBridge
import qualified Bot.Chat.Driver as ChatDriver
import qualified Bot.Chat.Driver.Matrix as Matrix
import qualified Bot.Chat.Driver.QQ as QQ
import qualified Bot.Chat.Driver.Telegram as Telegram
import qualified Bot.Core.Transcript as Transcript
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Handler.FM as FMHandler
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Agent.Middleware.Tools as ToolMiddleware
import Bot.Core.Message
import Bot.Prelude
import qualified Crypto.Cipher.AES as CryptoAES
import qualified Crypto.Cipher.Types as CryptoCipher
import qualified Crypto.Error as CryptoError
import qualified Crypto.Hash as CryptoHash
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.Map.Strict as Map
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "chat platforms"
      [ testCase "QQ user message converts to incoming message" testQqUserMessageConvertsToIncomingMessage
      , testCase "QQ user display name prefers group card" testQqUserDisplayName
      , testCase "transcript preserves sender identity per user turn" testTranscriptPreservesSenderIdentity
      , testCase "QQ invitation actions accept friend and group invites" testQqInvitationActions
      , testCase "QQ recall converts to a deleted incoming message" testQqRecallConvertsToDeletedMessage
      , testCase "QQ poke targeting the bot becomes a natural agent event" testQqPokeBecomesAgentEvent
      , testCase "FM Matrix owner messages use QQ execution context" testFmMatrixOwnerUsesQqContext
      , testCase "FM mapped QQ group uses the Matrix reply pipeline" testFmMappedQqGroupUsesMatrixReplyPipeline
      , testCase "FM Matrix reply body is relayed at most once" testFmMatrixReplyBodyIsRelayedAtMostOnce
      , testCase "FM bridge delivery ids preserve Matrix and QQ recall targets" testFmBridgeDeliveryIds
      , testCase "FM exact library commands bypass the agent" testFmExactLibraryCommands
      , testCase "FM exact recall commands bypass the agent" testFmExactRecallCommands
      , testCase "FM exact model commands bypass the agent" testFmExactModelCommands
      , testCase "FM ignores QQ bridge echoes from Matrix" testFmIgnoresMatrixBridgeEcho
      , testCase "FM tool notifications are readable and hide internal ids" testFmToolNotificationsAreReadable
      , testCase "incoming message JSON defaults missing files" testIncomingMessageJsonDefaultsMissingFiles
      , testCase "QQ superuser is also allowed sender" testQqSuperuserIsAlsoAllowedSender
      , testCase "QQ private chat is open without granting superuser" testQqPrivateChatIsOpen
      , testCase "QQ self message is ignored" testQqSelfMessageIsIgnored
      , testCase "QQ CQ mention string keeps mentioned user ids" testQqCQMentionStringKeepsMentionedUserIds
      , testCase "QQ reply strips its leading mention" testQqReplyStripsLeadingMention
      , testCase "QQ forwarded messages merge all node text" testQqForwardedMessagesMergeAllNodeText
      , testCase "QQ file segment becomes a message file" testQqFileSegmentBecomesMessageFile
      , testCase "QQ record segment becomes a message file" testQqRecordSegmentBecomesMessageFile
      , testCase "QQ sends local file bytes as a base64 resource" testQqBase64FileRef
      , testCase "Telegram user message converts to incoming message" testTelegramUserMessageConvertsToIncomingMessage
      , testCase "Telegram audio becomes a message file" testTelegramAudioBecomesMessageFile
      , testCase "Telegram superuser is also allowed private sender" testTelegramSuperuserIsAlsoAllowedPrivateSender
      , testCase "Telegram bot message is ignored" testTelegramBotMessageIsIgnored
      , testCase "Telegram referenced message includes sender identity" testTelegramReferencedMessageIncludesSenderIdentity
      , testCase "Telegram rich messages use HTML, media, and structured replies" testTelegramRichMessageRequest
      , testCase "Telegram rich-message edits keep HTML content" testTelegramRichMessageEditRequest
      , testCase "Telegram rich-message fallback is safe" testTelegramRichMessageFallback
      , testCase "Telegram CommonMark renders rich HTML" testTelegramCommonMarkRendersRichHtml
      , testCase "Telegram display math spans paragraphs" testTelegramDisplayMathSpansParagraphs
      , testCase "Telegram CommonMark renders rich media blocks" testTelegramCommonMarkRendersRichMedia
      , testCase "Telegram ok false becomes TelegramException description" testTelegramOkFalseBecomesTelegramExceptionDescription
      , testCase "Telegram failure reply is concise" testTelegramFailureReplyIsConcise
      , testCase "Matrix message converts to incoming message" testMatrixMessageConvertsToIncomingMessage
      , testCase "Matrix sync finds room invitations" testMatrixSyncFindsRoomInvitations
      , testCase "Matrix redaction converts to a deleted incoming message" testMatrixRedactionConvertsToDeletedMessage
      , testCase "Matrix direct room converts to private message" testMatrixDirectRoomConvertsToPrivateMessage
      , testCase "Matrix image message includes media URL" testMatrixImageMessageIncludesMediaUrl
      , testCase "Matrix encrypted image message includes media URL" testMatrixEncryptedImageMessageIncludesMediaUrl
      , testCase "Matrix referenced image without body includes media URL" testMatrixReferencedImageWithoutBodyIncludesMediaUrl
      , testCase "Matrix referenced message uses latest replacement" testMatrixReferencedMessageUsesLatestReplacement
      , testCase "Matrix file message includes a message file" testMatrixFileMessageIncludesMessageFile
      , testCase "Matrix audio message includes a message file" testMatrixAudioMessageIncludesMessageFile
      , testCase "Matrix encrypted image bytes decrypt and verify ciphertext hash" testMatrixEncryptedImageBytesDecryptAndVerifyCiphertextHash
      , testCase "Matrix reply relation converts to reply message id" testMatrixReplyRelationConvertsToReplyMessageId
      , testCase "Matrix reply relation strips multiline fallback" testMatrixReplyRelationStripsMultilineFallback
      , testCase "Matrix reply relation strips emote fallback" testMatrixReplyRelationStripsEmoteFallback
      , testCase "Matrix reply relation preserves an ordinary leading quote" testMatrixReplyRelationPreservesOrdinaryLeadingQuote
      , testCase "Matrix quote without reply relation is preserved" testMatrixQuoteWithoutReplyRelationIsPreserved
      , testCase "Matrix edit event converts to incoming message" testMatrixEditEventConvertsToIncomingMessage
      , testCase "Matrix incomplete stream event is ignored" testMatrixIncompleteStreamEventIsIgnored
      , testCase "Matrix superuser is marked in digest" testMatrixSuperuserIsMarkedInDigest
      , testCase "Matrix bot mention uses mentions field only" testMatrixBotMentionUsesMentionsFieldOnly
      , testCase "Matrix Markdown renders custom HTML" testMatrixMarkdownRendersCustomHtml
      , testCase "Matrix Markdown renders user ids as mention links" testMatrixMarkdownRendersUserIdsAsMentionLinks
      , testCase "Discord message converts to incoming message" testDiscordMessageConvertsToIncomingMessage
      , testCase "Discord delete converts to a deleted incoming message" testDiscordDeleteConvertsToDeletedMessage
      , testCase "Discord self message is ignored" testDiscordSelfMessageIsIgnored
      , testCase "Discord superuser and bot mention are marked" testDiscordSuperuserAndBotMentionAreMarked
      , testCase "Discord CommonMark extensions render Discord Markdown" testDiscordCommonMarkExtensionsRenderDiscordMarkdown
      , testCase "Discord avatar value includes avatar URL" testDiscordAvatarValueIncludesAvatarUrl
      , testCase "Discord image context includes embeds and image links" testDiscordImageContextIncludesEmbedsAndImageLinks
      , testCase "Discord document attachment becomes a message file" testDiscordDocumentAttachmentBecomesMessageFile
      , testCase "Discord audio attachment becomes a message file" testDiscordAudioAttachmentBecomesMessageFile
      ]

testQqUserMessageConvertsToIncomingMessage :: IO ()
testQqUserMessageConvertsToIncomingMessage = do
  let incoming = QQ.eventToIncomingMessage (qqMessageEvent 10001)
  ((.platform) <$> incoming) @?= Just PlatformQQ
  ((.text) <$> incoming) @?= Just "hello"
  ((.digest.botId) <$> incoming) @?= Just (Just "424242")

testQqUserDisplayName :: IO ()
testQqUserDisplayName = do
  let event = (qqMessageEvent 10001)
        { QQ.sender = Just (Aeson.object
            [ "nickname" Aeson..= ("昵称" :: Text)
            , "card" Aeson..= ("群名片" :: Text)
            ])
        }
      incoming = fromMaybe (error "expected QQ message") (QQ.eventToIncomingMessage event)
  incoming.senderUsername @?= Just "群名片"

testTranscriptPreservesSenderIdentity :: IO ()
testTranscriptPreservesSenderIdentity = do
  let first = fromMaybe (error "expected first QQ message") $
        QQ.eventToIncomingMessage ((qqMessageEvent 10001)
          { QQ.sender = Just (Aeson.object ["nickname" Aeson..= ("alice" :: Text)])
          , QQ.messageId = Just 1
          })
      second = fromMaybe (error "expected second QQ message") $
        QQ.eventToIncomingMessage ((qqMessageEvent 20002)
          { QQ.sender = Just (Aeson.object ["nickname" Aeson..= ("bob" :: Text)])
          , QQ.messageId = Just 2
          })
      input = MessageInput { text = "hello", attachments = [] }
      transcript = Transcript.appendIncomingMessage second input
        (Transcript.startWithIncomingMessage first input)
      texts = [content | message <- toList transcript.messages, Just (LLM.TextContent content) <- [message.content]]
  length texts @?= 2
  assertBool "first user turn has first sender id" ("sender_id=10001" `Text.isInfixOf` head texts)
  assertBool "second user turn has second sender id" ("sender_id=20002" `Text.isInfixOf` last texts)

testQqInvitationActions :: IO ()
testQqInvitationActions = do
  QQ.invitationAction (qqRequestEvent "friend" Nothing) @?= Just (qqInvitationAction "set_friend_add_request" Nothing)
  QQ.invitationAction (qqRequestEvent "group" (Just "invite")) @?= Just (qqInvitationAction "set_group_add_request" (Just "invite"))
  QQ.invitationAction (qqRequestEvent "group" (Just "add")) @?= Nothing

testQqRecallConvertsToDeletedMessage :: IO ()
testQqRecallConvertsToDeletedMessage = do
  let original = qqMessageEvent 10001
      recall = original
        { QQ.postType = "notice"
        , QQ.rawEvent = Aeson.object
            [ "post_type" Aeson..= ("notice" :: Text)
            , "notice_type" Aeson..= ("group_recall" :: Text)
            ]
        }
      incoming = fromMaybe (error "expected QQ recall") (QQ.eventToIncomingMessage recall)
  incoming.eventKind @?= IncomingMessageDeleted
  incoming.messageId @?= Just (integerMessageId 80001)
  incoming.chatId @?= Just 90001

testQqPokeBecomesAgentEvent :: IO ()
testQqPokeBecomesAgentEvent = do
  let cfg = QQ.Config
        { QQ.host = ""
        , QQ.port = 0
        , QQ.path = ""
        , QQ.token = Nothing
        , QQ.botQQ = Just qqBotUserId
        , QQ.allowedGroups = [90001]
        , QQ.allowedUsers = []
        , QQ.superusers = []
        }
      incoming = fromMaybe (error "expected QQ poke") $
        QQ.eventToIncomingMessageWith cfg (qqPokeEvent qqBotUserId)
  incoming.kind @?= ChatGroup
  incoming.chatId @?= Just 90001
  incoming.senderId @?= Just "10001"
  incoming.messageId @?= Nothing
  incoming.digest.chatIsAllowed @?= True
  incoming.digest.mentionsBot @?= True
  assertBool "poke prompt asks for contextual, non-template speech"
    ("不要复用固定句式" `Text.isInfixOf` incoming.text)
  assertBool "poke targeting another user is ignored"
    (isNothing (QQ.eventToIncomingMessageWith cfg (qqPokeEvent 10002)))

testFmMatrixOwnerUsesQqContext :: IO ()
testFmMatrixOwnerUsesQqContext = do
  let source = matrixBridgeMessage FMBridge.fmOwnerMatrixId "fm 回复测试"
      bridged = fromMaybe (error "expected FM Matrix bridge") (FMBridge.matrixOwnerAsQQ source)
      mentionedSource = source{digest = source.digest{mentionsBot = True}}
      mentioned = fromMaybe (error "expected mentioned FM Matrix bridge") (FMBridge.matrixOwnerAsQQ mentionedSource)
  bridged.platform @?= PlatformQQ
  bridged.chatId @?= Just FMBridge.fmQQGroupId
  bridged.senderId @?= Just FMBridge.fmOwnerQQId
  bridged.text @?= "fm 回复测试"
  bridged.digest.mentionsBot @?= False
  mentioned.digest.mentionsBot @?= True
  bridged.digest.senderIsSuperuser @?= True
  (FMBridge.matrixReplyTarget bridged).platform @?= PlatformMatrix
  FMBridge.fmOwnerRelayBody bridged.text @?= "😼 Fixmood：fm 回复测试"
  FMBridge.fmReplyRelayBody "◆ FM：结果" @?= "😻 FM：结果"
  FMBridge.fmReplyRelayBody "😻 FM：[FM/赛文·极速联赛] [日期2026-08-26] 《标题》 [字数2]\n正文\n-----第650107段-FM发文"
    @?= "[FM/赛文·极速联赛] [日期2026-08-26] 《标题》 [字数2]\n正文\n-----第650107段-FM发文"

testFmMappedQqGroupUsesMatrixReplyPipeline :: IO ()
testFmMappedQqGroupUsesMatrixReplyPipeline = do
  let mapped = fromMaybe (error "expected mapped QQ message") $
        QQ.eventToIncomingMessage ((qqMessageEvent 10001){QQ.groupId = Just FMBridge.fmQQGroupId})
      other = fromMaybe (error "expected other QQ message") $
        QQ.eventToIncomingMessage (qqMessageEvent 10001)
      ownerBridge = fromMaybe (error "expected Matrix owner bridge") $
        FMBridge.matrixOwnerAsQQ (matrixBridgeMessage FMBridge.fmOwnerMatrixId "hello")
      nativeMatrix = matrixBridgeMessage "@alice:g24.at" "hello"
      otherMatrix = nativeMatrix{chatAliases = ["!other:g24.at"]}
  assertBool "mapped QQ group uses Matrix first" (FMBridge.usesMatrixReplyPipeline mapped)
  assertBool "Matrix owner bridge keeps using Matrix first" (FMBridge.usesMatrixReplyPipeline ownerBridge)
  assertBool "native messages in the mapped Matrix room use Matrix first" (FMBridge.usesMatrixReplyPipeline nativeMatrix)
  assertBool "other Matrix rooms keep their direct reply path" (not (FMBridge.usesMatrixReplyPipeline otherMatrix))
  assertBool "other QQ groups keep their direct reply path" (not (FMBridge.usesMatrixReplyPipeline other))
  (FMBridge.matrixReplyTarget mapped).platform @?= PlatformMatrix
  (FMBridge.matrixReplyTarget mapped).chatAliases @?= [FMBridge.fmMatrixRoomId]
  (FMBridge.matrixReplyTarget mapped).messageId @?= Nothing
  (FMBridge.matrixReplyTarget mapped).replyToMessageId @?= Nothing
  (FMBridge.matrixReplyTarget mapped).raw @?= Aeson.Null
  (FMBridge.matrixReplyTarget ownerBridge).messageId @?= ownerBridge.messageId
  (FMBridge.matrixReplyTarget ownerBridge).replyToMessageId @?= Nothing
  (FMBridge.matrixReplyTarget ownerBridge).raw @?= ownerBridge.raw

testFmMatrixReplyBodyIsRelayedAtMostOnce :: IO ()
testFmMatrixReplyBodyIsRelayedAtMostOnce = do
  let messageId = integerMessageId 12345
  bodies <- IORef.newIORef (Map.singleton messageId "final reply")
  firstTake <- ChatDriver.takeBridgeReplyBody bodies messageId
  secondTake <- ChatDriver.takeBridgeReplyBody bodies messageId
  firstTake @?= Just "final reply"
  secondTake @?= Nothing
  emptyBodies <- IORef.newIORef Map.empty
  emptyTake <- ChatDriver.takeBridgeReplyBody emptyBodies messageId
  emptyTake @?= Nothing

testFmBridgeDeliveryIds :: IO ()
testFmBridgeDeliveryIds = do
  let matrixMessageId = textMessageId "$matrix-event:g24.at"
      qqMessageIds = [integerMessageId 12345, integerMessageId (-67890)]
      deliveryId = FMBridge.bridgeDeliveryMessageId matrixMessageId qqMessageIds
  FMBridge.bridgeDeliveryTargets deliveryId @?= Just (matrixMessageId, qqMessageIds)
  FMBridge.bridgeDeliveryTargets matrixMessageId @?= Nothing

testFmExactLibraryCommands :: IO ()
testFmExactLibraryCommands = do
  FMHandler.parseDirectLibraryCommand "fm 文库"
    @?= Just FMHandler.DirectLibraryStats
  FMHandler.parseDirectLibraryCommand "FM 虐500"
    @?= Just (FMHandler.DirectLibraryArticle "虐" 500)
  FMHandler.parseDirectLibraryCommand "fm文来"
    @?= Just (FMHandler.DirectLibraryArticle "" 0)
  FMHandler.parseDirectLibraryCommand "fm 续"
    @?= Just FMHandler.DirectLibraryContinue
  FMHandler.parseDirectLibraryCommand "FM续发"
    @?= Just FMHandler.DirectLibraryContinue
  FMHandler.parseDirectLibraryCommand "fm 前500 100 顺 击6 准100"
    @?= Just (FMHandler.DirectLibrarySingle "前500" 100 "顺" 6 100)
  FMHandler.parseDirectLibraryCommand "fm 前1500 50乱"
    @?= Just (FMHandler.DirectLibrarySingle "前1500" 50 "乱" 0 0)
  FMHandler.parseDirectLibraryCommand "fm 虐1500"
    @?= Just (FMHandler.DirectLibraryInvalid "单次发文最多 1400 字。")
  FMHandler.parseDirectLibraryCommand "fm 帮我找一篇虐文"
    @?= Nothing

testFmExactRecallCommands :: IO ()
testFmExactRecallCommands = do
  FMHandler.parseDirectRecallCommand "@FM 撤回这条消息"
    @?= Nothing
  FMHandler.parseDirectRecallCommand "@FM 撤回"
    @?= Just FMHandler.DirectRecallOne
  FMHandler.parseDirectRecallCommand "fm 撤回最近所有消息"
    @?= Just FMHandler.DirectRecallAll
  FMHandler.parseDirectRecallCommand "FM 撤回全部消息"
    @?= Just FMHandler.DirectRecallAll

testFmExactModelCommands :: IO ()
testFmExactModelCommands = do
  FMHandler.parseDirectModelCommand "fm 模型列表"
    @?= Just FMHandler.DirectModelStatus
  FMHandler.parseDirectModelCommand "fm 切换模型 deepseek-v4-flash-vision"
    @?= Just (FMHandler.DirectModelSwitch "deepseek-v4-flash-vision")
  FMHandler.parseDirectModelCommand "fm 使用 deepseek-v4-pro"
    @?= Just (FMHandler.DirectModelSwitch "deepseek-v4-pro")
  FMHandler.parseDirectModelCommand "fm，切换视觉模型"
    @?= Just (FMHandler.DirectModelSwitch "deepseek-v4-flash-vision")
  FMHandler.parseDirectModelCommand "让fm切换带视觉的模型"
    @?= Just (FMHandler.DirectModelSwitch "deepseek-v4-flash-vision")

testFmIgnoresMatrixBridgeEcho :: IO ()
testFmIgnoresMatrixBridgeEcho = do
  assertBool "QQ member bridge echo is ignored"
    (FMBridge.isMirroredQQMatrixMessage (matrixBridgeMessage "@qq_10001:pfeiwu.com" "hello"))
  assertBool "QQ bot bridge echo is ignored"
    (FMBridge.isMirroredQQMatrixMessage (matrixBridgeMessage "@qq_3471095459:pfeiwu.com" "reply"))
  assertBool "native Matrix member is not mistaken for a QQ bridge"
    (not (FMBridge.isMirroredQQMatrixMessage (matrixBridgeMessage "@alice:g24.at" "hello")))

testFmToolNotificationsAreReadable :: IO ()
testFmToolNotificationsAreReadable = do
  let rankCall = LLM.ToolCall
        { LLM.id = "call-secret-rank"
        , LLM.name = "fm_live_competition_rank"
        , LLM.arguments = "{\"source\":\"极速杯\"}"
        }
      avatarCall = LLM.ToolCall
        { LLM.id = "call-secret-avatar"
        , LLM.name = "user_avatar"
        , LLM.arguments = "{\"user_id\":\"2822751355\"}"
        }
      rendered = Text.intercalate "\n" $
        map (ToolMiddleware.toolProgressText . (.name)) [rankCall, avatarCall]
  rendered @?= "😻 FM：正在调用 fm_live_competition_rank 工具…\n😻 FM：正在调用 user_avatar 工具…"
  assertBool "tool call id is hidden" (not ("call-secret" `Text.isInfixOf` rendered))
  assertBool "tool arguments are hidden" (not ("2822751355" `Text.isInfixOf` rendered))

testIncomingMessageJsonDefaultsMissingFiles :: IO ()
testIncomingMessageJsonDefaultsMissingFiles = do
  let message = fromMaybe (error "expected QQ message") (QQ.eventToIncomingMessage (qqMessageEvent 10001))
      legacyJson = case Aeson.toJSON message of
        Aeson.Object fields -> Aeson.Object (AesonKeyMap.delete "eventKind" (AesonKeyMap.delete "files" fields))
        _ -> error "expected message object"
  case Aeson.fromJSON legacyJson of
    Aeson.Success (decoded :: IncomingMessage) -> do
      decoded.files @?= []
      decoded.eventKind @?= IncomingMessageCreated
    Aeson.Error err -> assertFailure err

testQqFileSegmentBecomesMessageFile :: IO ()
testQqFileSegmentBecomesMessageFile = do
  let original :: QQ.Event
      original = qqMessageEvent 10001
      fileMessage = Just (Aeson.toJSON
            [ Aeson.object
                [ "type" Aeson..= ("file" :: Text)
                , "data" Aeson..= Aeson.object
                    [ "file" Aeson..= ("notes.txt" :: Text)
                    , "file_id" Aeson..= ("file-123" :: Text)
                    ]
                ]
            ])
      event = QQ.Event
        { QQ.time = original.time
        , QQ.selfId = original.selfId
        , QQ.postType = original.postType
        , QQ.messageType = original.messageType
        , QQ.subType = original.subType
        , QQ.messageId = original.messageId
        , QQ.userId = original.userId
        , QQ.groupId = original.groupId
        , QQ.message = fileMessage
        , QQ.rawMessage = original.rawMessage
        , QQ.sender = original.sender
        , QQ.rawEvent = original.rawEvent
        }
      incoming = QQ.eventToIncomingMessage event
  ((.files) <$> incoming) @?= Just [MessageFile{name = "notes.txt", ref = "qq-file:file-123"}]

testQqBase64FileRef :: IO ()
testQqBase64FileRef =
  QQ.base64FileRef "cosmobot" @?= "base64://Y29zbW9ib3Q="

testQqRecordSegmentBecomesMessageFile :: IO ()
testQqRecordSegmentBecomesMessageFile = do
  let original = qqMessageEvent 10001
      event = QQ.Event
        { QQ.time = original.time
        , QQ.selfId = original.selfId
        , QQ.postType = original.postType
        , QQ.messageType = original.messageType
        , QQ.subType = original.subType
        , QQ.messageId = original.messageId
        , QQ.userId = original.userId
        , QQ.groupId = original.groupId
        , QQ.message = Just (Aeson.toJSON
            [ Aeson.object
                [ "type" Aeson..= ("record" :: Text)
                , "data" Aeson..= Aeson.object ["file" Aeson..= ("https://example.test/voice.ogg" :: Text)]
                ]
            ])
        , QQ.rawMessage = original.rawMessage
        , QQ.sender = original.sender
        , QQ.rawEvent = original.rawEvent
        }
  ((.files) <$> QQ.eventToIncomingMessage event)
    @?= Just [MessageFile{name = "audio", ref = "https://example.test/voice.ogg"}]

testQqSuperuserIsAlsoAllowedSender :: IO ()
testQqSuperuserIsAlsoAllowedSender = do
  let cfg = QQ.Config
        { QQ.host = ""
        , QQ.port = 0
        , QQ.path = ""
        , QQ.token = Nothing
        , QQ.botQQ = Nothing
        , QQ.allowedGroups = []
        , QQ.allowedUsers = []
        , QQ.superusers = [10001]
        }
      incoming = fromMaybe (error "expected incoming QQ message") $
        QQ.eventToIncomingMessageWith cfg (qqMessageEvent 10001)
  incoming.digest.senderIsAllowed @?= True
  incoming.digest.senderIsSuperuser @?= True

testQqPrivateChatIsOpen :: IO ()
testQqPrivateChatIsOpen = do
  let cfg = QQ.Config
        { QQ.host = ""
        , QQ.port = 0
        , QQ.path = ""
        , QQ.token = Nothing
        , QQ.botQQ = Just qqBotUserId
        , QQ.allowedGroups = []
        , QQ.allowedUsers = []
        , QQ.superusers = []
        }
      event = (qqMessageEvent 10001)
        { QQ.messageType = Just "private"
        , QQ.groupId = Nothing
        }
      incoming = fromMaybe (error "expected incoming private QQ message") $
        QQ.eventToIncomingMessageWith cfg event
  incoming.kind @?= ChatPrivate
  incoming.digest.senderIsAllowed @?= True
  incoming.digest.senderIsSuperuser @?= False

testQqSelfMessageIsIgnored :: IO ()
testQqSelfMessageIsIgnored =
  assertBool
    "QQ messages sent by the bot itself are ignored"
    (isNothing (QQ.eventToIncomingMessage (qqMessageEvent qqBotUserId)))

testQqCQMentionStringKeepsMentionedUserIds :: IO ()
testQqCQMentionStringKeepsMentionedUserIds = do
  let cfg = QQ.Config
        { QQ.host = ""
        , QQ.port = 0
        , QQ.path = ""
        , QQ.token = Nothing
        , QQ.botQQ = Just qqBotUserId
        , QQ.allowedGroups = []
        , QQ.allowedUsers = []
        , QQ.superusers = []
        }
      event = (qqMessageEvent 10001)
        { QQ.message = Just (Aeson.String "[CQ:at,qq=123456] hi [CQ:at,qq=424242]")
        , QQ.rawMessage = Just "[CQ:at,qq=123456] hi [CQ:at,qq=424242]"
        }
      incoming = fromMaybe (error "expected incoming QQ message") $
        QQ.eventToIncomingMessageWith cfg event
  incoming.mentions @?= ["123456", show qqBotUserId]
  incoming.text @?= "@123456 hi @424242"
  incoming.digest.mentionsBot @?= True

testQqReplyStripsLeadingMention :: IO ()
testQqReplyStripsLeadingMention = do
  let arrayMessage = Aeson.toJSON
        [ Aeson.object
            [ "type" Aeson..= ("reply" :: Text)
            , "data" Aeson..= Aeson.object ["id" Aeson..= (80000 :: Integer)]
            ]
        , Aeson.object
            [ "type" Aeson..= ("at" :: Text)
            , "data" Aeson..= Aeson.object ["qq" Aeson..= (123456 :: Integer)]
            ]
        , textSegment " hello"
        ]
  ((.text) <$> QQ.eventToIncomingMessage (qqEvent arrayMessage)) @?= Just "hello"
  ((.text) <$> QQ.eventToIncomingMessage (qqEvent (Aeson.String "[CQ:reply,id=80000][CQ:at,qq=123456] hello"))) @?= Just "hello"
  where
    qqEvent message =
      case Aeson.fromJSON (Aeson.object
        [ "post_type" Aeson..= ("message" :: Text)
        , "message" Aeson..= message
        ]) of
        Aeson.Success event -> event
        Aeson.Error err -> error (toText err)

testQqForwardedMessagesMergeAllNodeText :: IO ()
testQqForwardedMessagesMergeAllNodeText =
  QQ.forwardedMessagesText forwardedMessages @?= Text.intercalate "\n"
    [ "Alice (QQ: 10001):"
    , "<cosmobot:forwarded_msg>"
    , "Bob (QQ: 10002):"
    , "nested"
    , "[image]"
    , "</cosmobot:forwarded_msg>"
    , "Carol (QQ: 10003):"
    , "second"
    , "[file: notes.txt]"
    , "third"
    ]
  where
    forwardedMessages = Aeson.object
      [ "message" Aeson..=
          [ Aeson.object
              [ "type" Aeson..= ("node" :: Text)
              , "raw_message" Aeson..= ("[CQ:forward,id=nested-forward,content=&#91;object Object&#93;]" :: Text)
              , "data" Aeson..= Aeson.object
                  [ "nickname" Aeson..= ("Alice" :: Text)
                  , "user_id" Aeson..= ("10001" :: Text)
                  , "content" Aeson..=
                      [ Aeson.object
                          [ "type" Aeson..= ("forward" :: Text)
                          , "data" Aeson..= Aeson.object
                              [ "id" Aeson..= ("nested-forward" :: Text)
                              , "content" Aeson..=
                                  [ Aeson.object
                                      [ "type" Aeson..= ("node" :: Text)
                                      , "data" Aeson..= Aeson.object
                                          [ "name" Aeson..= ("Bob" :: Text)
                                          , "uin" Aeson..= (10002 :: Integer)
                                          , "content" Aeson..=
                                              [ textSegment "nested"
                                              , imageSegment "https://example.test/ignored.png"
                                              ]
                                          ]
                                      ]
                                  ]
                              ]
                          ]
                      ]
                  ]
              ]
          , Aeson.object
              [ "sender" Aeson..= Aeson.object
                  [ "nickname" Aeson..= ("Carol" :: Text)
                  , "user_id" Aeson..= ("10003" :: Text)
                  ]
              , "content" Aeson..=
                  [ textSegment "second"
                  , Aeson.object
                      [ "type" Aeson..= ("file" :: Text)
                      , "data" Aeson..= Aeson.object
                          [ "name" Aeson..= ("notes.txt" :: Text)
                          , "file" Aeson..= ("notes.txt" :: Text)
                          ]
                      ]
                  ]
              ]
          , Aeson.object
              [ "raw_message" Aeson..= ("third" :: Text)
              ]
          ]
      ]

testTelegramUserMessageConvertsToIncomingMessage :: IO ()
testTelegramUserMessageConvertsToIncomingMessage = do
  let incoming = Telegram.updateToIncomingMessage (telegramUpdate False)
  ((.platform) <$> incoming) @?= Just PlatformTelegram
  ((.text) <$> incoming) @?= Just "hello"

testTelegramAudioBecomesMessageFile :: IO ()
testTelegramAudioBecomesMessageFile = do
  let audio = Telegram.TelegramMedia
        { Telegram.fileId = "audio-file-id"
        , Telegram.fileUniqueId = "audio-unique-id"
        , Telegram.fileName = Just "song.mp3"
        , Telegram.mimeType = Just "audio/mpeg"
        }
      incoming = Telegram.updateToIncomingMessage (telegramUpdateWithMessage (telegramMessage False){Telegram.audio = Just audio})
  ((.files) <$> incoming) @?= Just [MessageFile{name = "song.mp3", ref = "audio-file-id"}]

testTelegramSuperuserIsAlsoAllowedPrivateSender :: IO ()
testTelegramSuperuserIsAlsoAllowedPrivateSender = do
  let cfg = Telegram.Config
        { Telegram.botToken = ""
        , Telegram.botIds = []
        , Telegram.botUsernames = []
        , Telegram.allowedChatIds = []
        , Telegram.allowedChatAliases = []
        , Telegram.superusers = ["alice"]
        }
      incoming = fromMaybe (error "expected incoming Telegram message") $
        Telegram.updateToIncomingMessageWith cfg (telegramUpdateWithMessage privateTelegramMessage)
  incoming.digest.senderIsAllowed @?= True
  incoming.digest.senderIsSuperuser @?= True

testTelegramBotMessageIsIgnored :: IO ()
testTelegramBotMessageIsIgnored =
  assertBool
    "Telegram bot messages are ignored"
    (isNothing (Telegram.updateToIncomingMessage (telegramUpdate True)))

testTelegramReferencedMessageIncludesSenderIdentity :: IO ()
testTelegramReferencedMessageIncludesSenderIdentity = do
  let referencedSender = Telegram.User
        { Telegram.id = 10001
        , Telegram.isBot = False
        , Telegram.firstName = "Bob"
        , Telegram.lastName = Just "Smith"
        , Telegram.username = Just "bob"
        }
  let referenced = (telegramMessage False)
        { Telegram.messageId = 70001
        , Telegram.from = Just referencedSender
        , Telegram.text = Just "quoted"
        }
      messageWithReply = (telegramMessage False){Telegram.replyToMessage = Just referenced}
      incoming = fromMaybe (error "expected incoming Telegram message") $
        Telegram.updateToIncomingMessage (telegramUpdateWithMessage messageWithReply)
      rawMessage :: Telegram.Message
      rawMessage = case Aeson.fromJSON incoming.raw of
        Aeson.Success message -> message
        Aeson.Error err -> error (toText err)
      fetched = rawMessage.replyToMessage
  ((\user -> Text.unwords [user.firstName, fromMaybe "" user.lastName]) <$> (fetched >>= (.from))) @?= Just "Bob Smith"
  ((\user -> maybe (Text.pack (show user.id :: String)) ("@" <>) user.username) <$> (fetched >>= (.from))) @?= Just "@bob"
  (fetched >>= (.text)) @?= Just "quoted"

testTelegramRichMessageRequest :: IO ()
testTelegramRichMessageRequest =
  Aeson.toJSON
    Telegram.SendRichMessageRequest
      { Telegram.chatId = 42
      , Telegram.messageThreadId = Nothing
      , Telegram.richMessage = Telegram.InputRichMessage
          { Telegram.html = "<h1>Heading</h1>"
          , Telegram.media = Just
              [ Telegram.InputRichMessageMedia
                  { Telegram.id = "media-1"
                  , Telegram.media = Telegram.InputRichMedia
                      { Telegram.type_ = "photo"
                      , Telegram.media = "attach://rich-media-1"
                      }
                  }
              ]
          }
      , Telegram.disableNotification = Nothing
      , Telegram.replyParameters = Just (Telegram.ReplyParameters 7)
      }
    @?= Aeson.object
      [ "chat_id" Aeson..= (42 :: Integer)
      , "rich_message" Aeson..= Aeson.object
          [ "html" Aeson..= ("<h1>Heading</h1>" :: Text)
          , "media" Aeson..=
              [ Aeson.object
                  [ "id" Aeson..= ("media-1" :: Text)
                  , "media" Aeson..= Aeson.object
                      [ "type" Aeson..= ("photo" :: Text)
                      , "media" Aeson..= ("attach://rich-media-1" :: Text)
                      ]
                  ]
              ]
          ]
      , "reply_parameters" Aeson..= Aeson.object
          [ "message_id" Aeson..= (7 :: Integer)
          ]
      ]

testTelegramRichMessageEditRequest :: IO ()
testTelegramRichMessageEditRequest =
  Aeson.toJSON
    Telegram.EditMessageTextRequest
      { Telegram.chatId = 42
      , Telegram.messageId = 8
      , Telegram.richMessage = Telegram.InputRichMessage
          { Telegram.html = "<p><strong>updated</strong></p>"
          , Telegram.media = Nothing
          }
      }
    @?= Aeson.object
      [ "chat_id" Aeson..= (42 :: Integer)
      , "message_id" Aeson..= (8 :: Integer)
      , "rich_message" Aeson..= Aeson.object
          [ "html" Aeson..= ("<p><strong>updated</strong></p>" :: Text)
          ]
      ]

testTelegramRichMessageFallback :: IO ()
testTelegramRichMessageFallback = do
  assertBool "short bad requests can use the legacy request" $
    Telegram.richMessageFallbackAllowed "body" (Telegram.TelegramException "Bad Request: can't parse rich message")
  assertBool "transport failures must not risk duplicate sends" $
    not (Telegram.richMessageFallbackAllowed "body" (Telegram.TelegramException "connection reset"))
  assertBool "legacy requests cannot carry long rich messages" $
    not (Telegram.richMessageFallbackAllowed (Text.replicate 4097 "x") (Telegram.TelegramException "Bad Request: can't parse rich message"))

testTelegramCommonMarkRendersRichHtml :: IO ()
testTelegramCommonMarkRendersRichHtml = do
  let html = Telegram.formatTelegramRichHtml $ Text.unlines
        [ "# Heading"
        , ""
        , "**bold** ~~old~~ H~2~O x^2^ and $x+y$"
        , ""
        , "- [x] done"
        , "- [ ] todo"
        , ""
        , "| left | right |"
        , "|:-----|------:|"
        , "| 1 | 2 |"
        , ""
        , "[^a]: **detail**"
        , ""
        , "note[^a]"
        , ""
        , "![👍](tg://emoji?id=5368324170671202286) ![tomorrow](tg://time?unix=1647531900&format=wDT)"
        , ""
        , "Inline: \\(E = mc^2\\)"
        , ""
        , "\\[ \\sum_{i=1}^{n} i = \\frac{n(n+1)}{2} \\]"
        , ""
        , "\\[ \\int_{-\\infty}^{\\infty} e^{-x^2}\\,dx = \\sqrt{\\pi} \\]"
        , ""
        , "\\[ A = \\begin{pmatrix} 1 & 2 \\\\ 3 & 4 \\end{pmatrix} \\]"
        , ""
        , "`\\(not math\\)`"
        ]
  traverse_ (\tag -> assertBool ("expected Telegram rich HTML tag: " <> Text.unpack tag <> " in " <> Text.unpack html) (tag `Text.isInfixOf` html))
    [ "<h1>Heading</h1>"
    , "<strong>bold</strong>"
    , "<del>old</del>"
    , "<sub>2</sub>"
    , "<sup>2</sup>"
    , "<tg-math>x+y</tg-math>"
    , "<input type=\"checkbox\" checked=\"\" />"
    , "<table>"
    , "<th align=\"left\">left</th>"
    , "<td align=\"right\">2</td>"
    , "<tg-reference name=\"note-a\">"
    , "<a href=\"#note-a\">[1]</a>"
    , "<tg-emoji emoji-id=\"5368324170671202286\">👍</tg-emoji>"
    , "<tg-time unix=\"1647531900\" format=\"wDT\">tomorrow</tg-time>"
    , "<tg-math>E = mc^2</tg-math>"
    , "<tg-math-block> \\sum_{i=1}^{n} i = \\frac{n(n+1)}{2} </tg-math-block>"
    , "<tg-math-block> \\int_{-\\infty}^{\\infty} e^{-x^2}\\,dx = \\sqrt{\\pi} </tg-math-block>"
    , "<tg-math-block> A = \\begin{pmatrix} 1 &amp; 2 \\\\ 3 &amp; 4 \\end{pmatrix} </tg-math-block>"
    , "<code>\\(not math\\)</code>"
    ]

testTelegramDisplayMathSpansParagraphs :: IO ()
testTelegramDisplayMathSpansParagraphs =
  Telegram.formatTelegramRichHtml input @?= expected
  where
    input = Text.unlines
      [ "\\[ \\begin{pmatrix}X \\\\ Y \\\\ Z\\end{pmatrix}"
      , ""
      , "\\begin{pmatrix} -\\frac{95}{2} & -\\frac{95}{2} & \\frac{277}{12} \\\\ -\\frac{91}{2} & \\frac{91}{2} & 0 \\\\ -6 & -6 & 1 \\end{pmatrix} \\begin{pmatrix}a \\\\ b \\\\ c\\end{pmatrix}. \\tag{2} \\]"
      ]
    expected = "<tg-math-block> \\begin{pmatrix}X \\\\ Y \\\\ Z\\end{pmatrix}\n\n\\begin{pmatrix} -\\frac{95}{2} &amp; -\\frac{95}{2} &amp; \\frac{277}{12} \\\\ -\\frac{91}{2} &amp; \\frac{91}{2} &amp; 0 \\\\ -6 &amp; -6 &amp; 1 \\end{pmatrix} \\begin{pmatrix}a \\\\ b \\\\ c\\end{pmatrix}. \\tag{2} </tg-math-block>"

testTelegramCommonMarkRendersRichMedia :: IO ()
testTelegramCommonMarkRendersRichMedia = do
  let html = Telegram.formatTelegramRichHtml $ Text.unlines
        [ "![](https://example.test/photo.jpg)"
        , ""
        , "![](https://example.test/video.mp4 \"Video caption\")"
        , ""
        , "![](https://example.test/audio.mp3)"
        , ""
        , "$$E = mc^2$$"
        ]
  traverse_ (\tag -> assertBool ("expected Telegram rich media tag: " <> Text.unpack tag <> " in " <> Text.unpack html) (tag `Text.isInfixOf` html))
    [ "<img src=\"https://example.test/photo.jpg\" />"
    , "<figure>"
    , "<video src=\"https://example.test/video.mp4\"></video>"
    , "<figcaption>Video caption</figcaption>"
    , "<audio src=\"https://example.test/audio.mp3\"></audio>"
    , "<tg-math-block>E = mc^2</tg-math-block>"
    ]

testTelegramOkFalseBecomesTelegramExceptionDescription :: IO ()
testTelegramOkFalseBecomesTelegramExceptionDescription = do
  let raw = ByteStringChar8.pack "{\"ok\":false,\"error_code\":400,\"description\":\"Bad Request: can't parse entities\"}"
      parsed = either (error . toText) id (Aeson.eitherDecodeStrict raw :: Either String Telegram.TelegramResult)
  result <- runEff (trySync (Telegram.parseTelegramResult parsed)) :: IO (Either SomeException Telegram.Message)
  case result of
    Left err ->
      exceptionFirstLine err @?= "Bad Request: can't parse entities"
    Right _ ->
      assertFailure "expected TelegramException"

testTelegramFailureReplyIsConcise :: IO ()
testTelegramFailureReplyIsConcise =
  Telegram.telegramFailureReplyText (Telegram.TelegramException "Bad Request: message is too long")
    @?= "Telegram request failed: Bad Request: message is too long"

exceptionFirstLine :: Exception err => err -> Text
exceptionFirstLine =
  Text.takeWhile (/= '\n') . toText . displayException

testMatrixMessageConvertsToIncomingMessage :: IO ()
testMatrixMessageConvertsToIncomingMessage = do
  let incoming = Matrix.eventToIncomingMessage matrixRoomEvent
  ((.platform) <$> incoming) @?= Just PlatformMatrix
  ((.kind) <$> incoming) @?= Just ChatGroup
  ((.chatAliases) <$> incoming) @?= Just ["!room:example.org"]
  ((.senderUsername) <$> incoming) @?= Just (Just "@alice:example.org")
  ((.text) <$> incoming) @?= Just "hello"

testMatrixSyncFindsRoomInvitations :: IO ()
testMatrixSyncFindsRoomInvitations = do
  let response = Aeson.object
        [ "next_batch" Aeson..= ("token" :: Text)
        , "rooms" Aeson..= Aeson.object
            [ "invite" Aeson..= Aeson.object ["!dm:example.org" Aeson..= Aeson.object []]
            ]
        ]
  case Aeson.fromJSON response of
    Aeson.Success syncResponse -> Matrix.syncInvitedRoomIds syncResponse @?= ["!dm:example.org"]
    Aeson.Error err -> assertFailure err

testMatrixRedactionConvertsToDeletedMessage :: IO ()
testMatrixRedactionConvertsToDeletedMessage = do
  let original = matrixRoomEvent.event
      redaction = matrixRoomEvent
        { Matrix.event = original
            { Matrix.type_ = "m.room.redaction"
            , Matrix.raw = Aeson.object
                [ "type" Aeson..= ("m.room.redaction" :: Text)
                , "redacts" Aeson..= ("$deleted:example.org" :: Text)
                ]
            }
        }
      incoming = fromMaybe (error "expected Matrix redaction") (Matrix.eventToIncomingMessage redaction)
  incoming.eventKind @?= IncomingMessageDeleted
  incoming.messageId @?= Just (textMessageId "$deleted:example.org")
  incoming.chatAliases @?= ["!room:example.org"]

testMatrixDirectRoomConvertsToPrivateMessage :: IO ()
testMatrixDirectRoomConvertsToPrivateMessage = do
  let incoming = Matrix.eventToIncomingMessage matrixDirectRoomEvent
  ((.platform) <$> incoming) @?= Just PlatformMatrix
  ((.kind) <$> incoming) @?= Just ChatPrivate
  ((.chatAliases) <$> incoming) @?= Just ["!room:example.org"]
  ((.senderUsername) <$> incoming) @?= Just (Just "@alice:example.org")
  ((.text) <$> incoming) @?= Just "hello"

testMatrixImageMessageIncludesMediaUrl :: IO ()
testMatrixImageMessageIncludesMediaUrl = do
  let incoming = Matrix.eventToIncomingMessage matrixImageRoomEvent
  ((.text) <$> incoming) @?= Just "image.png"
  ((.imageUrls) <$> incoming) @?= Just ["mxc://example.org/plain-image"]

testMatrixEncryptedImageMessageIncludesMediaUrl :: IO ()
testMatrixEncryptedImageMessageIncludesMediaUrl = do
  let incoming = Matrix.eventToIncomingMessage matrixEncryptedImageRoomEvent
  ((.text) <$> incoming) @?= Just "image.png"
  ((.imageUrls) <$> incoming) @?= Just ["mxc://example.org/encrypted-image"]

testMatrixReferencedImageWithoutBodyIncludesMediaUrl :: IO ()
testMatrixReferencedImageWithoutBodyIncludesMediaUrl = do
  let referenced = Matrix.matrixReferencedMessage matrixImageWithoutBodyRoomEvent.event
  ((.text) <$> referenced) @?= Just ""
  ((.imageUrls) <$> referenced) @?= Just ["mxc://example.org/bodyless-image"]

testMatrixReferencedMessageUsesLatestReplacement :: IO ()
testMatrixReferencedMessageUsesLatestReplacement = do
  let latestContent = Aeson.object
        [ "msgtype" Aeson..= ("m.text" :: Text)
        , "body" Aeson..= ("complete" :: Text)
        ]
      latestReplacement = Aeson.object
        [ "content" Aeson..= Aeson.object
            [ "m.new_content" Aeson..= latestContent
            ]
        ]
      event = matrixRoomEvent.event
        { Matrix.content = matrixRoomEvent.event.content{Matrix.body = Just "partial"}
        , Matrix.raw = Aeson.object
            [ "content" Aeson..= Aeson.object
                [ "msgtype" Aeson..= ("m.text" :: Text)
                , "body" Aeson..= ("partial" :: Text)
                ]
            , "unsigned" Aeson..= Aeson.object
                [ "m.relations" Aeson..= Aeson.object
                    [ "m.replace" Aeson..= latestReplacement
                    ]
                ]
            ]
        }
      referenced = Matrix.matrixReferencedMessage event
  ((.messageId) <$> referenced) @?= Just (Just (textMessageId "$event:example.org"))
  ((.text) <$> referenced) @?= Just "complete"

testMatrixFileMessageIncludesMessageFile :: IO ()
testMatrixFileMessageIncludesMessageFile = do
  let roomEvent = matrixRoomEvent
        { Matrix.event = matrixRoomEvent.event
            { Matrix.content = matrixRoomEvent.event.content
                { Matrix.msgtype = Just "m.file"
                , Matrix.body = Just "notes.txt"
                }
            , Matrix.raw = matrixImageRawContent
                [ "msgtype" Aeson..= ("m.file" :: Text)
                , "body" Aeson..= ("notes.txt" :: Text)
                , "url" Aeson..= ("mxc://example.org/notes" :: Text)
                , "info" Aeson..= Aeson.object ["mimetype" Aeson..= ("text/plain" :: Text)]
                ]
            }
        }
      incoming = Matrix.eventToIncomingMessage roomEvent
  ((.files) <$> incoming) @?= Just [MessageFile{name = "notes.txt", ref = "mxc://example.org/notes"}]

testMatrixAudioMessageIncludesMessageFile :: IO ()
testMatrixAudioMessageIncludesMessageFile = do
  let event = matrixRoomEvent.event
        { Matrix.content = matrixRoomEvent.event.content{Matrix.msgtype = Just "m.audio", Matrix.body = Just "voice.ogg"}
        , Matrix.raw = matrixImageRawContent
            [ "msgtype" Aeson..= ("m.audio" :: Text)
            , "body" Aeson..= ("voice.ogg" :: Text)
            , "url" Aeson..= ("mxc://example.org/voice" :: Text)
            , "info" Aeson..= Aeson.object ["mimetype" Aeson..= ("audio/ogg" :: Text)]
            ]
        }
      incoming = Matrix.eventToIncomingMessage matrixRoomEvent{Matrix.event = event}
  ((.files) <$> incoming) @?= Just [MessageFile{name = "voice.ogg", ref = "mxc://example.org/voice"}]

testMatrixEncryptedImageBytesDecryptAndVerifyCiphertextHash :: IO ()
testMatrixEncryptedImageBytesDecryptAndVerifyCiphertextHash = do
  let key = StrictByteString.replicate 32 0
      iv = StrictByteString.replicate 16 0
      plainText = "Matrix encrypted image bytes"
      cipherText = matrixAes256Ctr key iv plainText
      encryptedKey = TextEncoding.decodeUtf8 (Base64URL.encodeUnpadded key)
      encryptedIv = TextEncoding.decodeUtf8 (Base64.encode iv)
      encryptedSha256 = TextEncoding.decodeUtf8 (Base64.encode (matrixSha256 cipherText))
  chunks <- Matrix.decryptMatrixEncryptedBytesForTest encryptedKey encryptedIv encryptedSha256 [StrictByteString.take 7 cipherText, StrictByteString.drop 7 cipherText]
  StrictByteString.concat chunks @?= plainText

testMatrixMarkdownRendersCustomHtml :: IO ()
testMatrixMarkdownRendersCustomHtml =
  Matrix.formatMatrixMarkdown markdown @?= Just expected
  where
    markdown =
      Text.unlines
        [ "**hi** and `code`"
        , ""
        , "~~old~~"
        , ""
        , "- [x] done"
        , ""
        , "| a | b |"
        , "| - | - |"
        , "| 1 | 2 |"
        ]
    expected =
      Text.intercalate "\n"
        [ "<p><strong>hi</strong> and <code>code</code></p>"
        , "<p><del>old</del></p>"
        , "<ul class=\"task-list\">"
        , "<li><input type=\"checkbox\" disabled=\"\" checked=\"\" />done"
        , "</li>"
        , "</ul>"
        , "<table>"
        , "<thead>"
        , "<tr>"
        , "<th>a</th>"
        , "<th>b</th>"
        , "</tr>"
        , "</thead>"
        , "<tbody>"
        , "<tr>"
        , "<td>1</td>"
        , "<td>2</td>"
        , "</tr>"
        , "</tbody>"
        , "</table>"
        ]

testMatrixMarkdownRendersUserIdsAsMentionLinks :: IO ()
testMatrixMarkdownRendersUserIdsAsMentionLinks =
  Matrix.formatMatrixMarkdownWithMentionNames mentionNames "@foo:matrix.org @bar:matrix.org." @?= Just expected
  where
    mentionNames =
      Map.fromList
        [ ("@foo:matrix.org", "Foo")
        , ("@bar:matrix.org", "Bar")
        ]
    expected =
      Text.intercalate "\n"
        [ "<p><a href=\"https://matrix.to/#/@foo:matrix.org\">@Foo</a> <a href=\"https://matrix.to/#/@bar:matrix.org\">@Bar</a>.</p>"
        ]

testMatrixReplyRelationConvertsToReplyMessageId :: IO ()
testMatrixReplyRelationConvertsToReplyMessageId = do
  let incoming = Matrix.eventToIncomingMessage matrixReplyRoomEvent
  ((.replyToMessageId) <$> incoming) @?= Just (Just (textMessageId "$parent:example.org"))

testMatrixReplyRelationStripsMultilineFallback :: IO ()
testMatrixReplyRelationStripsMultilineFallback = do
  let event = matrixReplyRoomEvent
        { Matrix.event = matrixReplyRoomEvent.event
            { Matrix.content = matrixReplyRoomEvent.event.content
                { Matrix.body = Just "> <@alice:example.org> first line\n> \n> second line\n\nreply"
                }
            }
        }
  ((.text) <$> Matrix.eventToIncomingMessage event) @?= Just "reply"

testMatrixReplyRelationStripsEmoteFallback :: IO ()
testMatrixReplyRelationStripsEmoteFallback = do
  let event = matrixReplyRoomEvent
        { Matrix.event = matrixReplyRoomEvent.event
            { Matrix.content = matrixReplyRoomEvent.event.content
                { Matrix.body = Just "> * <@alice:example.org> waves\n\nreply"
                }
            }
        }
  ((.text) <$> Matrix.eventToIncomingMessage event) @?= Just "reply"

testMatrixReplyRelationPreservesOrdinaryLeadingQuote :: IO ()
testMatrixReplyRelationPreservesOrdinaryLeadingQuote = do
  let event = matrixReplyRoomEvent
        { Matrix.event = matrixReplyRoomEvent.event
            { Matrix.content = matrixReplyRoomEvent.event.content
                { Matrix.body = Just "> an ordinary quote\n\nreply"
                }
            }
        }
  ((.text) <$> Matrix.eventToIncomingMessage event) @?= Just "> an ordinary quote\n\nreply"

testMatrixQuoteWithoutReplyRelationIsPreserved :: IO ()
testMatrixQuoteWithoutReplyRelationIsPreserved = do
  let event = matrixRoomEvent
        { Matrix.event = matrixRoomEvent.event
            { Matrix.content = matrixRoomEvent.event.content
                { Matrix.body = Just "> <@alice:example.org> quoted\n\nreply"
                }
            }
        }
  ((.text) <$> Matrix.eventToIncomingMessage event) @?= Just "> <@alice:example.org> quoted\n\nreply"

testMatrixEditEventConvertsToIncomingMessage :: IO ()
testMatrixEditEventConvertsToIncomingMessage = do
  let incoming = Matrix.eventToIncomingMessage matrixEditRoomEvent
  ((.messageId) <$> incoming) @?= Just (Just (textMessageId "$event:example.org"))
  ((.text) <$> incoming) @?= Just "hello"

testMatrixIncompleteStreamEventIsIgnored :: IO ()
testMatrixIncompleteStreamEventIsIgnored =
  assertBool
    "Matrix stream events should not trigger handlers before completion"
    (isNothing (Matrix.eventToIncomingMessage matrixIncompleteStreamRoomEvent))

testMatrixSuperuserIsMarkedInDigest :: IO ()
testMatrixSuperuserIsMarkedInDigest = do
  let incoming = fromMaybe (error "expected incoming Matrix message") $
        Matrix.eventToIncomingMessageWith matrixMentionConfig matrixMentionRoomEvent
  incoming.digest.chatIsAllowed @?= True
  incoming.digest.senderIsAllowed @?= True
  incoming.digest.senderIsSuperuser @?= True
  incoming.digest.mentionsBot @?= True

testMatrixBotMentionUsesMentionsFieldOnly :: IO ()
testMatrixBotMentionUsesMentionsFieldOnly = do
  let incoming = fromMaybe (error "expected incoming Matrix message") $
        Matrix.eventToIncomingMessageWith matrixMentionConfig matrixTextOnlyMentionRoomEvent
  incoming.digest.mentionsBot @?= False

testDiscordMessageConvertsToIncomingMessage :: IO ()
testDiscordMessageConvertsToIncomingMessage = do
  let incoming = Discord.eventToIncomingMessage discordMessage
  ((.platform) <$> incoming) @?= Just PlatformDiscord
  ((.kind) <$> incoming) @?= Just ChatGroup
  ((.chatAliases) <$> incoming) @?= Just ["90001", "80001"]
  ((.senderId) <$> incoming) @?= Just (Just "10001")
  ((.senderUsername) <$> incoming) @?= Just (Just "alice")
  ((.messageId) <$> incoming) @?= Just (Just (textMessageId "70001"))
  ((.replyToMessageId) <$> incoming) @?= Just (Just (textMessageId "60001"))
  ((.mentions) <$> incoming) @?= Just ["424242"]
  ((.imageUrls) <$> incoming) @?= Just ["https://cdn.discordapp.com/image.png"]
  ((.text) <$> incoming) @?= Just "hello <@424242>"

testDiscordDeleteConvertsToDeletedMessage :: IO ()
testDiscordDeleteConvertsToDeletedMessage = do
  let deleted = Discord.DeletedMessage
        { Discord.id = "123456789012345678"
        , Discord.channelId = "987654321098765432"
        , Discord.guildId = Just "111111111111111111"
        , Discord.raw = Aeson.Null
        }
      incoming = Discord.deletedEventToIncomingMessageWith discordConfig deleted
  incoming.eventKind @?= IncomingMessageDeleted
  incoming.messageId @?= Just (textMessageId "123456789012345678")
  incoming.kind @?= ChatGroup

testDiscordSelfMessageIsIgnored :: IO ()
testDiscordSelfMessageIsIgnored =
  assertBool
    "Discord messages sent by the bot itself are ignored"
    (isNothing (Discord.eventToIncomingMessageWith discordConfig discordMessage{Discord.author = discordUser "424242" "krkr" True}))

testDiscordSuperuserAndBotMentionAreMarked :: IO ()
testDiscordSuperuserAndBotMentionAreMarked = do
  let incoming = fromMaybe (error "expected incoming Discord message") $
        Discord.eventToIncomingMessageWith discordConfig discordMessage
  incoming.digest.chatIsAllowed @?= True
  incoming.digest.senderIsAllowed @?= True
  incoming.digest.senderIsSuperuser @?= True
  incoming.digest.mentionsBot @?= True

testDiscordCommonMarkExtensionsRenderDiscordMarkdown :: IO ()
testDiscordCommonMarkExtensionsRenderDiscordMarkdown = do
  Discord.formatDiscordMarkdown "**hi** and `code`" @?= "**hi** and `code`"
  Discord.formatDiscordMarkdown "~~old~~ and [site](https://example.test/a)" @?= "~~old~~ and [site](https://example.test/a)"
  Discord.formatDiscordMarkdown "- [ ] todo\n- [x] **done**" @?= "- [ ] todo\n- [x] **done**"
  Discord.formatDiscordMarkdown "Use $x^2$ and $$y$$" @?= "Use `x^2` and ```\ny\n```"
  Discord.formatDiscordMarkdown "| a | b |\n| - | - |\n| 1 | 2 |" @?= "```\na | b\n1 | 2\n```"

testDiscordAvatarValueIncludesAvatarUrl :: IO ()
testDiscordAvatarValueIncludesAvatarUrl = do
  let customAvatar = (discordUser "10001" "alice" False){Discord.avatar = Just "hash"}
  avatarUrl (fromMaybe (error "expected custom avatar") (Discord.discordUserAvatarValue customAvatar))
    @?= Just "https://cdn.discordapp.com/avatars/10001/hash.png?size=512"

  let defaultAvatar = (discordUser "10001" "alice" False){Discord.avatar = Nothing}
  avatarUrl (fromMaybe (error "expected default avatar") (Discord.discordUserAvatarValue defaultAvatar))
    @?= Just "https://cdn.discordapp.com/embed/avatars/0.png"

testDiscordImageContextIncludesEmbedsAndImageLinks :: IO ()
testDiscordImageContextIncludesEmbedsAndImageLinks = do
  let message = (discordMessageNoReference "70002")
        { Discord.content = "look https://example.test/generated.webp?size=512"
        , Discord.attachments =
            [ Discord.Attachment
                { Discord.id = "2"
                , Discord.filename = "photo.png"
                , Discord.url = "https://cdn.discordapp.com/attachment-without-content-type"
                , Discord.contentType = Nothing
                }
            ]
        , Discord.embeds =
            [ Discord.Embed
                { Discord.image = Just (Discord.EmbedImage "https://example.test/embed.png")
                , Discord.thumbnail = Just (Discord.EmbedImage "https://example.test/thumb.jpg")
                }
            ]
        }
      incoming = fromMaybe (error "expected incoming Discord message") (Discord.eventToIncomingMessage message)
  incoming.imageUrls @?=
    [ "https://cdn.discordapp.com/attachment-without-content-type"
    , "https://example.test/embed.png"
    , "https://example.test/thumb.jpg"
    , "https://example.test/generated.webp?size=512"
    ]

testDiscordDocumentAttachmentBecomesMessageFile :: IO ()
testDiscordDocumentAttachmentBecomesMessageFile = do
  let message = (discordMessageNoReference "70003")
        { Discord.content = "document"
        , Discord.attachments =
            [ Discord.Attachment
                { Discord.id = "3"
                , Discord.filename = "notes.txt"
                , Discord.url = "https://cdn.discordapp.com/notes.txt"
                , Discord.contentType = Just "text/plain"
                }
            ]
        }
      incoming = fromMaybe (error "expected incoming Discord message") (Discord.eventToIncomingMessage message)
  incoming.files @?= [MessageFile{name = "notes.txt", ref = "https://cdn.discordapp.com/notes.txt"}]

testDiscordAudioAttachmentBecomesMessageFile :: IO ()
testDiscordAudioAttachmentBecomesMessageFile = do
  let message = (discordMessageNoReference "70004")
        { Discord.attachments =
            [ Discord.Attachment
                { Discord.id = "4"
                , Discord.filename = "voice.ogg"
                , Discord.url = "https://cdn.discordapp.com/voice.ogg"
                , Discord.contentType = Just "audio/ogg"
                }
            ]
        }
      incoming = fromMaybe (error "expected incoming Discord message") (Discord.eventToIncomingMessage message)
  incoming.files @?= [MessageFile{name = "voice.ogg", ref = "https://cdn.discordapp.com/voice.ogg"}]

avatarUrl :: Aeson.Value -> Maybe Text
avatarUrl =
  AesonTypes.parseMaybe $
    Aeson.withObject "avatar value" (Aeson..: "avatar_url")

qqMessageEvent :: Integer -> QQ.Event
qqMessageEvent userId =
  QQ.Event
    { time = Just 1
    , selfId = Just qqBotUserId
    , postType = "message"
    , messageType = Just "group"
    , subType = Just "normal"
    , messageId = Just 80001
    , userId = Just userId
    , groupId = Just 90001
    , message = Just (Aeson.String "hello")
    , rawMessage = Just "hello"
    , sender = Nothing
    , rawEvent = Aeson.Null
    }

matrixBridgeMessage :: Text -> Text -> IncomingMessage
matrixBridgeMessage senderId text =
  IncomingMessage
    { eventKind = IncomingMessageCreated
    , platform = PlatformMatrix
    , kind = ChatGroup
    , chatId = Just 12345
    , chatAliases = [FMBridge.fmMatrixRoomId]
    , digest = emptyMessageDigest
    , senderId = Just senderId
    , senderUsername = Just senderId
    , messageId = Just "$matrix-event"
    , replyToMessageId = Nothing
    , mentions = []
    , mentionUsernames = []
    , imageUrls = []
    , files = []
    , text = text
    , raw = Aeson.Null
    }

qqRequestEvent :: Text -> Maybe Text -> QQ.Event
qqRequestEvent requestType requestSubType =
  (qqMessageEvent 10001)
    { QQ.postType = "request"
    , QQ.subType = requestSubType
    , QQ.rawEvent = Aeson.object
        ( [ "request_type" Aeson..= requestType
          , "flag" Aeson..= ("request-flag" :: Text)
          ]
            <> maybe [] (pure . ("sub_type" Aeson..=)) requestSubType
        )
    }

qqPokeEvent :: Integer -> QQ.Event
qqPokeEvent targetId =
  (qqMessageEvent 10001)
    { QQ.postType = "notice"
    , QQ.messageType = Nothing
    , QQ.subType = Just "poke"
    , QQ.messageId = Nothing
    , QQ.message = Nothing
    , QQ.rawMessage = Nothing
    , QQ.rawEvent = Aeson.object
        [ "post_type" Aeson..= ("notice" :: Text)
        , "notice_type" Aeson..= ("notify" :: Text)
        , "sub_type" Aeson..= ("poke" :: Text)
        , "self_id" Aeson..= qqBotUserId
        , "user_id" Aeson..= (10001 :: Integer)
        , "target_id" Aeson..= targetId
        , "group_id" Aeson..= (90001 :: Integer)
        ]
    }

qqInvitationAction :: Text -> Maybe Text -> Aeson.Value
qqInvitationAction action requestSubType =
  Aeson.object
    [ "action" Aeson..= action
    , "params" Aeson..= Aeson.object
        ( [ "flag" Aeson..= ("request-flag" :: Text)
          , "approve" Aeson..= True
          ]
            <> maybe [] (pure . ("sub_type" Aeson..=)) requestSubType
        )
    ]

qqBotUserId :: Integer
qqBotUserId =
  424242

textSegment :: Text -> Aeson.Value
textSegment text =
  Aeson.object
    [ "type" Aeson..= ("text" :: Text)
    , "data" Aeson..= Aeson.object
        [ "text" Aeson..= text
        ]
    ]

imageSegment :: Text -> Aeson.Value
imageSegment url =
  Aeson.object
    [ "type" Aeson..= ("image" :: Text)
    , "data" Aeson..= Aeson.object
        [ "url" Aeson..= url
        ]
    ]

telegramUpdate :: Bool -> Telegram.Update
telegramUpdate fromBot =
  telegramUpdateWithMessage (telegramMessage fromBot)

telegramUpdateWithMessage :: Telegram.Message -> Telegram.Update
telegramUpdateWithMessage message =
  Telegram.Update
    { updateId = 1
    , message = Just message
    , editedMessage = Nothing
    , channelPost = Nothing
    , editedChannelPost = Nothing
    }

telegramMessage :: Bool -> Telegram.Message
telegramMessage fromBot =
  Telegram.Message
    { messageId = 80001
    , messageThreadId = Nothing
    , from = Just (telegramUser fromBot)
    , senderChat = Nothing
    , chat = telegramChat
    , replyToMessage = Nothing
    , text = Just "hello"
    , entities = Nothing
    , caption = Nothing
    , captionEntities = Nothing
    , photo = Nothing
    , document = Nothing
    , audio = Nothing
    , voice = Nothing
    }

telegramUser :: Bool -> Telegram.User
telegramUser fromBot =
  Telegram.User
    { id = 10001
    , isBot = fromBot
    , firstName = "Alice"
    , lastName = Nothing
    , username = Just "alice"
    }

telegramChat :: Telegram.Chat
telegramChat =
  Telegram.Chat
    { id = 90001
    , type_ = Telegram.ChatTypeGroup
    , title = Just "group"
    , username = Nothing
    , firstName = Nothing
    , lastName = Nothing
    }

privateTelegramMessage :: Telegram.Message
privateTelegramMessage =
  (telegramMessage False)
    { Telegram.chat = Telegram.Chat
        { id = 10001
        , type_ = Telegram.ChatTypePrivate
        , title = Nothing
        , username = Just "alice"
        , firstName = Just "Alice"
        , lastName = Nothing
        }
    }

matrixRoomEvent :: Matrix.RoomEvent
matrixRoomEvent =
  Matrix.RoomEvent
    { Matrix.roomId = "!room:example.org"
    , Matrix.roomIsDirect = False
    , Matrix.event = Matrix.Event
        { Matrix.type_ = "m.room.message"
        , Matrix.sender = "@alice:example.org"
        , Matrix.eventId = Just "$event:example.org"
        , Matrix.content = Matrix.EventContent
            { Matrix.msgtype = Just "m.text"
            , Matrix.body = Just "hello"
            , Matrix.mentions = []
            , Matrix.replyToEventId = Nothing
            }
        , Matrix.raw = Aeson.Null
        }
    }

matrixDirectRoomEvent :: Matrix.RoomEvent
matrixDirectRoomEvent =
  matrixRoomEvent{Matrix.roomIsDirect = True}

matrixImageRoomEvent :: Matrix.RoomEvent
matrixImageRoomEvent =
  matrixRoomEvent
    { Matrix.event = matrixRoomEvent.event
        { Matrix.content = matrixRoomEvent.event.content
            { Matrix.msgtype = Just "m.image"
            , Matrix.body = Just "image.png"
            }
        , Matrix.raw = matrixImageRawContent
            [ "msgtype" Aeson..= ("m.image" :: Text)
            , "body" Aeson..= ("image.png" :: Text)
            , "url" Aeson..= ("mxc://example.org/plain-image" :: Text)
            , "info" Aeson..= Aeson.object
                [ "mimetype" Aeson..= ("image/png" :: Text)
                ]
            ]
        }
    }

matrixEncryptedImageRoomEvent :: Matrix.RoomEvent
matrixEncryptedImageRoomEvent =
  matrixRoomEvent
    { Matrix.event = matrixRoomEvent.event
        { Matrix.content = matrixRoomEvent.event.content
            { Matrix.msgtype = Just "m.image"
            , Matrix.body = Just "image.png"
            }
        , Matrix.raw = matrixImageRawContent
            [ "msgtype" Aeson..= ("m.image" :: Text)
            , "body" Aeson..= ("image.png" :: Text)
            , "file" Aeson..= Aeson.object
                [ "url" Aeson..= ("mxc://example.org/encrypted-image" :: Text)
                , "key" Aeson..= Aeson.object
                    [ "alg" Aeson..= ("A256CTR" :: Text)
                    , "k" Aeson..= ("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" :: Text)
                    , "kty" Aeson..= ("oct" :: Text)
                    ]
                , "iv" Aeson..= ("AAAAAAAAAAAAAAAAAAAAAA" :: Text)
                , "hashes" Aeson..= Aeson.object
                    [ "sha256" Aeson..= ("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" :: Text)
                    ]
                ]
            , "info" Aeson..= Aeson.object
                [ "mimetype" Aeson..= ("image/png" :: Text)
                ]
            ]
        }
    }

matrixImageWithoutBodyRoomEvent :: Matrix.RoomEvent
matrixImageWithoutBodyRoomEvent =
  matrixRoomEvent
    { Matrix.event = matrixRoomEvent.event
        { Matrix.content = matrixRoomEvent.event.content
            { Matrix.msgtype = Just "m.image"
            , Matrix.body = Nothing
            }
        , Matrix.raw = matrixImageRawContent
            [ "msgtype" Aeson..= ("m.image" :: Text)
            , "url" Aeson..= ("mxc://example.org/bodyless-image" :: Text)
            , "info" Aeson..= Aeson.object
                [ "mimetype" Aeson..= ("image/png" :: Text)
                ]
            ]
        }
    }

matrixImageRawContent :: [AesonTypes.Pair] -> Aeson.Value
matrixImageRawContent content =
  Aeson.object ["content" Aeson..= Aeson.object content]

matrixAes256Ctr :: StrictByteString.ByteString -> StrictByteString.ByteString -> StrictByteString.ByteString -> StrictByteString.ByteString
matrixAes256Ctr key iv bytes =
  let cipher = CryptoError.throwCryptoError (CryptoCipher.cipherInit key :: CryptoError.CryptoFailable CryptoAES.AES256)
      initialIv = fromMaybe (error "invalid AES IV in Matrix test") (CryptoCipher.makeIV iv)
  in CryptoCipher.ctrCombine cipher initialIv bytes

matrixSha256 :: StrictByteString.ByteString -> StrictByteString.ByteString
matrixSha256 bytes =
  ByteArray.convert (CryptoHash.hash bytes :: CryptoHash.Digest CryptoHash.SHA256)

matrixReplyRoomEvent :: Matrix.RoomEvent
matrixReplyRoomEvent =
  matrixRoomEvent
    { Matrix.event = matrixRoomEvent.event
        { Matrix.content = matrixRoomEvent.event.content
            { Matrix.replyToEventId = Just "$parent:example.org"
            }
        }
    }

matrixEditRoomEvent :: Matrix.RoomEvent
matrixEditRoomEvent =
  matrixRoomEvent
    { Matrix.event = matrixRoomEvent.event
        { Matrix.raw = Aeson.object
            [ "content" Aeson..= Aeson.object
                [ "m.relates_to" Aeson..= Aeson.object
                    [ "rel_type" Aeson..= ("m.replace" :: Text)
                    , "event_id" Aeson..= ("$event:example.org" :: Text)
                    ]
                ]
            ]
        }
    }

matrixIncompleteStreamRoomEvent :: Matrix.RoomEvent
matrixIncompleteStreamRoomEvent =
  matrixRoomEvent
    { Matrix.event = matrixRoomEvent.event
        { Matrix.raw = Aeson.object
            [ "content" Aeson..= Aeson.object
                [ "body" Aeson..= ("streaming" :: Text)
                , "msgtype" Aeson..= ("m.text" :: Text)
                , "com.pfeiwu.ai.stream" Aeson..= Aeson.object
                    [ "complete" Aeson..= False
                    ]
                ]
            ]
        }
    }

matrixMentionRoomEvent :: Matrix.RoomEvent
matrixMentionRoomEvent =
  Matrix.RoomEvent
    { Matrix.roomId = "!room:example.org"
    , Matrix.roomIsDirect = False
    , Matrix.event = Matrix.Event
        { Matrix.content = Matrix.EventContent
            { Matrix.msgtype = Just "m.text"
            , Matrix.body = Just "hello @bot:example.org"
            , Matrix.mentions = ["@bot:example.org"]
            , Matrix.replyToEventId = Nothing
            }
        , Matrix.type_ = "m.room.message"
        , Matrix.sender = "@alice:example.org"
        , Matrix.eventId = Just "$event:example.org"
        , Matrix.raw = Aeson.Null
        }
    }

matrixTextOnlyMentionRoomEvent :: Matrix.RoomEvent
matrixTextOnlyMentionRoomEvent =
  matrixMentionRoomEvent
    { Matrix.event = matrixMentionRoomEvent.event
        { Matrix.content = matrixMentionRoomEvent.event.content
            { Matrix.mentions = []
            }
        }
    }

matrixMentionConfig :: Matrix.Config
matrixMentionConfig =
  Matrix.Config
    { Matrix.homeserver = "https://matrix.example.org"
    , Matrix.loginUser = Nothing
    , Matrix.loginPassword = Nothing
    , Matrix.deviceId = Nothing
    , Matrix.directRooms = []
    , Matrix.userId = Just "@bot:example.org"
    , Matrix.allowedRooms = ["!room:example.org"]
    , Matrix.superusers = ["@alice:example.org"]
    }

discordConfig :: Discord.Config
discordConfig =
  Discord.Config
    { Discord.botToken = ""
    , Discord.botId = Just "424242"
    , Discord.applicationId = Nothing
    , Discord.allowedGuilds = [80001]
    , Discord.allowedChannels = []
    , Discord.allowedUsers = []
    , Discord.superusers = ["10001"]
    , Discord.gatewayHost = "gateway.discord.gg"
    , Discord.gatewayPath = "/?v=10&encoding=json"
    }

discordMessage :: Discord.Message
discordMessage =
  Discord.Message
    { Discord.id = "70001"
    , Discord.channelId = "90001"
    , Discord.guildId = Just "80001"
    , Discord.author = discordUser "10001" "alice" False
    , Discord.member = Nothing
    , Discord.content = "hello <@424242>"
    , Discord.attachments =
        [ Discord.Attachment
            { Discord.id = "1"
            , Discord.filename = "image.png"
            , Discord.url = "https://cdn.discordapp.com/image.png"
            , Discord.contentType = Just "image/png"
            }
        ]
    , Discord.embeds = []
    , Discord.mentions = [discordUser "424242" "krkr" True]
    , Discord.referencedMessage = Just (discordReferencedMessage "60001")
    , Discord.messageReference = Nothing
    , Discord.raw = Aeson.object ["guild_id" Aeson..= ("80001" :: Text)]
    }

discordReferencedMessage :: Text -> Discord.Message
discordReferencedMessage messageId =
  (discordMessageNoReference messageId)
    { Discord.content = "quoted"
    , Discord.attachments = []
    , Discord.embeds = []
    , Discord.mentions = []
    }

discordMessageNoReference :: Text -> Discord.Message
discordMessageNoReference messageId =
  Discord.Message
    { Discord.id = messageId
    , Discord.channelId = "90001"
    , Discord.guildId = Just "80001"
    , Discord.author = discordUser "20001" "bob" False
    , Discord.member = Nothing
    , Discord.content = ""
    , Discord.attachments = []
    , Discord.embeds = []
    , Discord.mentions = []
    , Discord.referencedMessage = Nothing
    , Discord.messageReference = Nothing
    , Discord.raw = Aeson.object ["guild_id" Aeson..= ("80001" :: Text)]
    }

discordUser :: Text -> Text -> Bool -> Discord.User
discordUser userId username fromBot =
  Discord.User
    { Discord.id = userId
    , Discord.username = Just username
    , Discord.globalName = Nothing
    , Discord.bot = fromBot
    , Discord.avatar = Nothing
    }
