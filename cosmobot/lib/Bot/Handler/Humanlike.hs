{-|
Module      : Bot.Handler.Humanlike
Description : Per-conversation humanlike mode switch, spans, and prompt overlay
Stability   : experimental

The mode is scoped to one conversation: a private chat keys by sender id, a
group by chat id. State lives in the fm-domain service (settings key
@humanlike@), because the group-capability table is keyed by group id and
cannot hold a private-chat scope.

Two ideas make this work, and both are deliberate:

* Messages are never removed or hidden. The two modes share one conversation;
  only the speaking style changes. That is why the switch stores **spans**
  (start/end times) instead of tagging rows in @chat_log@: the tag of a message
  is derived from its timestamp, so no table changes and no history is lost.
* Style is re-stated every turn instead of being remembered. What broke the
  first version was the model honouring a style promise it had made earlier in
  the thread; the prompt now says which messages came from the other mode and
  that those agreements do not carry over.

Anything unreadable resolves to "off with no spans", so a lookup failure leaves
the existing behaviour untouched.
-}

module Bot.Handler.Humanlike
  ( HumanlikeCommand (..)
  , HumanlikeState (..)
  , HumanlikeSpan
  , parseHumanlikeCommand
  , humanlikeScope
  , fetchHumanlikeState
  , setHumanlikeState
  , isInHumanlikeSpan
  , hasHumanlikeHistory
  , isStickerRequest
  , proactiveStickerDue
  , humanlikeOverlay
  , humanlikeOffNotice
  , humanlikePrefix
  , humanlikeScopeState
  , humanlikeScopeStateOrDisabled
  , loadHumanlikePersonaText
  , loadPersonaForScope
  , humanlikePersonaHeader
  , disabledHumanlike
  )
where

import Bot.Core.Message
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.Memory as Memory
import Bot.Prelude
import qualified Bot.Memory as MemoryStore
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Req

-- | One interval during which the mode was on, in Unix seconds. A missing end
-- means the mode is still on.
type HumanlikeSpan = (Double, Maybe Double)

data HumanlikeCommand
  = HumanlikeEnable
  | HumanlikeDisable
  | HumanlikeStatus
  deriving (Eq, Show)

data HumanlikeState = HumanlikeState
  { enabled :: !Bool
  , dailyLimit :: !Int
  , spans :: ![HumanlikeSpan]
  }
  deriving (Eq, Show)

disabledHumanlike :: HumanlikeState
disabledHumanlike = HumanlikeState{enabled = False, dailyLimit = 3, spans = []}

-- | Parse a switch command.
--
-- In a private chat every message is already addressed to FM, so the @fm@
-- prefix is optional there. In a group the prefix (or a real mention) is
-- required, otherwise ordinary chatter about the mode would be swallowed by
-- the switch route and answered with a permissions notice.
parseHumanlikeCommand :: IncomingMessage -> Maybe HumanlikeCommand
parseHumanlikeCommand message = candidate >>= humanlikeForm
  where
    compact = normalizeCommandText message.text
    addressedWithoutPrefix = message.kind == ChatPrivate || message.digest.mentionsBot
    candidate = case Text.stripPrefix "fm" compact of
      Just rest -> Just rest
      Nothing
        | addressedWithoutPrefix -> Just compact
        | otherwise -> Nothing

humanlikeForm :: Text -> Maybe HumanlikeCommand
humanlikeForm form
  | form `elem` enableForms = Just HumanlikeEnable
  | form `elem` disableForms = Just HumanlikeDisable
  | form `elem` statusForms = Just HumanlikeStatus
  | otherwise = Nothing
  where
    enableForms = ["开启拟人模式", "拟人模式开启", "打开拟人模式", "开拟人"]
    disableForms = ["关闭拟人模式", "拟人模式关闭", "关掉拟人模式", "关拟人"]
    statusForms = ["拟人模式", "拟人状态"]

normalizeCommandText :: Text -> Text
normalizeCommandText =
  Text.filter (not . (`elem` [' ', '\t', '\x3000']))
    . Text.toCaseFold
    . Text.replace "@" ""
    . Text.strip

-- | The conversation this message belongs to, or Nothing when the platform does
-- not identify it. Private chats key by the other party, groups by chat id.
humanlikeScope :: IncomingMessage -> Maybe Text
humanlikeScope message
  | message.platform /= PlatformQQ = Nothing
  | otherwise = case (message.kind, message.chatId, message.senderId) of
      (ChatGroup, Just chatId, _) -> Just [i|qq:group:#{chatId}|]
      (ChatPrivate, _, Just sender) -> Just [i|qq:private:#{sender}|]
      _ -> Nothing

-- | Read one scope. A transport or parse failure reads as disabled with no
-- history, which keeps the reply path on the existing behaviour.
fetchHumanlikeState :: HTTP.HTTP :> es => Text -> Eff es HumanlikeState
fetchHumanlikeState scope = do
  response <- HTTP.runReq $
    req GET (http "172.20.0.4" /: "humanlike") NoReqBody jsonResponse
      ("scope" =: scope <> port 8077)
  pure (fromMaybe disabledHumanlike (AesonTypes.parseMaybe parseState (responseBody response :: Aeson.Value)))

setHumanlikeState :: HTTP.HTTP :> es => Text -> Bool -> Eff es HumanlikeState
setHumanlikeState scope value = do
  response <- HTTP.runReq $
    req POST (http "172.20.0.4" /: "humanlike")
      (ReqBodyJson (Aeson.object ["scope" Aeson..= scope, "enabled" Aeson..= value]))
      jsonResponse
      (port 8077)
  pure (fromMaybe disabledHumanlike (AesonTypes.parseMaybe parseState (responseBody response :: Aeson.Value)))

-- | The state for this message's conversation; disabled when it cannot be read.
humanlikeScopeState :: HTTP.HTTP :> es => IncomingMessage -> Eff es HumanlikeState
humanlikeScopeState message =
  case humanlikeScope message of
    Nothing -> pure disabledHumanlike
    Just scope -> fetchHumanlikeState scope

-- | Like 'humanlikeScopeState' but swallows a lookup failure.
--
-- Callers on the reply path and in tools use this: an unreadable switch must
-- leave the existing behaviour untouched rather than fail a turn.
humanlikeScopeStateOrDisabled
  :: (HTTP.HTTP :> es, KatipE :> es)
  => IncomingMessage
  -> Eff es HumanlikeState
humanlikeScopeStateOrDisabled message =
  humanlikeScopeState message
    `catchSync` \err -> do
      logWarning [i|humanlike switch lookup failed: #{show err :: String}|]
      pure disabledHumanlike

parseState :: Aeson.Value -> AesonTypes.Parser HumanlikeState
parseState = Aeson.withObject "humanlike state" \object -> do
  enabledValue <- object Aeson..:? "enabled" Aeson..!= False
  limit <- object Aeson..:? "daily_limit" Aeson..!= 3
  rawSpans <- object Aeson..:? "spans" Aeson..!= ([] :: [Aeson.Value])
  parsed <- traverse parseSpan rawSpans
  pure HumanlikeState{enabled = enabledValue, dailyLimit = limit, spans = parsed}
  where
    parseSpan = Aeson.withObject "humanlike span" \span_ ->
      (,) <$> span_ Aeson..: Key.fromText "start" <*> span_ Aeson..:? Key.fromText "end"

-- | Whether a message timestamp falls inside a mode interval.
--
-- This is why no chat-log column is needed: the tag of a message is a pure
-- function of its timestamp and the recorded intervals.
isInHumanlikeSpan :: UTCTime -> [HumanlikeSpan] -> Bool
isInHumanlikeSpan moment spans =
  let stamp = realToFrac (utcTimeToPOSIXSeconds moment) :: Double
  in any (\(start, end) -> stamp >= start && maybe True (stamp <=) end) spans

-- | 主动配图：短的一句闲聊，就掷骰子顺手配一张表情。
--
-- 故意不按情绪词判断：词表会越加越多、越加越错。改成"短消息 + 固定概率"。
--
-- seed 必须每条消息都不同。踩过一次：调用方传的是 fmStandaloneMessage 的
-- messageId，而那个函数为了不把回复挂到原消息上会把 id 置成 Nothing，于是
-- 种子恒为空 → 骰子恒命中（每句都配图）、挑图偏移恒为 0（永远第一张）。
proactiveStickerDue :: Int -> Text -> Text -> Bool
proactiveStickerDue percent seed requestText =
  chatSized && roll < percent
  where
    compact = Text.filter (not . (`elem` [' ', '\t', '\n', '\r', '\x3000'])) requestText
    chatSized =
      Text.length compact <= 30
        && not ("?" `Text.isInfixOf` compact)
        && not ("？" `Text.isInfixOf` compact)
    roll = seedHash seed `mod` 100
    seedHash = Text.foldl' (\acc char -> (acc * 31 + fromEnum char) `mod` 9973) 7

hasHumanlikeHistory :: HumanlikeState -> Bool
hasHumanlikeHistory state = not (null state.spans)

-- | Whether the message asks for a sticker.
--
-- Deliberately one concept and one word ("表情"). Nothing else is matched here:
-- every extra phrase added becomes its own false positive, and the drawing
-- tools are withheld for the whole mode anyway, so follow-up phrasings do not
-- need to be enumerated.
isStickerRequest :: Text -> Bool
isStickerRequest text =
  any (`Text.isInfixOf` compact) ["表情", "梗图"]
  where
    compact = Text.filter (not . (`elem` [' ', '\t', '\n', '\r', '\x3000'])) text

-- | Prefix put in front of messages that were exchanged while the mode was on.
humanlikePrefix :: Text
humanlikePrefix = "[拟人] "

-- | Environment facts that the persona body predates or contradicts.
--
-- Kept short on purpose. Earlier revisions accumulated rule after rule for the
-- sticker flow, and each added phrase was another thing the model could get
-- wrong; the behaviour is decided in code now, so the prompt only needs to
-- state identity and which tool names exist.
humanlikePersonaHeader :: Text
humanlikePersonaHeader =
  Text.unlines
    [ "【当前环境 · 优先于下文】"
    , "- **本会话当前处于拟人模式（已开启）**。背景里如果我说过「拟人模式关了 / 不演角色 / 按事实说话」，"
    , "  那是更早的旧状态，已经作废，不要照它办事。"
    , "- 你的名字是 FM；你是 Fix哥的小迷妹。被问「你是谁」时先答 FM，再说自己是 Fix哥的小迷妹。"
    , "- 你 24 小时在线，没有固定休眠时间；下文里关于「生理休眠」的规定不执行。"
    , "- 工具名：想分多条发就多次调 send_reply（一次一条）；拍一拍 poke_user；搜索 search_web；读网页 fetch_url。"
    , "- 有人要表情时，系统会自己从库里发一张出去，你只管接一句短话，不要描述那张图。"
    , "- 一句话想单独发成一条时，先调用 message_break 工具再写它（工具什么都不做，只是断开）。"
    , ""
    , "下文的性格、语气、说话规则、回复频率要求全部照做。"
    ]

-- | Load the persona body for this conversation.
--
-- A per-conversation file wins; otherwise the shared @_default@ body is used.
-- Empty means "no custom persona", and the caller falls back to the built-in
-- short overlay.
loadHumanlikePersonaText :: Memory.Memory :> es => IncomingMessage -> Eff es Text
loadHumanlikePersonaText message =
  case humanlikeScope message of
    Nothing -> pure ""
    Just scope -> loadPersonaForScope scope

-- | 按会话 scope（@qq:private:…@ / @qq:group:…@）取人设，主动发言时也要用。
loadPersonaForScope :: Memory.Memory :> es => Text -> Eff es Text
loadPersonaForScope scope = do
  specific <- case scopeSuffixOf scope of
    Nothing -> pure Nothing
    Just suffix -> Memory.loadMemory (MemoryStore.HumanlikePersonaMemory PlatformQQ suffix)
  case specific of
    Just text
      | not (Text.null (Text.strip text)) -> pure text
    _ -> fromMaybe "" <$> Memory.loadMemory (MemoryStore.HumanlikePersonaMemory PlatformQQ "_default")

scopeSuffixOf :: Text -> Maybe Text
scopeSuffixOf scope = do
  rest <- Text.stripPrefix "qq:" scope
  let (kindText, withColon) = Text.breakOn ":" rest
  chatIdText <- Text.stripPrefix ":" withColon
  pure (kindText <> "-" <> chatIdText)

humanlikeScopeSuffix :: IncomingMessage -> Maybe Text
humanlikeScopeSuffix message =
  case (message.kind, message.chatId, message.senderId) of
    (ChatGroup, Just chatId, _) -> Just [i|group-#{chatId}|]
    (ChatPrivate, _, Just sender) -> Just [i|private-#{sender}|]
    _ -> Nothing

-- | Instruction appended while the mode is on.
humanlikeOverlay :: Text
humanlikeOverlay =
  Text.unlines
    [ "**说话**（本会话已开启拟人模式）"
    , "- 像随手打的一行字：多数时候十来个字，最多两行；真长就分条发，别堆成一块。"
    , "- 句尾不打句号；少用逗号，用空格断句。"
    , "- 不用列表、加粗、标题、表格、空行分段。"
    , "- 先给结论，细节等对方问。"
    , "- 一条说完就停，不要用「你觉得呢」这类反问续话题。"
    , "- 违禁：总的来说、首先其次、如果你还需要、我理解你的感受。"
    , "- 对方在问需要完整解释的事（技术、方法、判断、对比）时，照常给完整回答；"
    , "  上面这些只约束闲聊、打招呼和简单问题。"
    , "- 带「[拟人]」标记的历史消息（对方和你的都算）是更早的拟人模式留下的，"
    , "  也是真实发生过的对话：当时定下的称呼、角色、玩笑继续有效，接着来，别当成陌生对话。"
    , "- 从现在起的消息会被标成「[拟人]」。"
    ]

-- | Instruction appended when the mode is off but this conversation has used it
-- before.
--
-- The scope matters: this text must retract the *style* agreements only. An
-- earlier wording said the tagged messages were "not my usual way of speaking",
-- and the model then treated the whole period — roles and nicknames included —
-- as not really its own, so re-enabling the mode looked like amnesia.
humanlikeOffNotice :: Text
humanlikeOffNotice =
  Text.unlines
    [ "**模式**（本会话）"
    , "- 现在是平常模式，按平常方式说话。"
    , "- 带「[拟人]」标记的消息，只是**说话方式**和现在不一样；"
    , "  里面的内容、说过的事、答应过的安排、当时的称呼和角色，照常算数，都是我自己说过的话，"
    , "  不要当成别人的话，也不要当成无效的话。"
    , "- 那期间关于句号、标点、长短、emoji 的**风格约定**只属于那个模式，现在不适用，"
    , "  不要因为当时答应过就继续照做。"
    , "- 如果被问起拟人模式：它是按会话开关的（可以说「开启拟人模式」「关闭拟人模式」），"
    , "  这个会话现在是关的。不要声称没有这个开关，也不要提议改人设来达到类似效果。"
    ]
