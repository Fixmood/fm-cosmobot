{-# LANGUAGE OverloadedLabels #-}
{-|
Module      : Bot.Handler.Poke
Description : Poke back when someone pokes FM (QQ 戳一戳)
Stability   : experimental
-}

module Bot.Handler.Poke
  ( pokeHandlers
  )
where

import Bot.Core.Message
import Bot.Core.Route
import Bot.Prelude
import qualified Bot.Effect.Chat as Chat
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import Effectful.FileSystem (FileSystem, doesFileExist)

-- | QQ 驱动把「被戳」变成一条普通消息（`isPokeEvent`），正文以这个前缀开头。
-- 用前缀识别，是因为这条消息**已经走完驱动**、带齐了 kind/chatId/senderId，
-- 路由里直接就能拿到回戳需要的一切。
pokeEventPrefix :: Text
pokeEventPrefix = "[QQ戳一戳事件]"

-- | 回戳的冷却状态。同聊天 5 秒内只回一次 —— 防的是**两个机器人互相戳**成死循环；
-- 人手动戳本来就快不到哪儿去。
pokeStatePath :: FilePath
pokeStatePath = "/data/huma/poke-state.json"

pokeCooldownSeconds :: Double
pokeCooldownSeconds = 5

-- | 回戳时说的话。**不编造情绪**（人设【骨架】禁止「为显得有性格而编造感情」），
-- 只做反应。选哪条由消息 id 决定：无状态、可复现、连续几次不重样。
pokeLines :: [Text]
pokeLines =
  [ "戳回来了。"
  , "你戳我一下，我戳你一下，扯平。"
  , "嗯，戳到了。"
  , "别光戳，有事直接说。"
  , "手别停啊。"
  , "回戳。"
  , "我在，戳什么。"
  , "又来？"
  , "戳我也不会掉血。"
  , "行，收到了。"
  ]

-- | 有人戳 FM 就回戳一下。
--
-- 为什么做成 route 而不是交给模型：回戳是个**反射**，不是需要推理的事。
-- 走模型要 3 秒（实测模型占一次运行 94.5% 的时间），反射该是瞬时的。
pokeHandlers
  :: (Chat.Chat :> es, FileSystem :> es, IOE :> es)
  => [RouteHandler es]
pokeHandlers =
  [pokeBackRoute]

pokeBackRoute
  :: (Chat.Chat :> es, FileSystem :> es, IOE :> es)
  => RouteHandler es
pokeBackRoute =
  withHelp (RouteHelp "被戳" "有人戳 FM 时回戳一下，并说一句。") $
    stopOn pokeEventFilter \message _ ->
      case message.senderId of
        Nothing -> pure ()
        Just poker -> do
          cooled <- claimPokeSlot message
          when cooled do
            Chat.pokeUser message poker >>= \case
              -- 回戳失败就**什么都不说**：戳一戳本来就是无声的动作，
              -- 失败时冒一句「戳失败」只会更奇怪。
              Left _ -> pure ()
              Right () ->
                void $ Chat.replyTo message{messageId = Nothing} (pickPokeLine message)

-- | 由消息 id 选一句：无状态、可复现、连续几次不重样。
pickPokeLine :: IncomingMessage -> Text
pickPokeLine message =
  case pokeLines of
    [] -> "戳回来了。"
    candidates ->
      let key = maybe "" messageIdText message.messageId
          n = abs (Text.foldl' (\ acc c -> acc * 31 + fromEnum c) 7 key) `mod` length candidates
       in fromMaybe "戳回来了。" (listToMaybe (drop n candidates))

-- | 冷却：同一个聊天里，pokeCooldownSeconds 内只回戳一次。
claimPokeSlot :: (FileSystem :> es, IOE :> es) => IncomingMessage -> Eff es Bool
claimPokeSlot message = do
  now <- liftIO getCurrentTime
  let nowSeconds = realToFrac (utcTimeToPOSIXSeconds now) :: Double
      chatKey = maybe "-" (Text.pack . show) message.chatId
  existing <- readPokeState
  case Map.lookup chatKey existing of
    Just lastAt | nowSeconds - lastAt < pokeCooldownSeconds -> pure False
    _ -> do
      let updated = Map.insert chatKey nowSeconds existing
      FileSystemByteString.writeFile pokeStatePath
        (LazyByteString.toStrict (Aeson.encode updated))
      pure True

readPokeState :: (FileSystem :> es, IOE :> es) => Eff es (Map.Map Text Double)
readPokeState = do
  exists <- doesFileExist pokeStatePath
  if not exists
    then pure Map.empty
    else do
      raw <- FileSystemByteString.readFile pokeStatePath
      pure (fromMaybe Map.empty (Aeson.decodeStrict' raw))

-- | 匹配被戳事件。
pokeEventFilter :: MessageFilter ()
pokeEventFilter =
  MessageFilter \message ->
    if pokeEventPrefix `Text.isPrefixOf` Text.strip message.text
      then Just ()
      else Nothing
