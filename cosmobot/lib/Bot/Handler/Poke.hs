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

-- | QQ 驱动把「被戳」变成一条普通消息（`isPokeEvent`），正文以这个前缀开头，
-- 并且带一句提示让模型自然回应。
pokeEventPrefix :: Text
pokeEventPrefix = "[QQ戳一戳事件]"

-- | 回戳的冷却状态。同聊天 5 秒内只回戳一次 —— 防的是**两个机器人互相戳**成死循环；
-- 人手动戳本来就快不到哪儿去。
pokeStatePath :: FilePath
pokeStatePath = "/data/huma/poke-state.json"

pokeCooldownSeconds :: Double
pokeCooldownSeconds = 5

-- | 有人戳 FM 就回戳一下。
--
-- **只做「戳」这一件事，然后把消息放走**（返回 Skip）。这一点第一版做错了：
-- 我在这里把消息吞掉、改用固定台词，结果既丢了上下文，又只会重复同一句 ——
-- 戳事件**没有 message id**（日志里 `message=-`），所以我那个「按消息 id 选句」
-- 永远选中同一条。
--
-- 现在的分工：**回戳是反射**（route 里瞬时完成，不走模型的 3 秒），
-- **说什么交给它自己**（戳事件本来就带着「自然回应、不要复用固定句式」的提示）。
--
-- 回戳做成 route 而不是工具，是因为反射不该等模型；说话该等，因为它要说人话。
pokeHandlers
  :: (Chat.Chat :> es, FileSystem :> es, IOE :> es)
  => [RouteHandler es]
pokeHandlers =
  [pokeBackRoute]

pokeBackRoute
  :: (Chat.Chat :> es, FileSystem :> es, IOE :> es)
  => RouteHandler es
pokeBackRoute =
  Route
    { help = Just (RouteHelp "被戳" "有人戳 FM 时回戳一下；说什么由它自己决定。")
    , helpVisible = const True
    , decide = \message -> do
        when (shouldPokeBack message && message.digest.senderIsAllowed) do
          case message.senderId of
            Nothing -> pure ()
            Just poker -> do
              cooled <- claimPokeSlot message
              when cooled do
                -- 失败就算了：戳一戳是无声动作，冒一句「戳失败」比不说更怪。
                void (Chat.pokeUser message poker)
        pure Skip
    }

-- | 是不是「被戳」事件。
--
-- senderIsAllowed 这道闸门是必须的：日志里 12:06:44 那次来自 `sender_allowed=False`
-- 的人，而**戳一戳是会打扰真人的动作** —— 不加闸门，任何陌生人都能让 FM 去戳别人。
shouldPokeBack :: IncomingMessage -> Bool
shouldPokeBack message =
  pokeEventPrefix `Text.isPrefixOf` Text.strip message.text
    && isJust message.senderId

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
