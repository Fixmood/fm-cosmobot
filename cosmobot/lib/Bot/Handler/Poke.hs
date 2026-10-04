{-# LANGUAGE OverloadedLabels #-}
{-|
Module      : Bot.Handler.Poke
Description : Poke back when someone pokes FM (QQ 戳一戳)
Stability   : experimental
-}

module Bot.Handler.Poke
  ( pokeHandlers
  , likeMeHandlers
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
  [pokeBackRoute, likeMeRoute]

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
-- ── 「赞我」 ────────────────────────────────────────────────────────────
--
-- 有人说「赞我」→ 立刻给这个人点一个赞，然后**返回 Skip 把消息放走**，
-- 由模型接一句符合当前语境的话。
--
-- 为什么点赞走 route、说话走模型：**点赞本身不带文字**，而「符合语境的那句话」
-- 只有模型说得出。这和回戳是同一个分工（反射瞬时完成、说话交给它自己）。
--
-- 三道闸门，每一道都是今天踩出来的：
--   1. **整句匹配** —— 「赞我」必须就是整条消息，群里闲聊不会误触。
--   2. **senderIsAllowed** —— 戳那次漏了这条，结果陌生人都能让 FM 去戳真人。
--   3. **冷却 6 小时/人** —— QQ 对名片赞有每日上限，被刷一次就废了。

likeMePhrases :: [Text]
likeMePhrases = ["赞我", "赞我一下", "给我点赞", "给我点个赞"]

likeMeCooldownSeconds :: Double
likeMeCooldownSeconds = 6 * 3600

likeMeHandlers
  :: (Chat.Chat :> es, FileSystem :> es, IOE :> es)
  => [RouteHandler es]
likeMeHandlers =
  [likeMeRoute]

likeMeRoute
  :: (Chat.Chat :> es, FileSystem :> es, IOE :> es)
  => RouteHandler es
likeMeRoute =
  Route
    { help = Just (RouteHelp "赞我" "发一句「赞我」，就给你点一个赞，然后说一句。")
    , helpVisible = const True
    , decide = \message -> do
        when (isLikeMe message && message.digest.senderIsAllowed) do
          case message.senderId of
            Nothing -> pure ()
            Just who ->
              -- 所有者 2026-10-04：**不做每人冷却** —— QQ 对名片赞每天只有 10 个额度，
              -- 与其用冷却去猜「他是不是点过了」，不如照点，**额度满了就如实说**。
              -- （原来 6 小时冷却的写法会让一天最多 4 次/人，但全群只有 10 个总额度，
              --   一个人就能把额度吃掉，反而更不公平。）
              Chat.likeUser message who 1 >>= \case
                -- 成功不出声：那句话交给模型说（本 route 返回 Skip）。
                Right () -> pure ()
                -- 失败**必须出声**：尤其是「今天额度用完了」—— 不说的话对方会以为点上了。
                Left err ->
                  void $ Chat.replyTo message{messageId = Nothing} [i|没点上：#{err}|]
        -- 不吞消息：那句话交给它自己说。
        pure Skip
    }

isLikeMe :: IncomingMessage -> Bool
isLikeMe message =
  Text.strip message.text `elem` likeMePhrases

shouldPokeBack :: IncomingMessage -> Bool
shouldPokeBack message =
  pokeEventPrefix `Text.isPrefixOf` Text.strip message.text
    && isJust message.senderId

-- | 冷却：同一个聊天里，pokeCooldownSeconds 内只回戳一次。
claimPokeSlot :: (FileSystem :> es, IOE :> es) => IncomingMessage -> Eff es Bool
claimPokeSlot message =
  claimSlot (maybe "-" (Text.pack . show) message.chatId) pokeCooldownSeconds

-- | 通用冷却：同一把钥匙在 windowSeconds 内只放行一次。
-- 「赞我」用同一份状态文件，只是键带 "like:" 前缀（按人算，不是按聊天算）。
claimSlot :: (FileSystem :> es, IOE :> es) => Text -> Double -> Eff es Bool
claimSlot slotKey windowSeconds = do
  now <- liftIO getCurrentTime
  let nowSeconds = realToFrac (utcTimeToPOSIXSeconds now) :: Double
  existing <- readPokeState
  case Map.lookup slotKey existing of
    Just lastAt | nowSeconds - lastAt < windowSeconds -> pure False
    _ -> do
      let updated = Map.insert slotKey nowSeconds existing
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
