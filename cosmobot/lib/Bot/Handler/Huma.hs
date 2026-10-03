{-# LANGUAGE OverloadedLabels #-}
{-|
Module      : Bot.Handler.Huma
Description : Deterministic "*fix <codes>" decoding, straight from the code table
Stability   : experimental
-}

module Bot.Handler.Huma
  ( humaHandlers
  )
where

import Bot.Core.Message
import Bot.Core.Route
import Bot.Prelude
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Media as Media
import qualified Bot.Util.Process as ProcessUtil
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import Effectful.FileSystem (FileSystem, doesFileExist)
import qualified Effectful.Process.Typed as TypedProcess
import qualified Effectful.Timeout as Timeout
import Effectful.Timeout (Timeout)
import System.Exit (ExitCode (..))

-- | The prefix the owner types. A constant so the matcher, the help text and
-- the reply cannot drift apart.
humaFixPrefix :: Text
humaFixPrefix = "*fix"

-- | The decoder reads this table and prints the sentence. Lives on the mounted
-- runtime volume, so replacing the table needs no rebuild and no restart.
humaDecodeScript :: FilePath
humaDecodeScript = "/data/huma/decode.py"

-- | The decoder takes ~140ms. This is a backstop so a wedged process can never
-- hold a route open -- not a budget to spend.
humaDecodeTimeoutMicros :: Int
humaDecodeTimeoutMicros = 10 * 1000000

-- | Owner-only for now. Setting this to False opens the command to everyone,
-- which costs nothing extra now that the model is out of the loop.
humaOwnerOnly :: Bool
humaOwnerOnly = True

-- | Send plain text, with no QQ quote attached.
--
-- The driver only builds a reply/quote payload when the target message carries
-- an id -- 'Bot.Chat.Driver.QQ' picks between "textOnly" and "withReply" on
-- @message.messageId@. That is also how @send_direct_message@ sends into a chat
-- it has no message for. Dropping the id here therefore yields a bare message,
-- which is what the owner asked for; nothing else about the routing changes.
plainReply :: Chat.Chat :> es => IncomingMessage -> Text -> Eff es ()
plainReply message = void . Chat.replyTo message{messageId = Nothing}

-- | Answers "*fix <codes>" without waking the agent.
--
-- Why this exists: measured on run agent--hxbXEri1l1W8EFZPkLjZA, the agent path
-- took 3150ms of which 2978ms (94.5%) was LLM turns (tool_enable, run_bash,
-- then the answer) and only 156ms was the decode. A code-table lookup is
-- deterministic, so routing it here removes the model entirely.
--
-- A second effect: this route answers through 'plainReply', so it never goes
-- through the Ask reply path that prepends the "😻 FM：" speaker prefix, and it
-- carries no QQ quote either. The answer is the sentence and nothing else,
-- which is what the owner asked for.
humaHandlers
  :: ( Chat.Chat :> es
     , FileSystem :> es
     , Media.Media :> es
     , Timeout :> es
     , Concurrent :> es
     , IOE :> es
     , TypedProcess.TypedProcess :> es
     )
  => [RouteHandler es]
humaHandlers =
  [humaFixRoute, humaUploadWatch, humaUpdateTableRoute]

-- ── 换码表：上传 -> 校验 -> 原子替换 ────────────────────────────────────
--
-- 固定指令，不走模型。理由不只是快（实测模型路径占 94.5% 的时间：3150ms 里
-- 2978ms 是三轮 LLM），更因为这是**破坏性操作**：智能识别的模糊匹配有可能在
-- 闲聊里被误触发（「那个码表更新了吗」），而一张坏表会让 *fix 整体失效、
-- 甚至悄悄解出错字。同义写法多认几个，确定性不变。

humaUpdateCommands :: [Text]
humaUpdateCommands =
  ["更新码表", "上传码表", "换码表", "码表更新", "更新一下码表"]

humaUpdateScript :: FilePath
humaUpdateScript = "/data/huma/update_table.py"

humaLastUploadPath :: FilePath
humaLastUploadPath = "/data/huma/last-upload.json"

-- | 下载 + 校验 + 替换要跑好几秒（1.7MB 的表），给足但要封顶。
humaUpdateTimeoutMicros :: Int
humaUpdateTimeoutMicros = 120 * 1000000

-- | 最近上传的有效期。QQ 里文件和文字是两条消息，所以指令到达时靠这个找文件。
humaUploadFreshSeconds :: Double
humaUploadFreshSeconds = 600

-- | 主人发来带附件的消息时，把下载地址记下来。
--
-- 为什么必须记：实测那条文件消息 `text` 是**空的**（文件单独一条），所以
-- 「先传文件、说指令」时指令那条消息身上没有附件。只认本条消息的附件会
-- 让这个功能永远用不上。只记主人的文件，不碰群里其他人的。
humaUploadWatch
  :: (FileSystem :> es, IOE :> es)
  => RouteHandler es
humaUploadWatch =
  Route
    { help = Nothing
    , helpVisible = const False
    , decide = \message -> do
        when (message.digest.senderIsSuperuser && not (null message.files)) $
          rememberUpload message
        pure Skip
    }

rememberUpload :: (FileSystem :> es, IOE :> es) => IncomingMessage -> Eff es ()
rememberUpload message = do
  now <- liftIO getCurrentTime
  let record =
        Aeson.object
          [ "at" Aeson..= (realToFrac (utcTimeToPOSIXSeconds now) :: Double)
          , "chat_id" Aeson..= message.chatId
          , "sender_id" Aeson..= message.senderId
          , "name" Aeson..= fmap (.name) (listToMaybe message.files)
          , "ref" Aeson..= fmap (.ref) (listToMaybe message.files)
          ]
  FileSystemByteString.writeFile humaLastUploadPath (LazyByteString.toStrict (Aeson.encode record))

-- | `更新码表`：取下载地址 -> 交给 update_table.py（校验不过它自己拒绝并说明）-> 回显。
humaUpdateTableRoute
  :: ( Chat.Chat :> es
     , FileSystem :> es
     , Media.Media :> es
     , Timeout :> es
     , Concurrent :> es
     , IOE :> es
     , TypedProcess.TypedProcess :> es
     )
  => RouteHandler es
humaUpdateTableRoute =
  withHelp
    (RouteHelp (fromMaybe "更新码表" (listToMaybe humaUpdateCommands) <> " [文件]") "用 QQ 文件消息里的码表替换 *fix 的码表；校验不过就拒绝、不动现有表。")
    $ stopOn humaUpdateFilter \message inlineRef ->
      if humaOwnerOnly && not message.digest.senderIsSuperuser
        then plainReply message "这条现在只对主人开放。"
        else do
          resolved <- resolveUpload message inlineRef
          case resolved of
            Left hint -> plainReply message hint
            Right url -> do
              result <- Timeout.timeout humaUpdateTimeoutMicros $
                ProcessUtil.readProcessGroupWithExitCode "python3" [humaUpdateScript, Text.unpack url]
              case result of
                Nothing -> plainReply message "更新超时了（下载或校验太慢），现有码表没动。"
                Just (exitCode, stdoutText, stderrText) -> do
                  let out = Text.strip stdoutText
                      err = Text.strip stderrText
                  if exitCode == ExitSuccess
                    then plainReply message (if Text.null out then "更新完成。" else out)
                    else
                      plainReply message $
                        "没换成（现有码表没动）：\n"
                          <> (if Text.null out then "" else out <> "\n")
                          <> (if Text.null err then "" else err)

-- | 优先用本条消息自带的附件；否则用 10 分钟内、同聊天、同发送者记下的那一个。
resolveUpload
  :: (FileSystem :> es, IOE :> es, Media.Media :> es)
  => IncomingMessage
  -> Maybe Text
  -> Eff es (Either Text Text)
resolveUpload message inlineRef = do
  candidate <- case inlineRef <|> fmap (.ref) (listToMaybe message.files) of
    Just ref -> pure (Right ref)
    Nothing -> do
      remembered <- readLastUpload
      now <- liftIO getCurrentTime
      pure $ case remembered of
        Just (at, chatId, senderId, ref)
          | chatId /= message.chatId ->
              Left "最近上传的文件不在这个聊天里。把码表发到当前聊天，或者带着附件发指令。"
          | senderId /= message.senderId ->
              Left "最近上传的文件不是你发的。"
          | realToFrac (utcTimeToPOSIXSeconds now) - at > humaUploadFreshSeconds ->
              Left "最近上传的文件超过 10 分钟了，重新发一次再试。"
          | otherwise -> Right ref
        Nothing ->
          Left "没找到码表文件。先在群里传一个 txt，然后 10 分钟内发「更新码表」。"
  either (pure . Left) resolveRef candidate

-- | 把 ref 变成脚本读得懂的东西：要么是 http(s) 链接，要么是本地路径。
--
-- 入站文件会被导进媒体库，所以 ref 往往**不是**那个 https 下载链接，而是
-- `media:mf_xxx`。第一版没处理这一步，直接把媒体 id 当路径传给了脚本，
-- 结果 FileNotFoundError（2026-10-03 实机就是这么挂的）。
-- 好在那次失败是干净的：脚本没读到文件，现有码表一个字都没动。
resolveRef :: Media.Media :> es => Text -> Eff es (Either Text Text)
resolveRef ref
  | "media:" `Text.isPrefixOf` Text.strip ref = do
      info <- Media.mediaFileInfoByRef ref
      pure $ case info of
        Just i
          | i.exists -> Right (Text.pack i.path)
          | otherwise -> Left "媒体库里那个文件已经不在了（可能被回收），重新传一次。"
        Nothing -> Left "媒体库里找不到那个文件，重新传一次。"
  | otherwise = pure (Right ref)

readLastUpload :: (FileSystem :> es, IOE :> es) => Eff es (Maybe (Double, Maybe Integer, Maybe Text, Text))
readLastUpload = do
  exists <- doesFileExist humaLastUploadPath
  if not exists
    then pure Nothing
    else do
      raw <- FileSystemByteString.readFile humaLastUploadPath
      pure $ case Aeson.eitherDecodeStrict' raw of
        Left _ -> Nothing
        Right value -> AesonTypes.parseMaybe parseRecord value
  where
    parseRecord = Aeson.withObject "last upload" \o ->
      (,,,)
        <$> o Aeson..: "at"
        <*> o Aeson..:? "chat_id"
        <*> o Aeson..:? "sender_id"
        <*> o Aeson..: "ref"

-- | 匹配更新指令，允许前面带 `fm `（主人习惯这么打）。返回值 = 本条消息自带附件的 ref。
humaUpdateFilter :: MessageFilter (Maybe Text)
humaUpdateFilter =
  MessageFilter \message -> do
    let stripped = Text.strip message.text
        withoutPrefix = Text.strip (fromMaybe stripped (Text.stripPrefix "fm" stripped))
    guard (withoutPrefix `elem` humaUpdateCommands)
    pure (fmap (.ref) (listToMaybe message.files))

humaFixRoute
  :: ( Chat.Chat :> es
     , Timeout :> es
     , Concurrent :> es
     , IOE :> es
     , TypedProcess.TypedProcess :> es
     )
  => RouteHandler es
humaFixRoute =
  withHelp
    (RouteHelp (humaFixPrefix <> " <虎码编码>") "把 *fix 后面的虎码编码解回句子（直接查码表，不经过模型）。")
    $ stopOn humaFixFilter \message codes ->
      if humaOwnerOnly && not message.digest.senderIsSuperuser
        then plainReply message "这条现在只对主人开放。"
        else runDecode message codes

runDecode
  :: ( Chat.Chat :> es
     , Timeout :> es
     , Concurrent :> es
     , IOE :> es
     , TypedProcess.TypedProcess :> es
     )
  => IncomingMessage
  -> Text
  -> Eff es ()
runDecode message codes
  | Text.null codes =
      plainReply message ("用法：" <> humaFixPrefix <> " <虎码编码>")
  | otherwise = do
      result <- Timeout.timeout humaDecodeTimeoutMicros $
        ProcessUtil.readProcessGroupWithExitCode "python3" [humaDecodeScript, Text.unpack codes]
      case result of
        Nothing ->
          plainReply message "解码超时了，稍后再试。"
        Just (exitCode, stdoutText, stderrText) ->
          let output = Text.strip stdoutText
              detail = Text.strip stderrText
           in case exitCode of
                ExitSuccess
                  | Text.null output -> plainReply message "解码器没有输出。"
                  | otherwise -> plainReply message output
                ExitFailure _ ->
                  plainReply message $
                    "解码失败：" <> if Text.null detail then "解码器返回了非零状态。" else detail

-- | Matches "*fix <codes>" and nothing else.
--
-- Deliberately stricter than 'command': that one strips a prefix and accepts
-- whatever follows, so "*fixfoo" would be read as the codes "foo". Here the
-- prefix must be followed by whitespace or end the message.
humaFixFilter :: MessageFilter Text
humaFixFilter =
  MessageFilter \message -> do
    rest <- Text.stripPrefix humaFixPrefix (Text.strip message.text)
    if Text.null rest || Text.isPrefixOf " " rest || Text.isPrefixOf "　" rest
      then Just (Text.strip rest)
      else Nothing
