{-|
Module      : Bot.Agent.Tools.Chat
Description : Agent tools for chat IO and chat metadata
Stability   : experimental
-}

module Bot.Agent.Tools.Chat
  ( queryChatLogTool
  , queryCurrentSenderChatLogTool
  , downloadNovelTool
  , novelStatusTool
  , selfMessagesTool
  , recallSelfMessagesTool
  , recallRecentSelfMessagesTool
  , sendReplyTool
  , messageBreakTool
  , sendFileTool
  , mentionUserTool
  , replyPrefixTool
  , pokeUserTool
  , likeUserTool
  , senderMemberInfoTool
  , memberInfoTool
  , userAvatarTool
  , listGroupMembersTool
  , currentMessageInfoTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Agent.Types
import Bot.Core.Message
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Bot.Chat.Bridge.FM as FMBridge
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.Media as Media
import qualified Bot.Handler.Humanlike as Humanlike
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import System.Directory (createDirectoryIfMissing, doesFileExist)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time (TimeZone (..), UTCTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime, utcToLocalTime)

queryChatLogTool :: (ChatLog.ChatLog :> es, HTTP.HTTP :> es, KatipE :> es) => Tool (Eff es)
queryChatLogTool =
  tagged [chatTag]
  . allowWhen hasExplicitChatLogIntent
  . withDescription "Return recent messages recorded in the current chat, optionally filtered by exact sender id. Results are in chronological order and include timestamps, sender ids, message ids, image urls, and text. Use since or before to page through time."
  $ tool "chat_log"
      ( requiredInt "limit" "Maximum number of recent messages to return."
      , optionalText "sender" "Optional exact sender id to include."
      , withDefault False (optionalBoolean "include_bot_messages" "Whether to include bot messages. Defaults to false.")
      , optionalArgument (fieldDateTime "since" "Return messages strictly after this ISO-8601 UTC timestamp.")
      , optionalArgument (fieldDateTime "before" "Return messages strictly before this ISO-8601 UTC timestamp.")
      )
      \limit sender includeBotMessages since before -> do
        context <- askToolContext
        if not (hasExplicitChatLogIntent context)
          then pure (toolText "未查询聊天记录：当前消息没有明确的查询记录请求。")
          else do
            let senderFilter = do
                  value <- Text.strip <$> sender
                  guard (not (Text.null value))
                  pure value
            case chatLogTimeRange since before of
              Left err ->
                pure (argumentFailure err)
              Right timeRange -> do
                entries <- ChatLog.queryChat context.message senderFilter (max 0 limit) includeBotMessages timeRange
                humanlike <- Humanlike.humanlikeScopeStateOrDisabled context.message
                pure (toolText (jsonText (map (chatLogToolEntry humanlike) entries)))

queryCurrentSenderChatLogTool :: (ChatLog.ChatLog :> es, HTTP.HTTP :> es, KatipE :> es) => Tool (Eff es)
queryCurrentSenderChatLogTool =
  tagged [chatTag]
  . noisy
  . withDescription "Return messages from the current sender whose text matches any keyword group. scope=chat searches only the current chat; scope=global searches all chats on the current platform. Each keyword group is matched as a SQL LIKE pattern with '%' between its terms. Results are newest first and limited to at most 100."
  $ tool "sender_log"
      ( requiredArgument (fieldTextArrayArray "keywords" "Keyword groups. Each inner array is joined with '%' and wrapped with '%' for ordered fuzzy matching.")
      , validateArgument validSenderLogLimit
          (requiredArgument (fieldIntegerMax "limit" 100 "Maximum number of matching messages to return. Must be <= 100."))
      , validateArgument parseSenderChatLogScope
          (withDefault "chat" (optionalArgument
            ("scope", Aeson.object
          [ "type" Aeson..= ("string" :: Text)
          , "enum" Aeson..= (["chat", "global"] :: [Text])
          , "description" Aeson..= ("Search the current chat or all chats on the current platform. Defaults to chat." :: Text)
          ])))
      , optionalArgument (fieldDateTime "since" "Return messages strictly after this ISO-8601 UTC timestamp.")
      , optionalArgument (fieldDateTime "before" "Return messages strictly before this ISO-8601 UTC timestamp.")
      )
      \keywords limit scope since before -> do
        context <- askToolContext
        case currentSenderChatLogScopeError scope context.message of
          Just err ->
              pure (argumentFailure err)
          Nothing ->
            case chatLogTimeRange since before of
              Left err ->
                pure (argumentFailure err)
              Right timeRange -> do
                entries <- ChatLog.queryCurrentSenderChatLog context.message scope keywords limit timeRange
                humanlike <- Humanlike.humanlikeScopeStateOrDisabled context.message
                pure (toolText (jsonText (map (chatLogToolEntry humanlike) entries)))

-- | 列出 FM 自己最近发过的消息，让它可以判断「该撤哪条」。
--
-- 2026-10-30：所有者反馈「撤回只会撤最近一条」。根因之一是 FM 根本看不到
-- 自己发过什么；chat_log 里其实存了 7000 多条自己发的消息（recordSelfMessage），
-- 只是从来没有工具把它读出来给模型看。
selfMessagesTool :: (ChatLog.ChatLog :> es, IOE :> es) => Tool (Eff es)
selfMessagesTool =
  tagged [chatTag]
    . allowWhen superuserOnly
    . withDescription
      "List FM's OWN recent messages in the current chat, newest first, with the message id, \
      \time, text and whether it carried images. Use this whenever the owner asks which \
      \messages you sent, or asks you to recall something specific (撤回那篇赛文 / 撤回刚才的图): \
      \look here first to identify the right messages, then call recall_self_messages with \
      \the ids. Never claim you cannot see your own messages."
    $ tool "self_messages"
        ( optionalInt "limit" "How many of FM's own recent messages to list, 1 to 50. Defaults to 20."
        , optionalInt "since_minutes" "Only include messages from the last N minutes. Defaults to 180."
        )
        \requestedLimit requestedMinutes -> do
          context <- askToolContext
          let limit = max 1 (min 50 (fromMaybe 20 requestedLimit))
              minutes = max 1 (min 1440 (fromMaybe 180 requestedMinutes))
          now <- liftIO getCurrentTime
          let since = addUTCTime (fromIntegral (negate minutes * 60)) now
          case chatLogTimeRange (Just since) Nothing of
            Left err -> pure (argumentFailure err)
            Right timeRange -> do
              entries <- ChatLog.queryChat context.message Nothing limit True timeRange
              let mine = filter (.isBot) entries
                  rows =
                    [ Aeson.object
                        [ "message_id" Aeson..= maybe "" messageIdText entry.messageId
                        , "time" Aeson..= maybe "" (Text.pack . show) entry.recordedAt
                        , "text" Aeson..= entry.text
                        , "images" Aeson..= length entry.imageUrls
                        ]
                    | entry <- mine
                    ]
              pure . toolText $
                if null rows
                  then "最近 " <> Text.pack (show minutes) <> " 分钟里没有我发的消息。"
                  else jsonText rows

-- | 按 message_id 撤回 FM 自己发过的消息（可以一次撤多条）。
recallSelfMessagesTool :: Chat.Chat :> es => Tool (Eff es)
recallSelfMessagesTool =
  tagged [chatTag]
    . noisy
    . allowWhen superuserOnly
    . withDescription
      "Recall specific messages that FM itself sent, by message id. Get the ids from \
      \self_messages first, pick the ones the owner actually means, and pass them here. \
      \This is how you recall 那篇赛文 / 刚才那张图 / 我说错的那一句 instead of only the \
      \newest message. IDs older than QQ's two-minute window will fail; report which failed \
      \and why instead of pretending it worked."
    $ tool "recall_self_messages"
        ( requiredArgument (fieldTextArray "message_ids" "Message ids to recall, from self_messages.")
        )
        \rawIds -> do
          context <- askToolContext
          let ids = filter (not . Text.null) (map Text.strip rawIds)
          if null ids
            then pure (argumentFailure "message_ids 不能为空。")
            else do
              results <- traverse (\rawId -> do
                let messageId = textMessageId rawId
                ok <- Chat.deleteMessage context.message messageId
                pure (rawId, ok)) ids
              let okIds = [i | (i, True) <- results]
                  badIds = [i | (i, False) <- results]
                  summary =
                    "撤回完成：成功 " <> Text.pack (show (length okIds))
                      <> " 条，失败 " <> Text.pack (show (length badIds)) <> " 条。"
                      <> if null badIds
                        then ""
                        else " 失败的多半是超过 QQ 两分钟时限：" <> Text.intercalate "、" badIds
              pure (toolText summary)

-- | 排队下载一本番茄小说。所有者 2026-11-05：想在 QQ 里说一句就让 FM 自己下。
--
-- 不在工具里直接下载：一本 300+ 章要几分钟，会把工具调用卡住。
-- 这里只把请求写成纯文本任务文件，交给宿主机的 cron 去消费
-- （容器的 /data 就是宿主机的 /opt/fm-cosmobot/runtime，同一个目录）。
--
-- 任务文件格式（一行一个字段，worker 用 splitOn "=" 读）：
--     job_id=novel-<毫秒时间戳>
--     book=<链接或 bookId>
--     chat_kind=group|private
--     chat_id=<QQ 号>
--     requester=<发起人 QQ 号>
downloadNovelTool :: IOE :> es => Tool (Eff es)
downloadNovelTool =
  tagged [chatTag]
    . allowWhen superuserOnly
    . withDescription
      "Queue a novel from fanqienovel.com to be downloaded into FM's typing library. \
      \Pass the book URL or its numeric book id. The download runs in the background and \
      \takes a few minutes; tell the owner it has started and that they can ask \
      \novel_status for progress. Do not claim the text is ready immediately."
    $ tool "download_novel"
        ( requiredText "book" "fanqienovel.com book URL or numeric book id."
        )
        \book -> do
          context <- askToolContext
          now <- liftIO getCurrentTime
          let stamp = show (floor (utcTimeToPOSIXSeconds now * 1000) :: Integer)
              jobId = "novel-" <> stamp
              kindText = if context.message.kind == ChatGroup then "group" else "private"
              chatIdText = maybe "" (Text.pack . show) context.message.chatId
              requesterText = fromMaybe "" context.message.senderId
              body =
                Text.unlines
                  [ "job_id=" <> jobId
                  , "book=" <> Text.strip book
                  , "chat_kind=" <> kindText
                  , "chat_id=" <> chatIdText
                  , "requester=" <> requesterText
                  ]
          liftIO (createDirectoryIfMissing True "/data/novel-jobs")
          liftIO (TextIO.writeFile ("/data/novel-jobs/" <> Text.unpack jobId <> ".txt") body)
          pure . toolText $
            "已经排队：" <> Text.strip book
              <> "（任务号 " <> jobId <> "）\n"
              <> "后台下载中，一般几分钟；下完我会主动告诉你。\n"
              <> "想查进度可以说：小说下好了吗"

-- | 查看小说下载进度。worker 会把状态追加到 /data/novel-status.txt。
novelStatusTool :: IOE :> es => Tool (Eff es)
novelStatusTool =
  tagged [chatTag]
    . allowWhen superuserOnly
    . withDescription
      "Show recent novel download activity: queued, downloading, done with chapter count, \
      \or failed. Use when the owner asks 小说下好了吗 or 下载到哪了."
    $ tool "novel_status" noArguments do
        exists <- liftIO (doesFileExist novelStatusPath)
        if not exists
          then pure (toolText "还没有任何小说下载任务。")
          else do
            raw <- liftIO (TextIO.readFile novelStatusPath)
            let recent = takeLast 12 (filter (not . Text.null) (Text.lines (Text.strip raw)))
            pure . toolText $
              if null recent
                then "还没有任何小说下载任务。"
                else Text.unlines recent

novelStatusPath :: FilePath
novelStatusPath = "/data/novel-status.txt"

takeLast :: Int -> [a] -> [a]
takeLast count items = drop (max 0 (length items - count)) items

recallRecentSelfMessagesTool :: Chat.Chat :> es => Tool (Eff es)
recallRecentSelfMessagesTool =
  tagged [chatTag]
  . noisy
  . allowWhen superuserOnly
  . withDescription "Recall FM's own messages sent in the current QQ chat during the last two minutes. This works for normal answers, typing-practice articles, tool output, images, audio, and files. Use it whenever the owner naturally asks FM to 撤回刚才的回答、撤回最近几条消息、撤回刚才的图片 or 撤回全部刚发内容. Never use chat_log or recall-history query tools for deleting FM's own messages. For plural or 全部 requests, set count high enough, up to 50."
  $ tool "recall_recent_self_messages"
      (optionalInt "count" "Number of FM's recent messages to recall, from 1 to 50. Use 1 for singular requests and up to 50 for 全部.")
      \requestedCount -> do
        context <- askToolContext
        let count = max 1 (min 50 (fromMaybe 1 requestedCount))
        (recalled, failed) <- Chat.recallRecentSelfMessages context.message count
        pure . toolText $
          case (recalled, failed) of
            (0, 0) -> "最近两分钟没有可撤回的 FM 消息。"
            (0, failures) ->
              "撤回失败，共 " <> show failures <> " 条；可能已经超过 QQ 的两分钟时限。"
            (successes, 0) ->
              "已成功撤回最近 " <> show successes <> " 条 FM 消息。"
            (successes, failures) ->
              "已撤回 " <> show successes <> " 条，另有 " <> show failures <> " 条撤回失败。"

-- | 一个空工具：模型调用它来表示"这条消息到此为止"。
--
-- 断点由模型决定，而不是由字数规则猜——这是 dsh-im-humanize 打磨出来的做法。
-- 工具本身什么都不做，回复管线把这个调用当作消息分隔。
messageBreakTool :: Tool (Eff es)
messageBreakTool =
  tagged [chatTag]
  . withDescription "Insert a chat message break here: everything you have written so far is delivered as its own chat message, and you continue afterwards. Call it when you would pause and send another line. It performs no action; pass no arguments."
  $ tool "message_break" noArguments do
      pure (toolText "break")

sendReplyTool :: Chat.Chat :> es => Tool (Eff es)
sendReplyTool =
  tagged [chatTag]
  . noisy
  . allowWhen hasExplicitAdditionalSendIntent
  . withDescription "Send a reply message to the same chat as the current user message. Supports text and image URLs. After search_web returns image_urls for an explicit image-search request, you MUST call this tool with image_urls so the platform sends actual images; never paste those URLs into the final answer. Also use it when the user explicitly asks you to send an additional message before the final answer."
  $ tool "send_reply"
      ( optionalText "text" "Message text to send. May be omitted when image_urls is non-empty."
      , optionalTextArray "image_urls" "Image URLs to send as images in the same reply. The platform must be able to fetch these URLs."
      )
      \maybeText maybeImageUrls -> do
        context <- askToolContext
        let text = Text.strip (fromMaybe "" maybeText)
            imageUrls = filter (not . Text.null) (map Text.strip (fromMaybe [] maybeImageUrls))
            body = replyBodyWithImages text imageUrls
        if not (hasExplicitAdditionalSendIntent context)
          then pure (toolText "未发送：当前消息没有明确要求额外发送消息。")
          else if Text.null body
          then pure (argumentFailure "Either text or image_urls must be provided.")
          else do
            sent <- Chat.replyTo context.message (FMBridge.fmReplyRelayBodyForRequest context.input.text body)
            case rights sent of
              messageIds@(_:_) -> do
                let sentText = show messageIds :: String
                pure (toolText [i|Sent message ids: #{sentText}|])
              [] ->
                let err = Text.intercalate "\n" (lefts sent)
                 in
                pure (toolFailure Failure
                  { category = ExternalServiceUnavailable
                  , userMessage = [i|发送消息失败：#{err}|]
                  , detail = err
                  })

sendFileTool :: Chat.Chat :> es => Tool (Eff es)
sendFileTool =
  tagged [chatTag]
  . noisy
  . allowWhen superuserOnly
  . withDescriptionBy (\context ->
      "Send a local file to the same chat as the current user message. "
        <> case context.message.platform of
          PlatformQQ ->
            "First move or copy the file into the NapCat container, then pass its path inside that container. Do not pass the host path."
          _ ->
            "The path must be readable by the bot."
        <> " Use only when the user explicitly asks you to send a file.")
  $ tool "send_file"
      (validateArgument validFilePath
        (requiredText "path" "Local file path to send. A file:// prefix is accepted and stripped before upload."))
      \path -> do
        context <- askToolContext
        result <- Chat.uploadFile context.message path Nothing
        case result of
          Right sent -> do
            let sentText = show sent :: String
            pure (toolText [i|Sent file #{Text.pack path}; message id: #{sentText}|])
          Left err -> do
            let failureText = "发送文件失败：" <> err
            void $ Chat.replyTo context.message failureText
            pure (toolFailure Failure
              { category = ExternalServiceUnavailable
              , userMessage = failureText
              , detail = err
              })

mentionUserTool :: Chat.Chat :> es => Tool (Eff es)
mentionUserTool =
  tagged [chatTag]
  . noisy
  . withDescription "Send a reply in the current chat that mentions the given platform user id. Matrix user ids are textual, for example @user:server."
  $ tool "mention_user"
      ( userIdArgument "Platform user id to mention."
      , requiredText "text" "Message text to send after the mention."
      )
      \userId text -> do
        context <- askToolContext
        Chat.mentionUser context.message userId text >>= \case
          Right sent -> do
            let sentText = show sent :: String
            pure (toolText [i|Sent mention message id: #{sentText}|])
          Left err ->
            pure (toolFailure Failure
              { category = ExternalServiceUnavailable
              , userMessage = [i|发送提及消息失败：#{err}|]
              , detail = err
              })

-- | 戳一下某个用户（QQ 的「戳一戳」）。
--
-- **主人限定**（allowWhen superuserOnly）。戳一戳会打扰真人，「谁都能让 FM 去戳谁」
-- 就是个骚扰工具 —— 这是所有者 2026-10-04 明确划的边界。
--
-- 戳本身**没有文字**，所以不用额外设计「戳 + 一句话」：模型调用这个工具时，
-- 它自己那条回复就是对方看到的话。
-- | 控制回复前缀。所有者 2026-10-29：要自己决定带不带「😻 FM：」。
--   改动立刻生效（IORef），同时写到 /data/reply-prefix.txt，重启后保持。
replyPrefixTool :: IOE :> es => Tool (Eff es)
replyPrefixTool =
  tagged [chatTag]
    . allowWhen superuserOnly
    . withDescription
      "Control the prefix FM puts in front of its replies (default: 😻 FM：). \
      \Actions: show, set (needs text), off (no prefix at all), reset (default). \
      \Changes take effect immediately. Use this when the owner asks to stop \
      \using the prefix, to change it, or to bring it back."
    $ tool "reply_prefix"
        ( requiredText "action" "One of: show, set, off, reset."
        , optionalText "text" "New prefix text; required when action=set."
        )
        \action requestedText -> do
          let normalized = Text.toLower (Text.strip action)
          case normalized of
            "show" -> pure (toolText (describePrefix (FMBridge.replyPrefixFor ())))
            "off" -> do
              liftIO (FMBridge.writeReplyPrefix "")
              -- 诚实说明：写文件是可靠的，但纯函数里的全局状态会被 GHC 折叠，
              -- 运行时改不进去（2026-10-29 验证过两次），所以必须重启才生效。
              pure (toolText "已关闭前缀，已写入配置文件。注意：需要重启 FM 进程才真正生效（约 10 秒）。")
            "reset" -> do
              liftIO (FMBridge.writeReplyPrefix FMBridge.defaultReplyPrefix)
              pure (toolText ("已恢复默认前缀：" <> FMBridge.defaultReplyPrefix))
            "set" ->
              case Text.strip <$> requestedText of
                Just value
                  | not (Text.null value) -> do
                      liftIO (FMBridge.writeReplyPrefix value)
                      pure (toolText ("前缀已改为：" <> value))
                _ -> pure (argumentFailure "action=set 需要 text 参数。")
            _ -> pure (argumentFailure "action 只能是 show / set / off / reset。")
  where
    describePrefix prefix
      | Text.null prefix = "当前不带前缀。"
      | otherwise = "当前前缀：" <> prefix <> "（例：" <> prefix <> "今天群里谁最活跃）"

pokeUserTool :: Chat.Chat :> es => Tool (Eff es)
pokeUserTool =
  tagged [chatTag]
  . allowWhen superuserOnly
  . noisy
  . withDescription "Poke a user in the current chat (QQ 戳一戳). A poke reaches a real person: use it only when the owner asks for it in so many words, and never repeatedly. The poke itself carries no words -- what you say in your reply is what the person reads."
  $ tool "poke_user"
      (userIdArgument "Platform user id to poke.")
      \userId -> do
        context <- askToolContext
        Chat.pokeUser context.message userId >>= \case
          Right () ->
            pure (toolText "已戳。戳本身不带文字，你回复里说的话才是对方看到的内容。")
          Left err ->
            pure (toolFailure Failure
              { category = ExternalServiceUnavailable
              , userMessage = [i|戳一戳失败：#{err}|]
              , detail = err
              })

-- | 给某个用户点赞（QQ 的名片赞）。
--
-- **主人限定**（allowWhen superuserOnly），和 poke_user 同一道闸门：点赞比戳温和，
-- 但它同样是「替 FM 对真人做一个动作」，谁有权触发这件事必须先定下来。
--
-- 次数夹在 1..10（QQ 有上限）；默认 1 下 —— 拉满容易被限，被点的人也会看到一串。
likeUserTool :: Chat.Chat :> es => Tool (Eff es)
likeUserTool =
  -- 所有者 2026-10-04 决定：**对所有人开放**（原来和 poke_user 一样是主人限定）。
  -- 点赞是善意动作，不像戳会打扰人，所以边界可以不同 —— 注意 **pokeUserTool 仍是主人限定**。
  --
  -- 代价写在明处：QQ 对名片赞每天只有约 10 个额度，放开之后几个人连着用就会见底。
  -- 池子满了要说「没点上」，不许假装点上。
  tagged [chatTag]
  . noisy
  . withDescription "Give a user a QQ profile like (名片赞). Anyone in the chat may ask for this, for themselves or for someone else, so use it when asked. Default is one like. QQ caps profile likes per account per day (about ten), so never ask for many, and when a like fails say so plainly instead of claiming success. A like is not a notification the person reads -- your reply is what carries the words."
  $ tool "like_user"
      ( userIdArgument "Platform user id to like."
      , optionalInteger "times" "How many likes to give. Defaults to 1; clamped to 1-10."
      )
      \userId requestedTimes -> do
        context <- askToolContext
        -- optionalInteger 给的是 Maybe Integer，驱动那层用 Int —— 显式转，别让它去猜。
        Chat.likeUser context.message userId (fromIntegral (fromMaybe 1 requestedTimes)) >>= \case
          Right () ->
            pure (toolText "已点赞。点赞本身不带文字，你回复里说的话才是对方看到的内容。")
          Left err ->
            pure (toolFailure Failure
              { category = ExternalServiceUnavailable
              , userMessage = [i|点赞失败：#{err}|]
              , detail = err
              })

senderMemberInfoTool :: Chat.Chat :> es => Tool (Eff es)
senderMemberInfoTool =
  tagged [chatTag]
  . withDescription "Get platform-provided member information for the sender of the current message in the current group chat."
  $ tool "sender_info" noArguments do
      context <- askToolContext
      info <- Chat.getSenderMemberInfo context.message
      pure (toolText (maybe "No member information is available for this message." jsonText info))

memberInfoTool :: Chat.Chat :> es => Tool (Eff es)
memberInfoTool =
  tagged [chatTag]
  . withDescription "Get platform-provided member information for any user id in the current group chat."
  $ tool "member_info"
      (userIdArgument "Platform user id to query in the current group.")
      \userId -> do
        context <- askToolContext
        info <- Chat.getMemberInfo context.message userId
        pure (toolText (maybe "No member information is available for this user in the current chat." jsonText info))

userAvatarTool :: (Chat.Chat :> es, Media.Media :> es, KatipE :> es) => Tool (Eff es)
userAvatarTool =
  noisy
  . allowWhen hasExplicitAvatarIntent
  . withDescription "Get avatar information for a platform user id and send the avatar image to the current chat."
  $ tool "user_avatar"
      (userIdArgument "Platform user id to query. Use message_info first when the target is the current sender or a mentioned user. 0 is invalid.")
      \userId -> do
        context <- askToolContext
        if not (hasExplicitAvatarIntent context)
          then pure (toolText "未发送头像：当前消息没有明确的头像请求。")
          else do
            avatar <- Chat.getUserAvatar context.message userId
            case avatar of
              Nothing ->
                pure (toolText "No avatar is available for this user on this platform.")
              Just value ->
                userAvatarResult context value

listGroupMembersTool :: Chat.Chat :> es => Tool (Eff es)
listGroupMembersTool =
  tagged [chatTag]
  . withDescription "List members in the current group chat, including platform user ids and nicknames when available. QQ groups are supported. Telegram Bot API does not expose full member lists, so Telegram may return unavailable."
  $ tool "group_members" noArguments do
      context <- askToolContext
      members <- Chat.listGroupMembers context.message
      pure (toolText (maybe "Group member listing is not available for this platform or chat." jsonText members))

currentMessageInfoTool :: Tool (Eff es)
currentMessageInfoTool =
  tagged [chatTag]
  . withDescription "Return structured metadata for the current message, including platform, chat, sender, message ids, mentions, image URLs, and text."
  $ tool "message_info" noArguments do
      context <- askToolContext
      pure (toolText (jsonText (currentMessageInfoValue context.message)))

currentMessageInfoValue :: IncomingMessage -> Aeson.Value
currentMessageInfoValue message =
  Aeson.object
    [ "platform" Aeson..= chatPlatformKey message.platform
    , "chat_kind" Aeson..= (show message.kind :: Text)
    , "chat_id" Aeson..= message.chatId
    , "chat_aliases" Aeson..= message.chatAliases
    , "message_id" Aeson..= message.messageId
    , "reply_to_message_id" Aeson..= message.replyToMessageId
    , "sender_id" Aeson..= message.senderId
    , "sender_username" Aeson..= message.senderUsername
    , "mentions" Aeson..= Aeson.object
        [ "user_ids" Aeson..= message.mentions
        , "text_user_ids" Aeson..= message.mentionUsernames
        ]
    , "image_urls" Aeson..= message.imageUrls
    , "text" Aeson..= message.text
    ]

-- | Render one chat-log row for a tool result.
--
-- Rows exchanged while the humanlike mode was on carry a @mode@ field so the
-- model can scope an earlier style agreement to that mode instead of reading it
-- as a standing rule. The row itself is never hidden or rewritten.
chatLogToolEntry :: Humanlike.HumanlikeState -> ChatLog.ChatLogEntry -> Aeson.Value
chatLogToolEntry humanlike entry =
  Aeson.object $
    [ "timestamp" Aeson..= fmap formatChinaTimestamp entry.recordedAt
    , "chatId" Aeson..= entry.chatId
    , "senderId" Aeson..= entry.senderId
    , "senderUsername" Aeson..= entry.senderUsername
    , "messageId" Aeson..= entry.messageId
    , "imageUrls" Aeson..= entry.imageUrls
    , "text" Aeson..= entry.text
    ]
      <> [ "mode" Aeson..= ("humanlike" :: Text)
         | Just recordedAt <- [entry.recordedAt]
         , Humanlike.isInHumanlikeSpan recordedAt humanlike.spans
         ]

-- Chat-log records are stored in UTC; present them in the user's China timezone.
formatChinaTimestamp :: UTCTime -> Text
formatChinaTimestamp =
  Text.pack . (<> " +08:00") . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" . utcToLocalTime chinaTimeZone

chinaTimeZone :: TimeZone
chinaTimeZone = TimeZone 480 False "CST"

parseSenderChatLogScope :: Text -> Either Text ChatLog.SenderChatLogScope
parseSenderChatLogScope = \case
  "chat" ->
    Right ChatLog.SenderChatLogChat
  "global" ->
    Right ChatLog.SenderChatLogGlobal
  _ ->
    Left "scope must be chat or global."

validSenderLogLimit :: Int -> Either Text Int
validSenderLogLimit limit
  | limit < 0 =
      Left "limit must be >= 0."
  | limit > 100 =
      Left "limit must be <= 100."
  | otherwise =
      Right limit

chatLogTimeRange
  :: Maybe UTCTime
  -> Maybe UTCTime
  -> Either Text ChatLog.ChatLogTimeRange
chatLogTimeRange since before
  | maybe False (uncurry (>=)) ((,) <$> since <*> before) =
      Left "since must be earlier than before."
  | otherwise =
      Right ChatLog.ChatLogTimeRange{since, before}

currentSenderChatLogScopeError :: ChatLog.SenderChatLogScope -> IncomingMessage -> Maybe Text
currentSenderChatLogScopeError scope message
  | isNothing message.senderId =
      Just "Current message has no sender_id; cannot query sender-scoped chat log."
  | scope == ChatLog.SenderChatLogChat
  , isNothing message.chatId =
      Just "Current message has no chat_id; cannot query chat-scoped chat log."
  | otherwise =
      Nothing

hasExplicitChatLogIntent :: Context -> Bool
hasExplicitChatLogIntent context =
  any (`Text.isInfixOf` normalized)
    -- 2026-10-23：这层闸门原来只认「聊天记录/历史消息」这类字眼，于是
    -- 「这个群今天谁最活跃」「今天聊了什么」这种**统计/回顾**请求拿不到工具，
    -- FM 只能 run_bash 手写 SQL 查 chat_log —— 48 小时里 96 次，占一半的 shell 调用。
    -- 闸门是为了防止无关话题被当成「查记录」，但统计和回顾本来就是在查记录。
    [ "聊天记录", "聊天日志", "历史消息", "刚才谁说", "查记录", "查一下记录", "看看记录"
    , "聊天历史", "search my history", "chat history", "recent messages"
    -- 统计类
    , "谁最活跃", "谁说得最多", "谁话最多", "谁发言最多", "最活跃"
    , "说了多少", "多少条", "几条", "统计一下", "统计下", "发言数"
    -- 回顾 / 总结类
    , "聊了什么", "聊了啥", "聊些什么", "说了什么", "说了啥"
    , "总结一下", "总结下", "概括一下", "回顾一下"
    ]
  where
    normalized = Text.toLower context.input.text

hasExplicitAvatarIntent :: Context -> Bool
hasExplicitAvatarIntent context =
  any (`Text.isInfixOf` normalized)
    [ "头像", "头像图片", "看头像", "发头像", "用户头像" ]
  where
    normalized = Text.toLower context.input.text

hasExplicitAdditionalSendIntent :: Context -> Bool
hasExplicitAdditionalSendIntent context =
  any (`Text.isInfixOf` normalized)
    [ "再发", "另外发", "额外发", "发送一条", "发一条消息", "把图片发", "把文件发" ]
    || isExplicitImageSearch normalized
  where
    normalized = Text.toLower context.input.text

isExplicitImageSearch :: Text -> Bool
isExplicitImageSearch normalized =
  any (`Text.isInfixOf` normalized)
    [ "搜图", "搜索图片", "查找图片", "找张图", "找一张图"
    , "找个图", "找一个图", "搜张图", "搜一张图"
    ]
  || (any (`Text.isInfixOf` normalized)
        ["搜", "搜索", "找", "查找", "想看", "给我", "来张", "来一张", "发张", "发一张"]
        && any (`Text.isInfixOf` normalized) ["图", "图片", "照片", "海报", "壁纸", "剧照"])

userIdArgument :: Text -> ToolArgument Text
userIdArgument description =
  mapArgument
    (parseUserIdValue >=> validateUserId)
    (requiredArgument (fieldText "user_id" description))

parseUserIdValue :: Aeson.Value -> AesonTypes.Parser Text
parseUserIdValue value =
  (Text.strip <$> Aeson.parseJSON value)
    <|> (Text.pack . show <$> (Aeson.parseJSON value :: AesonTypes.Parser Integer))

validateUserId :: Text -> AesonTypes.Parser Text
validateUserId userId
  | Text.null (Text.strip userId) =
      fail "user_id must not be empty."
  | Text.strip userId == "0" =
      fail "user_id must not be 0."
  | otherwise =
      pure (Text.strip userId)

avatarUrl :: Aeson.Value -> Maybe Text
avatarUrl =
  AesonTypes.parseMaybe $
    Aeson.withObject "user avatar" (Aeson..: Key.fromText "avatar_url")

userAvatarResult :: (Chat.Chat :> es, Media.Media :> es, KatipE :> es) => Context -> Aeson.Value -> Eff es ToolResult
userAvatarResult context value =
  case avatarUrl value of
    Nothing ->
      pure (toolText (jsonText value))
    Just url -> do
      ref <- Media.normalizeMediaRef url
      Media.mediaFileInfoByRef ref >>= \case
        Nothing ->
          sendAvatar url
        Just info
          | "image/" `Text.isPrefixOf` Text.toLower info.mimeType -> do
              let body = FMBridge.fmReplyRelayBodyForRequest context.input.text (ReplyBody.imageDirective ref)
              sent <- Chat.replyTo context.message body
              logInfo [i|user_avatar sent avatar image: url=#{ref} message_id=#{show sent :: Text}|]
              pure (toolTextWithImages (jsonText value) [ref])
        _ ->
          pure (toolFailure (permanentArgumentFailure "头像地址没有返回图片，已阻止发送。" "头像地址没有返回图片，已阻止发送。"))
  where
    sendAvatar imageRef = do
      let body = FMBridge.fmReplyRelayBodyForRequest context.input.text (ReplyBody.imageDirective imageRef)
      sent <- Chat.replyTo context.message body
      logInfo [i|user_avatar sent avatar image: url=#{imageRef} message_id=#{show sent :: Text}|]
      pure (toolTextWithImages (jsonText value) [imageRef])

validFilePath :: Text -> Either Text FilePath
validFilePath rawPath
  | Text.null (Text.strip path) =
      Left "path must not be empty."
  | otherwise =
      Right (Text.unpack path)
  where
    stripped = Text.strip rawPath
    path = fromMaybe stripped (Text.stripPrefix "file://" stripped)

argumentFailure :: Text -> ToolResult
argumentFailure err =
  toolFailure (permanentArgumentFailure err err)

replyBodyWithImages :: Text -> [Text] -> Text
replyBodyWithImages text imageUrls =
  Text.strip $ Text.unlines $
    [ text | not (Text.null text) ]
      <> map ReplyBody.imageDirective imageUrls
