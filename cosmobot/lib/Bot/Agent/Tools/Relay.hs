-- | Tell a member something in the group they were talking in, when the owner
-- asks for it from somewhere else (usually a private chat).
--
-- Why a separate tool: Chat.replyTo always replies to the chat the inbound
-- message came from, and the existing private-relay tool both resolves names
-- only inside the current chat and only sends QQ private messages. Posting into a
-- *different* group is what "go tell her in that group" needs. The QQ driver
-- chooses the destination from (kind, chatId), so a synthetic target message
-- aimed at the destination group is enough -- the same device bridgeTestTarget
-- already uses.
--
-- Name resolution asks the platform for each group's member list, because no
-- nickname-to-number table exists in memory or in the database. Live group cards
-- are spaced for display ('『枝 江』橘 猫'), so matching strips all whitespace.
{-# LANGUAGE FieldSelectors #-}

module Bot.Agent.Tools.Relay
  ( fmTellMemberTool
  , normalizedName
  , knownQqGroups
  ) where

import Bot.Agent.Tool
import Bot.Agent.Tools.Common (optionalBoolean, optionalText, requiredText, superuserOnly)
import Bot.Agent.Types (Context (..), toolFailure, toolText)
import qualified Bot.Agent.Failure as Failure
import Bot.Core.Message
  ( ChatKind (ChatGroup, ChatPrivate)
  , ChatPlatform (PlatformQQ)
  , MessageDigest (..)
  , IncomingMessage (..)
  )
import qualified Bot.Effect.Chat as Chat
import Bot.Prelude
import qualified Data.Aeson as Aeson
import Data.Char (isSpace)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text

-- | QQ groups this bot is in, read from the live chat log. A tool cannot see the
-- loaded Config and neither ChatDriver nor the Chat effect exposes the group
-- list, so this is the pragmatic source. Add a group here when the bot joins one.
knownQqGroups :: [Integer]
knownQqGroups =
  [ 21037015
  , 906230260
  , 776227233
  , 697015277
  , 145326153
  , 171262755
  , 1025725817
  , 216063968
  , 832089679
  , 158408350
  , 178356026
  ]

-- | A member we could deliver to.
data RelayMember = RelayMember
  { memberUserId :: !Integer
  , memberName   :: !Text
  , memberGroup  :: !Integer
  }

-- | Remove every kind of space and zero-width separator, so '橘猫' matches the
-- display-spaced card '『枝 江』橘 猫'.
normalizedName :: Text -> Text
normalizedName =
  Text.filter
    (\c -> not (isSpace c) && c /= '\x3000' && c /= '\x200b' && c /= '\x2060')

fmTellMemberTool :: Chat.Chat :> es => Tool (Eff es)
fmTellMemberTool =
  allowWhen tellMemberGuard
  . withDescription "Tell a person something in the QQ group they were talking in, when the owner asks for it from somewhere else such as a private chat. Use only when the owner explicitly asks FM to tell, relay, or notify that person. target is a nickname, group card, or QQ number. If the name matches several people, or one person in several groups, this reports the candidates and sends nothing, because delivering to the wrong person is worse than not delivering. Set dry_run to true to see exactly what would be sent, and where, without sending it."
  $ tool "fm_tell_member"
      ( requiredText "target" "Nickname, group card, or QQ number of the person to tell."
      , requiredText "content" "The exact message to deliver, without adding interpretation."
      , optionalText "group" "Optional group id, when the same person is in several groups."
      , withDefault False (optionalBoolean "dry_run" "When true, report what would be sent and send nothing.")
      )
      \target content groupText dryRun -> do
        context <- askToolContext
        if not (explicitTellRequest context.message.text)
          then pure (toolText "未发送：只有你明确说要告诉、传话或转告某人时，FM 才会代你发话。")
          else do
            let hint = fmap (Text.strip) groupText
            candidates <- resolveCandidates context target hint
            case chooseCandidate candidates of
              Left reason -> pure (toolText reason)
              Right member -> do
                let body = mentionBody member.memberUserId ("Fix哥说：" <> Text.strip content)
                    destination =
                      context.message
                        { platform = PlatformQQ
                        , kind = ChatGroup
                        , chatId = Just member.memberGroup
                        , chatAliases = []
                        , digest =
                            context.message.digest
                              { chatIsAllowed = True
                              , senderIsAllowed = True
                              , mentionsBot = True
                              }
                        }
                if dryRun
                  then
                    pure . toolText $
                      [i|干跑：本应发到 QQ群 #{member.memberGroup}，@#{member.memberUserId}（#{member.memberName}），内容：#{body}|]
                  else do
                    sent <- Chat.replyTo destination body
                    if any isRight sent
                      then
                        pure . toolText $
                          [i|已发到 QQ群 #{member.memberGroup}：@#{member.memberUserId}，内容：#{body}|]
                      else do
                        let err = Text.intercalate "；" (lefts sent)
                        pure (toolFailure Failure.Failure
                          { category = Failure.ExternalServiceUnavailable
                          , userMessage = [i|发送失败：没能把话送到 QQ群 #{member.memberGroup}。|]
                          , detail = err
                          })

mentionBody :: Integer -> Text -> Text
mentionBody userId body =
  "[CQ:at,qq=" <> show userId <> "] " <> body

-- | Owner only, and only from outside a group chat.
tellMemberGuard :: Context -> Bool
tellMemberGuard context =
  context.superuser
    && context.message.platform == PlatformQQ
    && context.message.kind == ChatPrivate

explicitTellRequest :: Text -> Bool
explicitTellRequest value =
  let normalized = normalizedName (Text.toCaseFold value)
  in any (`Text.isInfixOf` normalized)
      [ "告诉", "传话", "转告", "通知", "说一声", "回复" ]

resolveCandidates :: Chat.Chat :> es => Context -> Text -> Maybe Text -> Eff es [RelayMember]
resolveCandidates context rawTarget hint =
  case resolveGroups hint of
    [] -> pure []
    groups ->
      case readMaybe (toString (Text.strip rawTarget)) :: Maybe Integer of
        Just userId | userId > 0 ->
          pure [RelayMember userId (show userId) group | group <- groups]
        _ -> do
          let target = normalizedName rawTarget
          found <- concat <$> traverse (membersMatching context target) groups
          pure (dedupe (filter (matchesHint hint) found))

-- | Which groups to search. A numeric hint narrows to that group; a non-numeric
-- hint that matches no group id yields no candidates, so the caller reports the
-- mismatch instead of guessing.
resolveGroups :: Maybe Text -> [Integer]
resolveGroups Nothing = knownQqGroups
resolveGroups (Just hint) =
  case readMaybe (toString hint) :: Maybe Integer of
    Just groupId -> [groupId]
    Nothing -> []

matchesHint :: Maybe Text -> RelayMember -> Bool
matchesHint Nothing _ = True
matchesHint (Just hint) member =
  hint == show member.memberGroup || hint == normalizedName member.memberName

membersMatching :: Chat.Chat :> es => Context -> Text -> Integer -> Eff es [RelayMember]
membersMatching context target groupId = do
  let probe =
        context.message
          { platform = PlatformQQ
          , kind = ChatGroup
          , chatId = Just groupId
          , chatAliases = []
          }
  listed <- Chat.listGroupMembers probe
  pure $ case listed of
    Nothing -> []
    Just value ->
      [ RelayMember{memberUserId = userId, memberName = name, memberGroup = groupId}
      | (userId, name) <- parseMemberArray value
      , target `Text.isInfixOf` normalizedName name
      ]

parseMemberArray :: Aeson.Value -> [(Integer, Text)]
parseMemberArray value =
  case value of
    Aeson.Array items -> mapMaybe parseMember (toList items)
    _ -> []
  where
    parseMember item = case item of
      Aeson.Object obj -> do
        userId <- KeyMap.lookup "user_id" obj >>= asInteger
        name <-
          nonEmptyText (KeyMap.lookup "card" obj >>= asText)
            <|> nonEmptyText (KeyMap.lookup "nickname" obj >>= asText)
        pure (userId, name)
      _ -> Nothing
    asInteger = \case
      Aeson.Number number -> Just (round number)
      Aeson.String text -> readMaybe (toString text)
      _ -> Nothing
    asText = \case
      Aeson.String text -> Just text
      _ -> Nothing
    nonEmptyText value = do
      text <- value
      let stripped = Text.strip text
      if Text.null stripped then Nothing else Just stripped

dedupe :: [RelayMember] -> [RelayMember]
dedupe = go []
  where
    go seen [] = reverse seen
    go seen (member : rest)
      | any (same member) seen = go seen rest
      | otherwise = go (member : seen) rest
    same a b = a.memberUserId == b.memberUserId && a.memberGroup == b.memberGroup

-- | Exactly one answer, or an explanation of what to clarify.
chooseCandidate :: [RelayMember] -> Either Text RelayMember
chooseCandidate [] =
  Left "没找到这个人：FM 在群里按昵称和群名片都没匹配到。可以把对方的 QQ 号给我，或者核对一下昵称。"
chooseCandidate [only] = Right only
chooseCandidate candidates =
  case ordNub (fmap memberUserId candidates) of
    [onlyUser] ->
      case ordNub (fmap memberGroup candidates) of
        [onlyGroup] ->
          Right
            RelayMember
              { memberUserId = onlyUser
              , memberName = fromMaybe "" (viaNonEmpty head (fmap memberName candidates))
              , memberGroup = onlyGroup
              }
        groups ->
          Left . Text.unlines $
            [ [i|这个人在 #{length groups} 个群里都有记录，FM 不确定发哪边，所以没有发。|]
            , "请指定群号："
            ]
              <> fmap (\group -> "  - QQ群 " <> show group) groups
    users ->
      Left . Text.unlines $
        [ [i|有 #{length users} 个人匹配，FM 不猜，所以没有发。|]
        , "请给 QQ 号，或说得更具体："
        ]
          <> fmap
            (\member -> "  - " <> show member.memberUserId <> "  " <> member.memberName)
            (take 8 candidates)
