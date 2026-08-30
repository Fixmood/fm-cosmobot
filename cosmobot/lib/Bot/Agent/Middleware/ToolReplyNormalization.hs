{-|
Module      : Bot.Agent.Middleware.ToolReplyNormalization
Description : Normalize tool-emitted reply media before sending
Stability   : experimental
-}

module Bot.Agent.Middleware.ToolReplyNormalization
  ( withNormalizingToolReplies
  )
where

import Bot.Agent.Core
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Media as Media
import Bot.Prelude
import qualified Data.Text as Text

withNormalizingToolReplies
  :: (Chat.Chat :> es, Media.Media :> es)
  => Runtime context (Eff es)
  -> Runtime context (Eff es)
withNormalizingToolReplies program =
  program
    { aroundToolCall = \turn call context action ->
        runChatNormalizingReplies $
          program.aroundToolCall turn call context action
    }

runChatNormalizingReplies
  :: (Chat.Chat :> es, Media.Media :> es)
  => Eff es a
  -> Eff es a
runChatNormalizingReplies =
  Chat.runChatMappingReplies normalizeReplyImages

normalizeReplyImages :: Media.Media :> es => Text -> Eff es (Either Text Text)
normalizeReplyImages body =
  if null (Chat.replyImageUrls body)
    then pure (Right body)
    else do
      normalized <- Media.normalizeReplyBody body
      let remainingRemoteRefs = filter isRemoteImageRef (Chat.replyImageUrls normalized)
      pure $
        if null remainingRemoteRefs
          then Right normalized
          else Left (uncachedImageReplyError remainingRemoteRefs)

isRemoteImageRef :: Text -> Bool
isRemoteImageRef ref =
  let stripped = Text.toLower (Text.strip ref)
  in "http://" `Text.isPrefixOf` stripped || "https://" `Text.isPrefixOf` stripped

uncachedImageReplyError :: [Text] -> Text
uncachedImageReplyError refs =
  [i|Image reply contains remote image URLs that could not be cached: #{Text.intercalate ", " refs}|]
