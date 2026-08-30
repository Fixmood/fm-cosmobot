{-|
Module      : Bot.Core.ReplyBody
Description : Reply body directives shared by chat backends
Stability   : experimental
-}

module Bot.Core.ReplyBody
  ( ReplyContent (..)
  , imageDirective
  , replyContentFromBody
  , replyContentToBody
  , renderReplyBody
  , replyImageUrls
  , isBase64ImageRef
  , traverseReplyImageUrls
  )
where

import Bot.Prelude
import qualified Data.Text as Text

data ReplyContent = ReplyContent
  { text :: !Text
  , images :: ![Text]
  }
  deriving (Eq, Show)

imageDirective :: Text -> Text
imageDirective ref =
  "[image] " <> ref

replyContentFromBody :: Text -> ReplyContent
replyContentFromBody body =
  ReplyContent
    { text = Text.strip (Text.unlines textLines)
    , images = mapMaybe imageLineUrl lines_
    }
  where
    lines_ = Text.lines body
    textLines = filter (not . isImageLine) lines_

replyContentToBody :: ReplyContent -> Text
replyContentToBody ReplyContent{text, images} =
  Text.strip (Text.unlines (textLines <> imageLines))
  where
    textLines =
      [text | not (Text.null (Text.strip text))]
    imageLines =
      map imageDirective images

-- | Remove image directives from a reply body before storing it as text.
renderReplyBody :: Text -> Text
renderReplyBody =
  (.text) . replyContentFromBody

-- | Extract image URLs from @\[image\] ...@ reply directives.
replyImageUrls :: Text -> [Text]
replyImageUrls =
  (.images) . replyContentFromBody

isBase64ImageRef :: Text -> Bool
isBase64ImageRef ref =
  "data:image/" `Text.isPrefixOf` Text.strip ref &&
    ";base64," `Text.isInfixOf` ref

traverseReplyImageUrls :: Applicative f => (Text -> f Text) -> Text -> f Text
traverseReplyImageUrls update body =
  Text.unlines <$> traverse updateLine (Text.lines body)
  where
    updateLine line =
      case imageLineUrl line of
        Nothing ->
          pure line
        Just ref ->
          imageDirective <$> update ref

isImageLine :: Text -> Bool
isImageLine =
  isJust . imageLineUrl

imageLineUrl :: Text -> Maybe Text
imageLineUrl line =
  let marker = "[image] "
      stripped = Text.strip line
  in Text.strip <$> Text.stripPrefix marker stripped
