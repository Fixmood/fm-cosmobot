{-|
Module      : Bot.Util.Html
Description : Small HTML text helpers
Stability   : experimental
-}

module Bot.Util.Html
  ( htmlToPlainText
  , stripHtmlTags
  , htmlDecode
  )
where

import Bot.Prelude
import qualified Data.Text as Text

htmlToPlainText :: Text -> Text
htmlToPlainText =
  Text.unwords . Text.words . htmlDecode . stripHtmlTags

stripHtmlTags :: Text -> Text
stripHtmlTags =
  Text.pack . reverse . fst . Text.foldl' step ([], False)
  where
    step (acc, inTag) char =
      case (char, inTag) of
        ('<', _) -> (' ' : acc, True)
        ('>', _) -> (' ' : acc, False)
        (_, True) -> (acc, True)
        _ -> (char : acc, False)

htmlDecode :: Text -> Text
htmlDecode =
  Text.replace "&#39;" "'"
    . Text.replace "&quot;" "\""
    . Text.replace "&apos;" "'"
    . Text.replace "&gt;" ">"
    . Text.replace "&lt;" "<"
    . Text.replace "&nbsp;" " "
    . Text.replace "&amp;" "&"
