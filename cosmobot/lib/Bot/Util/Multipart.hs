{-|
Module      : Bot.Util.Multipart
Description : Multipart form helpers
Stability   : experimental
-}

module Bot.Util.Multipart
  ( textPart
  , maybePart
  )
where

import Bot.Prelude
import qualified Data.Text.Encoding as TextEncoding
import qualified Network.HTTP.Client.MultipartFormData as Multipart

textPart :: Text -> Text -> Multipart.Part
textPart name value =
  Multipart.partBS name (TextEncoding.encodeUtf8 value)

maybePart :: Text -> Maybe Text -> [Multipart.Part]
maybePart name =
  maybe [] \value -> [textPart name value]
