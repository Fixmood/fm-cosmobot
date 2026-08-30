{-|
Module      : Bot.ACP.Content
Description : ACP content block parsing and rendering
Stability   : experimental
-}

module Bot.ACP.Content
  ( PromptContent (..)
  , AcpContentBlock
  , parsePromptContent
  , messageContentBlocks
  , textContentBlock
  , contentBlockValue
  )
where

import Bot.Prelude
import qualified Bot.Session as Session
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import System.FilePath (takeFileName)

data PromptContent = PromptContent
  { text :: !Text
  , imageUrls :: ![Text]
  }
  deriving (Eq, Show)

data AcpContentBlock
  = AcpTextBlock !Text
  | AcpImageBlock !Text !Text
  | AcpResourceLinkBlock !Text !Text !(Maybe Text) !(Maybe Int)
  deriving (Eq, Show)

parsePromptContent :: Aeson.Value -> AesonTypes.Parser PromptContent
parsePromptContent =
  Aeson.withArray "prompt" \blocks -> do
    parsed <- traverse parsePromptBlock (toList blocks)
    pure PromptContent
      { text = Text.intercalate "\n" [text | Left text <- parsed]
      , imageUrls = ordNub [imageRef | Right imageRef <- parsed]
      }

parsePromptBlock :: Aeson.Value -> AesonTypes.Parser (Either Text Text)
parsePromptBlock =
  Aeson.withObject "content block" \o -> do
    contentType <- o Aeson..: "type"
    case contentType of
      "text" ->
        Left <$> o Aeson..: "text"
      "image" -> do
        mimeType <- o Aeson..: "mimeType"
        imageData <- o Aeson..: "data"
        unless ("image/" `Text.isPrefixOf` Text.toLower mimeType) $
          fail [i|ACP image content block has non-image MIME type: #{mimeType}|]
        when (Text.null imageData) $
          fail "ACP image content block has empty data"
        pure (Right (dataImageRef mimeType imageData))
      _ ->
        fail [i|unsupported ACP content block type: #{contentType :: Text}|]

messageContentBlocks :: Text -> [Text] -> [Session.SessionAttachmentRef] -> [AcpContentBlock]
messageContentBlocks text imageUrls attachments =
  textBlocks <> map imageRefContentBlock imageUrls <> mapMaybe attachmentContentBlock attachments
  where
    textBlocks =
      [textContentBlock text | not (Text.null (Text.strip text))]

textContentBlock :: Text -> AcpContentBlock
textContentBlock =
  AcpTextBlock

contentBlockValue :: AcpContentBlock -> Aeson.Value
contentBlockValue = \case
  AcpTextBlock text ->
    Aeson.object
      [ "type" Aeson..= ("text" :: Text)
      , "text" Aeson..= text
      ]
  AcpImageBlock mimeType imageData ->
    Aeson.object
      [ "type" Aeson..= ("image" :: Text)
      , "mimeType" Aeson..= mimeType
      , "data" Aeson..= imageData
      ]
  AcpResourceLinkBlock uri name mimeType size ->
    Aeson.object $
      [ "type" Aeson..= ("resource_link" :: Text)
      , "uri" Aeson..= uri
      , "name" Aeson..= name
      ]
        <> maybe [] (\value -> ["mimeType" Aeson..= value]) mimeType
        <> maybe [] (\value -> ["size" Aeson..= value]) size

imageRefContentBlock :: Text -> AcpContentBlock
imageRefContentBlock ref =
  case parseDataImageRef ref of
    Just (mimeType, imageData) ->
      AcpImageBlock mimeType imageData
    Nothing ->
      AcpResourceLinkBlock ref (resourceName ref) (guessImageMimeType ref) Nothing

attachmentContentBlock :: Session.SessionAttachmentRef -> Maybe AcpContentBlock
attachmentContentBlock attachment
  | attachment.kind == "image" =
      Just $
        case parseDataImageRef attachment.url of
          Just (mimeType, imageData) ->
            AcpImageBlock mimeType imageData
          Nothing ->
            AcpResourceLinkBlock attachment.url attachment.name (Just attachment.mediaType) (Just attachment.size)
  | otherwise =
      Nothing

parseDataImageRef :: Text -> Maybe (Text, Text)
parseDataImageRef ref = do
  withoutPrefix <- Text.stripPrefix "data:" (Text.strip ref)
  let (mimeType, rest) = Text.breakOn ";base64," withoutPrefix
  guard (not (Text.null mimeType))
  imageData <- Text.stripPrefix ";base64," rest
  guard ("image/" `Text.isPrefixOf` Text.toLower mimeType)
  pure (mimeType, imageData)

dataImageRef :: Text -> Text -> Text
dataImageRef mimeType imageData =
  "data:" <> mimeType <> ";base64," <> imageData

resourceName :: Text -> Text
resourceName ref =
  let name = Text.pack (takeFileName (Text.unpack ref))
  in if Text.null name then "image" else name

guessImageMimeType :: Text -> Maybe Text
guessImageMimeType ref =
  case Text.toLower ref of
    value
      | ".avif" `Text.isSuffixOf` value -> Just "image/avif"
      | ".gif" `Text.isSuffixOf` value -> Just "image/gif"
      | ".jpeg" `Text.isSuffixOf` value -> Just "image/jpeg"
      | ".jpg" `Text.isSuffixOf` value -> Just "image/jpeg"
      | ".png" `Text.isSuffixOf` value -> Just "image/png"
      | ".webp" `Text.isSuffixOf` value -> Just "image/webp"
      | otherwise -> Nothing
