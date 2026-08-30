{-|
Module      : Bot.Media.Mime
Description : MIME type lookup and lightweight content sniffing
Stability   : experimental
-}

module Bot.Media.Mime
  ( mimeFromName
  , extensionFromMime
  , sniffMime
  , sniffTextMime
  , isProbablyMediaMime
  , isGenericMime
  )
where

import Bot.Prelude
import qualified Data.Aeson as Aeson
import Data.Bits ((.&.))
import qualified Data.ByteString as StrictByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Network.Mime as Mime

mimeFromName :: Text -> Text
mimeFromName name =
  TextEncoding.decodeUtf8 (Mime.defaultMimeLookup name)

extensionFromMime :: Text -> Text
extensionFromMime mime
  | cleanMime mime == "image/jpeg" =
      ".jpg"
  | otherwise =
      maybe ".bin" ("." <>) (viaNonEmpty head =<< Map.lookup (TextEncoding.encodeUtf8 (cleanMime mime)) Mime.defaultExtensionMap)

sniffMime :: StrictByteString.ByteString -> Maybe Text
sniffMime bytes
  | "\x89PNG\r\n\x1a\n" `StrictByteString.isPrefixOf` bytes = Just "image/png"
  | "\xff\xd8\xff" `StrictByteString.isPrefixOf` bytes = Just "image/jpeg"
  | "GIF87a" `StrictByteString.isPrefixOf` bytes || "GIF89a" `StrictByteString.isPrefixOf` bytes = Just "image/gif"
  | "RIFF" `StrictByteString.isPrefixOf` bytes && "WEBP" `StrictByteString.isPrefixOf` StrictByteString.drop 8 bytes = Just "image/webp"
  | "RIFF" `StrictByteString.isPrefixOf` bytes && "WAVE" `StrictByteString.isPrefixOf` StrictByteString.drop 8 bytes = Just "audio/wav"
  | "ID3" `StrictByteString.isPrefixOf` bytes || mp3FrameSync bytes = Just "audio/mpeg"
  | "fLaC" `StrictByteString.isPrefixOf` bytes = Just "audio/flac"
  | "OggS" `StrictByteString.isPrefixOf` bytes = Just "audio/ogg"
  | "\x1a\x45\xdf\xa3" `StrictByteString.isPrefixOf` bytes = Just "video/webm"
  | "ftyp" `StrictByteString.isPrefixOf` StrictByteString.drop 4 bytes = Just "video/mp4"
  | "%PDF-" `StrictByteString.isPrefixOf` bytes = Just "application/pdf"
  | "\x50\x4b\x03\x04" `StrictByteString.isPrefixOf` bytes = Just "application/zip"
  | "\x1f\x8b" `StrictByteString.isPrefixOf` bytes = Just "application/gzip"
  | "7z\xbc\xaf\x27\x1c" `StrictByteString.isPrefixOf` bytes = Just "application/x-7z-compressed"
  | "Rar!\x1a\x07" `StrictByteString.isPrefixOf` bytes = Just "application/x-rar-compressed"
  | otherwise = Nothing

sniffTextMime :: StrictByteString.ByteString -> Text -> Text
sniffTextMime bytes text =
  case Aeson.eitherDecodeStrict' bytes :: Either String Aeson.Value of
    Right _ ->
      "application/json"
    Left _ ->
      sniffStructuredTextMime text

sniffStructuredTextMime :: Text -> Text
sniffStructuredTextMime text
  | "<!doctype html" `Text.isPrefixOf` lowerStripped || "<html" `Text.isPrefixOf` lowerStripped =
      "text/html; charset=utf-8"
  | "<?xml" `Text.isPrefixOf` lowerStripped =
      "application/xml"
  | otherwise =
      "text/plain; charset=utf-8"
  where
    lowerStripped = Text.toLower (Text.stripStart text)

isProbablyMediaMime :: Text -> Bool
isProbablyMediaMime mime =
  let mime' = cleanMime mime
  in any (`Text.isPrefixOf` mime')
      [ "image/"
      , "audio/"
      , "video/"
      ]
      || mime' `elem`
        [ "application/pdf"
        , "application/gzip"
        , "application/octet-stream"
        , "application/zip"
        , "application/x-7z-compressed"
        , "application/x-rar-compressed"
        , "application/x-tar"
        ]

isGenericMime :: Text -> Bool
isGenericMime mime =
  cleanMime mime `elem`
    [ "application/octet-stream"
    , "binary/octet-stream"
    ]

cleanMime :: Text -> Text
cleanMime =
  Text.toLower . Text.strip . Text.takeWhile (/= ';')

mp3FrameSync :: StrictByteString.ByteString -> Bool
mp3FrameSync bytes =
  case StrictByteString.unpack (StrictByteString.take 2 bytes) of
    byte1 : byte2 : _ ->
      byte1 == 0xff && (byte2 .&. 0xe0) == 0xe0
    _ ->
      False
