{-|
Module      : Bot.Agent.Tools.Media
Description : Agent tools for cached media objects
Stability   : experimental
-}

module Bot.Agent.Tools.Media
  ( readMediaTextTool
  , mediaToFileTool
  , sendMediaTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Agent.Types
import Bot.Core.Message
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Media as Media
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as StrictByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncoding
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import Effectful.FileSystem (FileSystem)

defaultReadSize :: Int
defaultReadSize =
  4096

maxReadSize :: Int
maxReadSize =
  16384

readMediaTextTool :: (Media.Media :> es, FileSystem :> es) => Tool (Eff es)
readMediaTextTool =
  tagged [workTag]
  . withDescription "Read a UTF-8 text slice from a cached media object. Use this with media ids returned in omitted tool results, such as mf_xxx or media:mf_xxx. offset and size are character counts."
  $ tool "media_text"
      ( validateArgument nonEmptyMediaId
          (requiredText "media_id" "Media id to read, either mf_xxx or media:mf_xxx.")
      , validateArgument (nonNegativeInt "offset")
          (withDefault 0 (optionalInteger "offset" "Optional zero-based character offset. Defaults to 0."))
      , validateArgument cappedReadSize
          (withDefault (fromIntegral defaultReadSize) (optionalInteger "size" "Optional maximum number of characters to return. Defaults to 4096 and is capped at 16384."))
      )
      \mediaId offset size ->
        readMediaText ReadMediaTextArgs{mediaId, offset, size}

mediaToFileTool :: Media.Media :> es => Tool (Eff es)
mediaToFileTool =
  tagged [workTag]
  . withDescription "Resolve a cached media object to its existing local cache file path. The file is not attached to agent context."
  $ tool "media_to_file"
      (requiredText "media_id" "Media id to resolve, either mf_xxx or media:mf_xxx.")
      (resolveMediaPath . Text.strip)

sendMediaTool :: (Chat.Chat :> es, Media.Media :> es) => Tool (Eff es)
sendMediaTool =
  tagged [chatTag]
  . allowWhen hasExplicitMediaSendIntent
  . withDescription "Send a cached media object to the current chat. Use this when the user asks for a generated or cached file to be sent."
  $ tool "send_media"
      ( requiredText "media_id" "Media id to send, either mf_xxx or media:mf_xxx."
      , optionalText "filename" "Optional filename shown to the user. Defaults to the cached file name."
      )
      \mediaId fileName -> do
        context <- askToolContext
        if not (hasExplicitMediaSendIntent context)
          then pure (toolText "未发送媒体：当前消息没有明确的发送文件或媒体请求。")
          else sendMedia context (Text.strip mediaId) (Text.strip <$> fileName)

sendMedia :: (Chat.Chat :> es, Media.Media :> es) => Context -> Text -> Maybe Text -> Eff es ToolResult
sendMedia context mediaId fileName
  | Text.null mediaId =
      pure (mediaFailure "media_id must not be empty.")
  | otherwise = Media.localMediaPath (mediaRef mediaId) >>= \case
      Nothing ->
        pure (mediaFailure [i|Media object not found: #{mediaId}|])
      Just path -> Chat.uploadFile context.message path fileName >>= \case
        Right sent ->
          pure (toolText [i|Sent media #{mediaId}; message id: #{show sent :: Text}|])
        Left err -> do
          let failureText = "发送媒体失败：" <> err
          void $ Chat.replyTo context.message failureText
          pure (toolFailure Failure
            { category = ExternalServiceUnavailable
            , userMessage = failureText
            , detail = err
            })

mediaFailure :: Text -> ToolResult
mediaFailure message =
  toolFailure (permanentArgumentFailure message message)

hasExplicitMediaSendIntent :: Context -> Bool
hasExplicitMediaSendIntent context =
  any (`Text.isInfixOf` normalized)
    [ "发文件", "发送文件", "发这个文件", "发附件", "发送附件", "发媒体", "发送媒体"
    , "把这个发出去", "把它发出去" ]
  where
    normalized = Text.toLower context.input.text

resolveMediaPath :: Media.Media :> es => Text -> Eff es ToolResult
resolveMediaPath mediaId
  | Text.null mediaId = pure (toolText "media_id must not be empty.")
  | otherwise = Media.localMediaPath (mediaRef mediaId) <&> \case
      Nothing -> toolText [i|Media object not found: #{mediaId}|]
      Just path -> toolText (jsonText (Aeson.object ["path" Aeson..= path]))

data ReadMediaTextArgs = ReadMediaTextArgs
  { mediaId :: !Text
  , offset :: !Int
  , size :: !Int
  }

nonEmptyMediaId :: Text -> Either Text Text
nonEmptyMediaId rawMediaId
  | Text.null mediaId =
      Left "media_id must not be empty."
  | otherwise =
      Right mediaId
  where
    mediaId = Text.strip rawMediaId

nonNegativeInt :: Text -> Integer -> Either Text Int
nonNegativeInt name value
  | value < 0 =
      Left [i|#{name} must be >= 0.|]
  | value > fromIntegral (maxBound :: Int) =
      Left [i|#{name} is too large.|]
  | otherwise =
      Right (fromInteger value)

cappedReadSize :: Integer -> Either Text Int
cappedReadSize =
  fmap (min maxReadSize) . nonNegativeInt "size"

readMediaText :: (Media.Media :> es, FileSystem :> es) => ReadMediaTextArgs -> Eff es ToolResult
readMediaText ReadMediaTextArgs{mediaId, offset, size} = do
  info <- Media.mediaFileInfoByRef ref
  path <- Media.localMediaPath ref
  case (info, path) of
    (_, Nothing) ->
      pure (toolText [i|Media object not found: #{mediaId}|])
    (Just mediaInfo, Just filePath) -> do
      bytes <- FileSystemByteString.readFile filePath
      let text = TextEncoding.decodeUtf8With TextEncoding.lenientDecode bytes
          chunk = Text.take size (Text.drop offset text)
      pure (toolText (jsonText (readMediaTextResult mediaInfo bytes text chunk offset size)))
    (Nothing, Just filePath) -> do
      bytes <- FileSystemByteString.readFile filePath
      let text = TextEncoding.decodeUtf8With TextEncoding.lenientDecode bytes
          chunk = Text.take size (Text.drop offset text)
      pure (toolText (jsonText (readMediaTextResultWithoutInfo mediaId bytes text chunk offset size)))
  where
    ref = mediaRef mediaId

mediaRef :: Text -> Text
mediaRef mediaId
  | "media:" `Text.isPrefixOf` mediaId = mediaId
  | otherwise = "media:" <> mediaId

readMediaTextResult :: Media.MediaFileInfo -> StrictByteString.ByteString -> Text -> Text -> Int -> Int -> Aeson.Value
readMediaTextResult info bytes text chunk offset size =
  Aeson.object
    [ "media_id" Aeson..= info.fileId
    , "mime" Aeson..= info.mimeType
    , "byte_size" Aeson..= StrictByteString.length bytes
    , "total_chars" Aeson..= Text.length text
    , "offset" Aeson..= offset
    , "requested_size" Aeson..= size
    , "returned_chars" Aeson..= Text.length chunk
    , "content" Aeson..= chunk
    ]

readMediaTextResultWithoutInfo :: Text -> StrictByteString.ByteString -> Text -> Text -> Int -> Int -> Aeson.Value
readMediaTextResultWithoutInfo mediaId bytes text chunk offset size =
  Aeson.object
    [ "media_id" Aeson..= mediaId
    , "byte_size" Aeson..= StrictByteString.length bytes
    , "total_chars" Aeson..= Text.length text
    , "offset" Aeson..= offset
    , "requested_size" Aeson..= size
    , "returned_chars" Aeson..= Text.length chunk
    , "content" Aeson..= chunk
    ]
