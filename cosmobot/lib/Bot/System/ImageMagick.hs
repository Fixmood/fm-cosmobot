{-|
Module      : Bot.System.ImageMagick
Description : ImageMagick-backed image transformations
Stability   : experimental
-}

module Bot.System.ImageMagick
  ( compressImageBytes
  )
where

import Bot.Prelude
import qualified Bot.Util.Image as Image
import qualified Bot.Util.Process as ProcessUtil
import qualified Data.ByteString as StrictByteString
import qualified Data.Text as Text
import Effectful.FileSystem (FileSystem, getTemporaryDirectory)
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import Effectful.FileSystem.IO (hClose)
import qualified Effectful.Process.Typed as TypedProcess
import System.Exit (ExitCode (..))
import System.FilePath ((<.>))
import System.IO (openBinaryTempFile)

compressImageBytes
  :: (IOE :> es, Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Fail :> es)
  => Image.ImageCompressionConfig
  -> StrictByteString.ByteString
  -> Eff es (Maybe (Text, StrictByteString.ByteString))
compressImageBytes cfg bytes =
  case targetImageFormat cfg of
    Nothing ->
      pure Nothing
    Just format ->
      convertImageBytes format cfg.compressionLevel bytes

targetImageFormat :: Image.ImageCompressionConfig -> Maybe Text
targetImageFormat cfg =
  case Text.toLower . Text.strip <$> cfg.compressionFormat of
    Just "jpeg" -> Just "jpg"
    Just "jpg" -> Just "jpg"
    Just "webp" -> Just "webp"
    _ -> Nothing

convertImageBytes :: (IOE :> es, Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Fail :> es) => Text -> Maybe Int -> StrictByteString.ByteString -> Eff es (Maybe (Text, StrictByteString.ByteString))
convertImageBytes format quality bytes = do
  output <- convertImageBytesToFile format quality bytes
  forM output \outputPath -> do
    outputBytes <- FileSystemByteString.readFile outputPath
    Image.removeFilesIfExists [outputPath]
    pure (mimeForImageFormat format, outputBytes)

convertImageBytesToFile :: (IOE :> es, Concurrent :> es, FileSystem :> es, TypedProcess.TypedProcess :> es, Fail :> es) => Text -> Maybe Int -> StrictByteString.ByteString -> Eff es (Maybe FilePath)
convertImageBytesToFile format quality bytes = do
  dir <- getTemporaryDirectory
  (inputPath, inputHandle) <- liftIO $ openBinaryTempFile dir ("cosmobot-image-input" <.> "png")
  liftIO $ StrictByteString.hPut inputHandle bytes
  hClose inputHandle
  (outputPath, outputHandle) <- liftIO $ openBinaryTempFile dir ("cosmobot-image-output" <.> Text.unpack format)
  hClose outputHandle
  let args =
        [ inputPath
        , "-auto-orient"
        , "-strip"
        ]
          <> maybe [] (\value -> ["-quality", show (clampImageQuality value)]) quality
          <> [outputPath]
  (code, _out, _err) <- ProcessUtil.readProcessGroupWithExitCode "magick" args
  Image.removeFilesIfExists [inputPath]
  case code of
    ExitSuccess ->
      pure (Just outputPath)
    ExitFailure _ -> do
      Image.removeFilesIfExists [outputPath]
      pure Nothing

mimeForImageFormat :: Text -> Text
mimeForImageFormat format =
  case Text.toLower (Text.strip format) of
    "jpg" -> "image/jpeg"
    "jpeg" -> "image/jpeg"
    "webp" -> "image/webp"
    "png" -> "image/png"
    other -> "image/" <> other

clampImageQuality :: Int -> Int
clampImageQuality =
  min 100 . max 1
