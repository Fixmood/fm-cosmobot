{-|
Module      : Bot.System.Typst.CLI
Description : Typst CLI-backed renderer
Stability   : experimental
-}

module Bot.System.Typst.CLI
  ( runTypst
  , withRenderedTypst
  , typstOutputFileName
  , TypstOutputFormat
  )
where

import Bot.Prelude
import Bot.System.Typst.Types
import qualified Bot.Effect.Typst as Typst
import qualified Bot.Util.Image as Image
import qualified Bot.Util.Process as ProcessUtil
import qualified Crypto.Hash as CryptoHash
import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Effectful.FileSystem (FileSystem)
import qualified Effectful.Process.Typed as TypedProcess
import qualified Effectful.Temporary as Temporary
import System.Exit
import System.FilePath
import System.IO.Error (userError)

runTypst
  :: (IOE :> es, KatipE :> es, Fail :> es, FileSystem :> es, Concurrent :> es, TypedProcess.TypedProcess :> es)
  => Eff (Typst.Typst : es) a
  -> Eff es a
runTypst = interpret $ \localEnv operation ->
  localSeqUnlift localEnv \runLocal ->
    case operation of
      Typst.WithTypst format source action ->
        withRenderedTypst format source (runLocal . action)

withRenderedTypst
  :: (IOE :> es, KatipE :> es, Fail :> es, FileSystem :> es, Concurrent :> es, TypedProcess.TypedProcess :> es)
  => TypstOutputFormat
  -> Text
  -> (FilePath -> Eff es a)
  -> Eff es a
withRenderedTypst format source action =
  Temporary.runTemporary $
    Temporary.withSystemTempDirectory "cosmobot-typst-" \dir -> do
      imagePath <- renderTypst format dir source
      raise (action imagePath)

renderTypst :: (IOE :> es, KatipE :> es, Fail :> es, FileSystem :> es, Concurrent :> es, TypedProcess.TypedProcess :> es) => TypstOutputFormat -> FilePath -> Text -> Eff es FilePath
renderTypst format dir source = do
  let typstPath = dir </> "document" <.> "typ"
      outputPath = dir </> Text.unpack (typstOutputFileName format source)
  liftIO $ TextIO.writeFile typstPath source
  logInfo [i|Rendering Typst document: #{typstPath}|]
  (code, _out, err) <- ProcessUtil.readProcessGroupWithExitCode "typst" ["compile", "--root", dir, typstPath, outputPath]
  case code of
    ExitSuccess -> do
      logInfo [i|Rendered Typst document: #{outputPath}|]
      pure outputPath
    ExitFailure _ -> do
      Image.removeFilesIfExists [typstPath, outputPath]
      throwIO (userError ("typst failed: " <> Text.unpack err))

typstOutputFileName :: TypstOutputFormat -> Text -> Text
typstOutputFileName format source =
  "typst-" <> Text.take 16 digest <> "." <> typstFormatToExtName format
  where
    digest =
      Text.pack . show $
        (CryptoHash.hash (TextEncoding.encodeUtf8 source) :: CryptoHash.Digest CryptoHash.SHA256)
