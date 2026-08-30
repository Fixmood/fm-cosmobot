{-|
Module      : Bot.System.Typst.Types
Description : Shared type definitions for Typst support
Stability   : experimental
-}

module Bot.System.Typst.Types
  ( TypstOutputFormat(..)
  , typstFormatToExtName
  )
where

import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

data TypstOutputFormat = TypstOutputPDF | TypstOutputPNG

typstFormatToExtName :: TypstOutputFormat -> Text
typstFormatToExtName TypstOutputPDF = "pdf"
typstFormatToExtName TypstOutputPNG = "png"

instance Aeson.ToJSON TypstOutputFormat where
  toJSON = Aeson.String . typstFormatToExtName

instance Aeson.FromJSON TypstOutputFormat where
  parseJSON = Aeson.withText "TypstOutputFormat" $ \case
    "pdf" -> pure TypstOutputPDF
    "png" -> pure TypstOutputPNG
    other -> Aeson.parseFail $ "Unknown TypstOutputFormat: " <> show other
