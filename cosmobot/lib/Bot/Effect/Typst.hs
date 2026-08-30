{-|
Module      : Bot.Effect.Typst
Description : Typst rendering capability facade
Stability   : experimental
-}
module Bot.Effect.Typst
  ( Typst(..)
  , withTypst
  , withTypstPng
  , withTypstPdf
  )
where

import Bot.Prelude
import Bot.System.Typst.Types

-- | Render Typst source into a PNG available for the duration of a continuation.
data Typst :: Effect where
  WithTypst
    :: TypstOutputFormat
    -> Text
    -> (FilePath -> m a)
    -> Typst m a

type instance DispatchOf Typst = Dynamic

withTypst :: Typst :> es => TypstOutputFormat -> Text -> (FilePath -> Eff es a) -> Eff es a
withTypst format source action =
  send (WithTypst format source action)

withTypstPng :: Typst :> es => Text -> (FilePath -> Eff es a) -> Eff es a
withTypstPng = withTypst TypstOutputPNG

withTypstPdf :: Typst :> es => Text -> (FilePath -> Eff es a) -> Eff es a
withTypstPdf = withTypst TypstOutputPDF

