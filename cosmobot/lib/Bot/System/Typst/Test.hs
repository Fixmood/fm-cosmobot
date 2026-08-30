{-|
Module      : Bot.System.Typst.Test
Description : Test interpreter for Typst rendering
Stability   : experimental
-}

module Bot.System.Typst.Test
  ( runTypstWith
  )
where

import Bot.Prelude
import qualified Bot.Effect.Typst as Typst
import Bot.System.Typst.Types

runTypstWith
  :: (forall r. TypstOutputFormat -> Text -> (FilePath -> Eff es r) -> Eff es r)
  -> Eff (Typst.Typst : es) a
  -> Eff es a
runTypstWith render = interpret $ \localEnv operation ->
  localSeqUnlift localEnv \runLocal ->
    case operation of
      Typst.WithTypst format source action ->
        render format source (runLocal . action)
