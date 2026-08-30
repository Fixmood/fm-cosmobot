{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.Util.Image
Description : Shared image helpers
Stability   : experimental
-}

module Bot.Util.Image
  ( ImageCompressionConfig (..)
  , removeFilesIfExists
  )
where

import Bot.Prelude
import Effectful.FileSystem (FileSystem, removeFile)

data ImageCompressionConfig = ImageCompressionConfig
  { compressionFormat :: !(Maybe Text)
  , compressionLevel :: !(Maybe Int)
  }
  deriving (Show, Eq)

removeFilesIfExists :: (IOE :> es, Fail :> es, FileSystem :> es) => [FilePath] -> Eff es ()
removeFilesIfExists =
  traverse_ removeIfExists

removeIfExists :: (Fail :> es, FileSystem :> es) => FilePath -> Eff es ()
removeIfExists path =
  removeFile path `catchSync` \_ -> pure ()
