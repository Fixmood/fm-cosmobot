{-|
Module      : Bot.Storage.RPC
Description : RPC-specific storage helpers over persistent chat sessions
Stability   : experimental
-}

module Bot.Storage.RPC
  ( referencedMediaFileIds
  )
where

import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import qualified Bot.Storage.Session as SessionStorage
import qualified Data.Text as Text

referencedMediaFileIds :: Storage.Storage :> es => Eff es [Text]
referencedMediaFileIds =
  pure . mapMaybe parseMediaId =<< SessionStorage.messageAttachmentIds

parseMediaId :: Text -> Maybe Text
parseMediaId ref = do
  fileId <- Text.stripPrefix "media:" (Text.strip ref)
  guard (not (Text.null fileId))
  pure fileId
