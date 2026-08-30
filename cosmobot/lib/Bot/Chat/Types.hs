{-|
Module      : Bot.Chat.Types
Description : Shared chat domain types
Stability   : experimental
-}

module Bot.Chat.Types
  ( MessageOutPolicy (..)
  , MessageOutResult (..)
  )
where

import Bot.Core.Message
import Bot.Prelude

data MessageOutPolicy
  = EditableMessage !Int !Int
  | ChunkedMessage !Int

data MessageOutResult = MessageOutResult
  { responseId :: !(Maybe MessageId)
  , sentMessageResults :: ![Either Text MessageId]
  , answer :: !Text
  }
  deriving (Show)
