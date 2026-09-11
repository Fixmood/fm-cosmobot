{-|
Module      : Bot.Handler.Ask.Config
Description : Ask handler configuration
Stability   : experimental
-}

module Bot.Handler.Ask.Config
  ( AskHandlerConfig (..)
  )
where

import Bot.Prelude
import Bot.Core.Message (ChatPlatform)
import Toml.Schema

-- | Identity, command, and prompt settings for the ask handler.
data AskHandlerConfig = AskHandlerConfig
  { name             :: !(Maybe Text)
  , command          :: !Text
  , drawCommand      :: !Text
  , systemPrompt     :: !Text
  , agentMaxTurns    :: !Int
  , contextCompactionThresholdKTokens :: !Int
  , recentChatContextEnabled :: !Bool
  , recentChatContextLimit :: !Int
  , recentChatContextMinutes :: !Int
  , recentChatContextMaxChars :: !Int
  , recentChatContextDisabledGroups :: ![Integer]
  , botIds           :: ![(ChatPlatform, Text)]
  }
  deriving (Show)

instance FromValue AskHandlerConfig where
  fromValue = parseTableFromValue do
    name <- optKey "name"
    command <- reqKey "command"
    drawCommand <- fromMaybe "!draw" <$> optKey "draw_command"
    systemPrompt <- reqKey "system_prompt"
    agentMaxTurns <- fromMaybe 4 <$> optKey "agent_max_turns"
    contextCompactionThresholdKTokens <- fromMaybe 1000 <$> optKey "context_compaction_threshold_ktokens"
    recentChatContextEnabled <- fromMaybe True <$> optKey "recent_chat_context_enabled"
    recentChatContextLimit <- fromMaybe 30 <$> optKey "recent_chat_context_limit"
    recentChatContextMinutes <- fromMaybe 30 <$> optKey "recent_chat_context_minutes"
    recentChatContextMaxChars <- fromMaybe 2000 <$> optKey "recent_chat_context_max_chars"
    recentChatContextDisabledGroups <- fromMaybe [] <$> optKey "recent_chat_context_disabled_groups"
    when (contextCompactionThresholdKTokens <= 0) do
      fail "handler.ask.context_compaction_threshold_ktokens must be positive"
    when (recentChatContextLimit < 0 || recentChatContextLimit > 100) do
      fail "handler.ask.recent_chat_context_limit must be between 0 and 100"
    when (recentChatContextMinutes <= 0) do
      fail "handler.ask.recent_chat_context_minutes must be positive"
    when (recentChatContextMaxChars < 0) do
      fail "handler.ask.recent_chat_context_max_chars must not be negative"
    pure AskHandlerConfig
      { name = name
      , command = command
      , drawCommand = drawCommand
      , systemPrompt = systemPrompt
      , agentMaxTurns = agentMaxTurns
      , contextCompactionThresholdKTokens = contextCompactionThresholdKTokens
      , recentChatContextEnabled = recentChatContextEnabled
      , recentChatContextLimit = recentChatContextLimit
      , recentChatContextMinutes = recentChatContextMinutes
      , recentChatContextMaxChars = recentChatContextMaxChars
      , recentChatContextDisabledGroups = recentChatContextDisabledGroups
      , botIds = []
      }
