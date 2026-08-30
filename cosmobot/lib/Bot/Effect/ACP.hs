{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.Effect.ACP
Description : ACP client capability facade
Stability   : experimental
-}

module Bot.Effect.ACP
  ( ACP (..)
  , TerminalCreate (..)
  , TerminalExitStatus (..)
  , TerminalOutput (..)
  , readClientFile
  , writeClientFile
  , createClientTerminal
  , readClientTerminalOutput
  , waitForClientTerminalExit
  , killClientTerminal
  , releaseClientTerminal
  )
where

import Bot.Core.Message
import Bot.Prelude

data ACP :: Effect where
  ReadClientFile :: IncomingMessage -> Text -> Maybe Int -> Maybe Int -> ACP m (Either Text Text)
  WriteClientFile :: IncomingMessage -> Text -> Text -> ACP m (Either Text ())
  CreateClientTerminal :: IncomingMessage -> TerminalCreate -> ACP m (Either Text Text)
  ReadClientTerminalOutput :: IncomingMessage -> Text -> ACP m (Either Text TerminalOutput)
  WaitForClientTerminalExit :: IncomingMessage -> Text -> ACP m (Either Text TerminalExitStatus)
  KillClientTerminal :: IncomingMessage -> Text -> ACP m (Either Text ())
  ReleaseClientTerminal :: IncomingMessage -> Text -> ACP m (Either Text ())

type instance DispatchOf ACP = Dynamic

data TerminalCreate = TerminalCreate
  { command :: !Text
  , args :: ![Text]
  , env :: ![(Text, Text)]
  , cwd :: !(Maybe Text)
  , outputByteLimit :: !(Maybe Int)
  }
  deriving (Eq, Show)

data TerminalExitStatus = TerminalExitStatus
  { exitCode :: !(Maybe Int)
  , signal :: !(Maybe Text)
  }
  deriving (Eq, Show)

data TerminalOutput = TerminalOutput
  { output :: !Text
  , truncated :: !Bool
  , exitStatus :: !(Maybe TerminalExitStatus)
  }
  deriving (Eq, Show)

readClientFile :: ACP :> es => IncomingMessage -> Text -> Maybe Int -> Maybe Int -> Eff es (Either Text Text)
readClientFile message path line limit =
  send (ReadClientFile message path line limit)

writeClientFile :: ACP :> es => IncomingMessage -> Text -> Text -> Eff es (Either Text ())
writeClientFile message path content =
  send (WriteClientFile message path content)

createClientTerminal :: ACP :> es => IncomingMessage -> TerminalCreate -> Eff es (Either Text Text)
createClientTerminal message create =
  send (CreateClientTerminal message create)

readClientTerminalOutput :: ACP :> es => IncomingMessage -> Text -> Eff es (Either Text TerminalOutput)
readClientTerminalOutput message terminalId =
  send (ReadClientTerminalOutput message terminalId)

waitForClientTerminalExit :: ACP :> es => IncomingMessage -> Text -> Eff es (Either Text TerminalExitStatus)
waitForClientTerminalExit message terminalId =
  send (WaitForClientTerminalExit message terminalId)

killClientTerminal :: ACP :> es => IncomingMessage -> Text -> Eff es (Either Text ())
killClientTerminal message terminalId =
  send (KillClientTerminal message terminalId)

releaseClientTerminal :: ACP :> es => IncomingMessage -> Text -> Eff es (Either Text ())
releaseClientTerminal message terminalId =
  send (ReleaseClientTerminal message terminalId)
