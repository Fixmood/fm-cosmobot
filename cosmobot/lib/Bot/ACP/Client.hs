{-|
Module      : Bot.ACP.Client
Description : ACP client capability interpreter
Stability   : experimental
-}

module Bot.ACP.Client
  ( runACP
  )
where

import qualified Bot.ACP.State as State
import Bot.Core.Message
import qualified Bot.Effect.ACP as ACP
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import qualified Bot.Session as Session
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import System.FilePath ((</>), isAbsolute)

runACP :: (Concurrent :> es, IOE :> es, Storage.Storage :> es) => State.AcpState -> Eff (ACP.ACP : es) a -> Eff es a
runACP acpState =
  interpret \_ -> \case
    ACP.ReadClientFile message path line limit ->
      clientRequest acpState message "fs/read_text_file" (.readTextFile) (readTextFileParams path line limit) parseReadTextFileResult
    ACP.WriteClientFile message path content ->
      clientRequest acpState message "fs/write_text_file" (.writeTextFile) (writeTextFileParams path content) parseUnitResult
    ACP.CreateClientTerminal message create ->
      clientRequest acpState message "terminal/create" (.terminal) (terminalCreateParams create) parseTerminalCreateResult
    ACP.ReadClientTerminalOutput message terminalId ->
      clientRequest acpState message "terminal/output" (.terminal) (terminalIdParams terminalId) parseTerminalOutputResult
    ACP.WaitForClientTerminalExit message terminalId ->
      clientRequest acpState message "terminal/wait_for_exit" (.terminal) (terminalIdParams terminalId) parseTerminalExitStatus
    ACP.KillClientTerminal message terminalId ->
      clientRequest acpState message "terminal/kill" (.terminal) (terminalIdParams terminalId) parseUnitResult
    ACP.ReleaseClientTerminal message terminalId ->
      clientRequest acpState message "terminal/release" (.terminal) (terminalIdParams terminalId) parseUnitResult

clientRequest
  :: (Concurrent :> es, IOE :> es, Storage.Storage :> es)
  => State.AcpState
  -> IncomingMessage
  -> Text
  -> (State.AcpClientCapabilities -> Bool)
  -> (Text -> Maybe Text -> Aeson.Value)
  -> (Aeson.Value -> AesonTypes.Parser a)
  -> Eff es (Either Text a)
clientRequest acpState message method supported params parseResult
  | message.platform /= PlatformACP =
      pure (Left "ACP client tools are only available in ACP sessions.")
  | otherwise =
      case listToMaybe message.chatAliases of
        Nothing ->
          pure (Left "ACP session id is unavailable for this message.")
        Just sessionIdText ->
          sessionCwd (Session.SessionId sessionIdText) >>= \cwd ->
            State.requestSessionClient acpState (Session.SessionId sessionIdText) method supported (params sessionIdText cwd) >>= \case
            Left err ->
              pure (Left err)
            Right result ->
              pure (first Text.pack (AesonTypes.parseEither parseResult result))

sessionCwd :: Storage.Storage :> es => Session.SessionId -> Eff es (Maybe Text)
sessionCwd sessionId =
  Session.getSession sessionId <&> (>>= (.label))

readTextFileParams :: Text -> Maybe Int -> Maybe Int -> Text -> Maybe Text -> Aeson.Value
readTextFileParams path line limit sessionId cwd =
  Aeson.object $
    [ "sessionId" Aeson..= sessionId
    , "path" Aeson..= clientPath cwd path
    ]
      <> maybe [] (\value -> ["line" Aeson..= value]) line
      <> maybe [] (\value -> ["limit" Aeson..= value]) limit

writeTextFileParams :: Text -> Text -> Text -> Maybe Text -> Aeson.Value
writeTextFileParams path content sessionId cwd =
  Aeson.object
    [ "sessionId" Aeson..= sessionId
    , "path" Aeson..= clientPath cwd path
    , "content" Aeson..= content
    ]

terminalCreateParams :: ACP.TerminalCreate -> Text -> Maybe Text -> Aeson.Value
terminalCreateParams create sessionId defaultCwd =
  Aeson.object $
    [ "sessionId" Aeson..= sessionId
    , "command" Aeson..= create.command
    , "args" Aeson..= create.args
    , "env" Aeson..=
        [ Aeson.object
            [ "name" Aeson..= name
            , "value" Aeson..= value
            ]
        | (name, value) <- create.env
        ]
    ]
      <> maybe [] (\cwd -> ["cwd" Aeson..= cwd]) (create.cwd <|> defaultCwd)
      <> maybe [] (\limit -> ["outputByteLimit" Aeson..= limit]) create.outputByteLimit

terminalIdParams :: Text -> Text -> Maybe Text -> Aeson.Value
terminalIdParams terminalId sessionId _cwd =
  Aeson.object
    [ "sessionId" Aeson..= sessionId
    , "terminalId" Aeson..= terminalId
    ]

clientPath :: Maybe Text -> Text -> Text
clientPath cwd rawPath
  | isAbsolute path =
      rawPath
  | Just cwdText <- cwd
  , isAbsolute (Text.unpack cwdText) =
      Text.pack (Text.unpack cwdText </> path)
  | otherwise =
      rawPath
  where
    path = Text.unpack rawPath

parseReadTextFileResult :: Aeson.Value -> AesonTypes.Parser Text
parseReadTextFileResult =
  Aeson.withObject "fs/read_text_file result" (Aeson..: "content")

parseUnitResult :: Aeson.Value -> AesonTypes.Parser ()
parseUnitResult _ =
  pure ()

parseTerminalCreateResult :: Aeson.Value -> AesonTypes.Parser Text
parseTerminalCreateResult =
  Aeson.withObject "terminal/create result" (Aeson..: "terminalId")

parseTerminalOutputResult :: Aeson.Value -> AesonTypes.Parser ACP.TerminalOutput
parseTerminalOutputResult =
  Aeson.withObject "terminal/output result" \o -> do
    exitStatus <- o Aeson..:? "exitStatus" >>= traverse parseTerminalExitStatus
    ACP.TerminalOutput
      <$> o Aeson..: "output"
      <*> o Aeson..: "truncated"
      <*> pure exitStatus

parseTerminalExitStatus :: Aeson.Value -> AesonTypes.Parser ACP.TerminalExitStatus
parseTerminalExitStatus =
  Aeson.withObject "terminal exit status" \o ->
    ACP.TerminalExitStatus
      <$> o Aeson..:? "exitCode"
      <*> o Aeson..:? "signal"
