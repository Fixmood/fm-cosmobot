-- | Command-line entry point for cosmobot.
module Main (main) where

import Bot.Prelude
import qualified Bot.Main as BotMain
import qualified Bot.RPC.Client as RpcClient
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Options.Applicative

main :: IO ()
main =
  execParser commandInfo >>= \case
    Serve configPath -> BotMain.mainWithConfig configPath
    Rpc options rpcCommand -> RpcClient.runRpcClientCommand options rpcCommand

data Command
  = Serve !FilePath
  | Rpc !RpcClient.RpcClientOptions !RpcClient.RpcClientCommand

commandInfo :: ParserInfo Command
commandInfo =
  info (commandParser <**> helper) $
    fullDesc
      <> progDesc "Run cosmobot commands"
      <> header "cosmobot"

configPathParser :: Parser FilePath
configPathParser =
  strOption $
    long "config"
      <> metavar "FILE"
      <> value "config.toml"
      <> showDefaultWith (const "cwd/config.toml")
      <> help "Path to the config TOML file"

commandParser :: Parser Command
commandParser =
  subparser $
    command "serve"
      ( info (serveParser <**> helper) $
          progDesc "Start the bot using config.toml from the current working directory"
      )
      <> command "rpc"
        ( info (rpcParser <**> helper) $
            progDesc "Call the local cosmobot RPC websocket service"
        )

serveParser :: Parser Command
serveParser = Serve <$> configPathParser

rpcParser :: Parser Command
rpcParser =
  Rpc
    <$> rpcClientOptionsParser
    <*> rpcCommandParser

rpcClientOptionsParser :: Parser RpcClient.RpcClientOptions
rpcClientOptionsParser =
  RpcClient.RpcClientOptions
    <$> configPathParser
    <*> optional
      ( strOption $
          long "host"
            <> metavar "HOST"
            <> help "RPC websocket host; overrides rpc.host from config"
      )
    <*> optional
      ( option auto $
          long "port"
            <> metavar "PORT"
            <> help "RPC websocket port; overrides rpc.port from config"
      )
    <*> optional
      ( Text.pack
          <$> strOption
            ( long "token"
                <> metavar "TOKEN"
                <> help "RPC bearer token; overrides rpc.token from config"
            )
      )

rpcCommandParser :: Parser RpcClient.RpcClientCommand
rpcCommandParser =
  subparser $
    command "audit"
      ( info (rpcAuditParser <**> helper) $
          progDesc "Query agent audit RPC methods"
      )
      <> command "media"
        ( info (rpcMediaParser <**> helper) $
            progDesc "Query and maintain media cache RPC methods"
        )
      <> command "call"
        ( info (rpcCallParser <**> helper) $
            progDesc "Call an arbitrary RPC method with JSON params"
        )

rpcAuditParser :: Parser RpcClient.RpcClientCommand
rpcAuditParser =
  subparser $
    command "recent"
      ( info (rpcAuditRecentParser <**> helper) $
          progDesc "Show recent agent audit tool uses"
      )
      <> command "show"
        ( info (rpcAuditShowParser <**> helper) $
            progDesc "Show one agent audit tool use by id"
        )
      <> command "thread"
        ( info (rpcAuditThreadParser <**> helper) $
            progDesc "Show agent audit records for a thread message"
        )

rpcAuditRecentParser :: Parser RpcClient.RpcClientCommand
rpcAuditRecentParser =
  RpcClient.RpcAuditRecent
    <$> option auto
      ( long "limit"
          <> metavar "N"
          <> value 20
          <> showDefault
          <> help "Maximum number of audit entries to return"
      )

rpcAuditShowParser :: Parser RpcClient.RpcClientCommand
rpcAuditShowParser =
  RpcClient.RpcAuditShow
    <$> argument auto (metavar "ID")

rpcAuditThreadParser :: Parser RpcClient.RpcClientCommand
rpcAuditThreadParser =
  RpcClient.RpcAuditThread
    <$> (Text.pack <$> strOption (long "platform" <> metavar "PLATFORM"))
    <*> optional (option auto (long "chat-id" <> metavar "CHAT_ID"))
    <*> (Text.pack <$> argument str (metavar "MESSAGE_ID"))

rpcMediaParser :: Parser RpcClient.RpcClientCommand
rpcMediaParser =
  subparser $
    command "stats"
      ( info (rpcMediaStatsParser <**> helper) $
          progDesc "Show media cache stats and recent files"
      )
      <> command "resolve-source"
        ( info (rpcMediaResolveSourceParser <**> helper) $
            progDesc "Resolve a source reference to a cached media id"
        )
      <> command "get"
        ( info (rpcMediaGetParser <**> helper) $
            progDesc "Show one media cache entry"
        )
      <> command "delete"
        ( info (rpcMediaDeleteParser <**> helper) $
            progDesc "Delete one media cache entry"
        )
      <> command "gc"
        ( info (rpcMediaGcParser <**> helper) $
            progDesc "Run media cache garbage collection"
        )

rpcMediaStatsParser :: Parser RpcClient.RpcClientCommand
rpcMediaStatsParser =
  RpcClient.RpcMediaStats
    <$> option auto
      ( long "limit"
          <> metavar "N"
          <> value 50
          <> showDefault
          <> help "Maximum number of media files to include"
      )

rpcMediaResolveSourceParser :: Parser RpcClient.RpcClientCommand
rpcMediaResolveSourceParser =
  RpcClient.RpcMediaResolveSource . Text.pack
    <$> argument str (metavar "SOURCE_REF")

rpcMediaGetParser :: Parser RpcClient.RpcClientCommand
rpcMediaGetParser =
  RpcClient.RpcMediaGet . Text.pack
    <$> argument str (metavar "MEDIA_ID_OR_FILE_ID")

rpcMediaDeleteParser :: Parser RpcClient.RpcClientCommand
rpcMediaDeleteParser =
  RpcClient.RpcMediaDelete . Text.pack
    <$> argument str (metavar "MEDIA_ID_OR_FILE_ID")

rpcMediaGcParser :: Parser RpcClient.RpcClientCommand
rpcMediaGcParser =
  RpcClient.RpcMediaGc
    <$> option auto
      ( long "max-age-seconds"
          <> metavar "SECONDS"
          <> value 0
          <> showDefault
          <> help "Delete media files older than this age unless retained"
      )

rpcCallParser :: Parser RpcClient.RpcClientCommand
rpcCallParser =
  RpcClient.RpcCall
    <$> (Text.pack <$> argument str (metavar "METHOD"))
    <*> argument jsonReader (metavar "JSON")

jsonReader :: ReadM Aeson.Value
jsonReader = eitherReader \input ->
  Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 (Text.pack input))
