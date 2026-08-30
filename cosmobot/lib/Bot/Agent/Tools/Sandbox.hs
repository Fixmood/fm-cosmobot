{-# LANGUAGE TypeApplications #-}

{-|
Module      : Bot.Agent.Tools.Sandbox
Description : Agent tools for chat-owned Docker sandboxes
Stability   : experimental
-}
module Bot.Agent.Tools.Sandbox
  ( sandboxTool
  )
where

import Bot.Agent.Tools.Common
import qualified Bot.Agent.Tools.Shell as Shell
import Bot.Agent.Tool
import Bot.Agent.Types
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Resource as Resource
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Media.Object as MediaObject
import Bot.Prelude
import qualified Bot.Resource.Sandbox as Sandbox
import qualified Bot.Resource.Command as Command
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import Effectful.FileSystem (FileSystem)
import qualified Effectful.Process.Typed as TypedProcess
import qualified Effectful.Temporary as Temporary
import Effectful.Timeout (Timeout)
import System.FilePath ((</>))
import qualified System.FilePath.Posix as Posix

sandboxTool
  :: (Media.Media :> es, Resource.Resource :> es, Concurrency.Concurrency :> es, FileSystem :> es, Timeout :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Tool (Eff es)
sandboxTool =
  tagged [workTag]
  . allowWhen hasResourceIdentity
  . withDescription "Create, list, rename, or delete isolated, persistent container sandboxes; run Bash; or copy files between one and the media cache. list returns a JSON array of accessible sandbox names. Delete sandboxes promptly when the job is done, unless the user asks explicitly to keep them."
  $ tool "sandbox"
      (parsedArguments
        (objectSchema
          [ fieldText "op" "One of: create, list, run, file_to_media, media_to_file, rename, delete."
          , fieldText "name" "Optional globally unique resource name for create; required as the new name for rename."
          , fieldText "sandbox" "Sandbox name; required except for create and list."
          , fieldText "path" "Sandbox file path; required for file_to_media and media_to_file."
          , fieldText "media_id" "Cached media id; required for media_to_file."
          , fieldText "script" "Bash script; required for run."
          , fieldInteger "ttl_minutes" "Sandbox inactivity lifetime in minutes; required for create, minimum 5."
          , fieldInteger "timeout_seconds" "Maximum seconds to wait before killing the script. Defaults to 30."
          , fieldInteger "output_byte_limit" "Maximum retained output bytes. Defaults to 1048576."
          ]
          ["op"])
        sandboxArgs)
      \call -> do
        context <- askToolContext
        metadata <- askToolCallMetadata
        case Resource.accessFromMessage context.message of
          Left err -> pure (resourceToolFailure err)
          Right access -> case call of
            SandboxCreate requestedName ttlMinutes ->
              createSandbox metadata requestedName Resource.Init
                { message = context.message
                , arguments = Sandbox.SandboxArgs{image = context.toolConfig.sandboxImage, ttlMinutes}
                } <&> \case
                Left err -> resourceToolFailure err
                Right sandboxId -> toolText (jsonText (Aeson.object ["sandbox" Aeson..= sandboxId]))
            SandboxList ->
              listResourceNames (Proxy @Sandbox.Sandbox) access
            SandboxRun sandboxId script timeoutSeconds outputByteLimit -> do
              command <- Command.createAndStart access metadata.resourceOwner Resource.Init
                { message = context.message, arguments = () }
                \worker _command -> do
                  result <- Resource.withResource @Sandbox.Sandbox access sandboxId (Just worker) \sandbox ->
                    Shell.runSandboxBashStreaming timeoutSeconds sandbox script outputByteLimit _command
                  pure (join (first renderResourceError result))
              case command of
                Left err -> pure (resourceToolFailure err)
                Right commandId -> Shell.observeCommand True access metadata.resourceOwner commandId 10 0 0
            SandboxFileToMedia sandboxId path -> do
              result <- Resource.withResource @Sandbox.Sandbox access sandboxId metadata.resourceOwner \sandbox ->
                copyFileToMedia sandbox path
              pure $ either resourceToolFailure (either clientFailure toolText) result
            SandboxMediaToFile sandboxId mediaId path -> do
              localPath <- Media.localMediaPath (mediaRef mediaId)
              case localPath of
                Nothing -> pure (clientFailure [i|Media object not found: #{mediaId}|])
                Just source -> do
                  result <- Resource.withResource @Sandbox.Sandbox access sandboxId metadata.resourceOwner \sandbox ->
                    Sandbox.copyFileToSandbox sandbox source path
                  pure $ either resourceToolFailure (either clientFailure (const (toolText "Media copied to sandbox."))) result
            SandboxDelete sandboxId ->
              Resource.destroy access sandboxId <&> either resourceToolFailure (const (toolText "Sandbox deleted."))
            SandboxRename sandboxId newName ->
              Resource.rename access sandboxId newName <&> either resourceToolFailure (toolText . ("Sandbox renamed: " <>))
  where
    createSandbox metadata = \case
      Nothing -> Resource.createForRun @Sandbox.Sandbox metadata.originRunId metadata.resourceOwner
      Just name -> Resource.createNamedForRun @Sandbox.Sandbox metadata.originRunId metadata.resourceOwner name

data SandboxCall
  = SandboxCreate !(Maybe Text) !Int
  | SandboxList
  | SandboxRun !Text !Text !Int !(Maybe Int)
  | SandboxFileToMedia !Text !FilePath
  | SandboxMediaToFile !Text !Text !FilePath
  | SandboxDelete !Text
  | SandboxRename !Text !Text

sandboxArgs :: Aeson.Value -> AesonTypes.Parser SandboxCall
sandboxArgs = Aeson.withObject "sandbox arguments" \o -> do
  op <- o Aeson..: Key.fromText "op"
  case op :: Text of
    "create" -> do
      requestedName <- o Aeson..:? Key.fromText "name"
      SandboxCreate requestedName <$> parseTTLMinutes o
    "list" -> pure SandboxList
    "run" -> do
      sandboxId <- o Aeson..: Key.fromText "sandbox" >>= validText "sandbox"
      script <- o Aeson..: Key.fromText "script" >>= validValue "script"
      timeoutSeconds <- fromMaybe 30 <$> o Aeson..:? Key.fromText "timeout_seconds"
      outputByteLimit <- o Aeson..:? Key.fromText "output_byte_limit"
      when (timeoutSeconds <= 0) $ fail "timeout_seconds must be positive."
      when (maybe False (<= 0) outputByteLimit) $ fail "output_byte_limit must be positive."
      pure (SandboxRun sandboxId script timeoutSeconds outputByteLimit)
    "file_to_media" -> SandboxFileToMedia
      <$> (o Aeson..: Key.fromText "sandbox" >>= validText "sandbox")
      <*> (Text.unpack <$> (o Aeson..: Key.fromText "path" >>= validText "path"))
    "media_to_file" -> SandboxMediaToFile
      <$> (o Aeson..: Key.fromText "sandbox" >>= validText "sandbox")
      <*> (Text.strip <$> (o Aeson..: Key.fromText "media_id" >>= validText "media_id"))
      <*> (Text.unpack <$> (o Aeson..: Key.fromText "path" >>= validText "path"))
    "delete" -> SandboxDelete <$> (o Aeson..: Key.fromText "sandbox" >>= validText "sandbox")
    "rename" -> SandboxRename
      <$> (o Aeson..: Key.fromText "sandbox" >>= validText "sandbox")
      <*> (o Aeson..: Key.fromText "name" >>= validText "name")
    _ -> fail "op must be one of: create, list, run, file_to_media, media_to_file, rename, delete."

copyFileToMedia
  :: (Media.Media :> es, FileSystem :> es, Concurrent :> es, TypedProcess.TypedProcess :> es, IOE :> es)
  => Sandbox.Sandbox
  -> FilePath
  -> Eff es (Either Text Text)
copyFileToMedia sandbox path =
  Temporary.runTemporary $
    Temporary.withSystemTempDirectory "cosmobot-sandbox-media-" \dir -> raise do
      let temporaryPath = dir </> ("media" <> Posix.takeExtension path)
      Sandbox.copyFileFromSandbox sandbox path temporaryPath >>= \case
        Left err -> pure (Left err)
        Right () -> do
          mediaObject <- MediaObject.fileObject ("file://" <> Text.pack temporaryPath)
          Media.storeMediaObject mediaObject
            <&> maybe (Left "Media cache rejected the sandbox file.") Right

mediaRef :: Text -> Text
mediaRef value
  | "media:" `Text.isPrefixOf` value = value
  | otherwise = "media:" <> value

hasResourceIdentity :: Context -> Bool
hasResourceIdentity = isRight . Resource.accessFromMessage . (.message)

clientFailure :: Text -> ToolResult
clientFailure err = toolFailure (permanentArgumentFailure err err)

validText :: String -> Text -> AesonTypes.Parser Text
validText label value = do
  when (Text.null (Text.strip value)) $ fail (label <> " must not be empty.")
  validValue label value

validValue :: String -> Text -> AesonTypes.Parser Text
validValue label value = do
  when (Text.any (== '\NUL') value) $ fail (label <> " must not contain NUL.")
  pure value
