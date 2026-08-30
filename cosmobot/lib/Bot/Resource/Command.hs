{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Bot.Resource.Command
  ( Command
  , CommandStatus (..)
  , createAndStart
  , queryCommand
  , waitCommand
  , appendStdout
  , appendStderr
  )
where

import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Data.Text as Text
import qualified Effectful.Concurrent.MVar as MVar
import Effectful.Timeout (Timeout, timeout)

data Command = Command
  { state :: !(MVar.MVar CommandStatus)
  , worker :: !(MVar.MVar (Maybe Concurrency.Handle))
  }

data CommandStatus = Running !Text !Text | Finished !(Either Text Text) !Text !Text
  deriving stock (Eq, Show)

instance (Resource.Resource :> es, Concurrency.Concurrency :> es, Concurrent :> es) => Resource.ResourceObject (Eff es) Command where
  type CreationArgs Command = ()
  resourceTypeName _ = "Command"
  resourceScope _ = Resource.PersonResource
  resourceIdPrefix _ = "cmd"
  resourceListed _ = False
  resourceTTLSeconds _ = Right (Just (5 * 60))
  createResourceObject _ = Right <$> (Command <$> MVar.newMVar (Running "" "") <*> MVar.newMVar Nothing)
  destroyResourceObject command = do
    active <- MVar.readMVar command.worker
    for_ active \workerHandle -> do
      void $ Concurrency.cancel workerHandle.handleId
      Concurrency.await workerHandle
    pure (Right ())
  probeResourceObject command = do
    status <- MVar.readMVar command.state
    pure $ Right $ case status of
      Running{} -> "running"
      Finished{} -> "finished"
  describeResourceObject _ result = pure (either (const "unavailable") id result)

createAndStart
  :: (Resource.Resource :> es, Concurrency.Concurrency :> es, Concurrent :> es)
  => Resource.ResourceAccess
  -> Maybe Concurrency.Handle
  -> Resource.Init ()
  -> (Concurrency.Handle -> Command -> Eff es (Either Text Text))
  -> Eff es (Either Resource.ResourceError Resource.ResourceId)
createAndStart access parent initValue action = do
  created <- Resource.createAssociated @Command parent initValue
  for_ (rightToMaybe created) \commandId -> do
    _ <- Resource.withResource @Command access commandId parent \command -> do
      workerHandle <- Concurrency.forkWithHandle "command" \worker -> do
        result <- trySync $ do
          held <- Resource.withResource @Command access commandId (Just worker) \resourceCommand -> do
            commandResult <- action worker resourceCommand
            void $ Resource.keepAlive access commandId
            pure commandResult
          pure $ join (first (Text.pack . show) held)
        MVar.modifyMVar_ command.state \case
          Running stdoutText stderrText -> pure (Finished (either (Left . Text.take 500 . show) id result) stdoutText stderrText)
          finished -> pure finished
      MVar.modifyMVar_ command.worker (const (pure (Just workerHandle)))
      pure ()
    pure ()
  pure created

queryCommand :: Concurrent :> es => Command -> Eff es CommandStatus
queryCommand = MVar.readMVar . (.state)

appendStdout :: Concurrent :> es => Command -> Text -> Eff es ()
appendStdout command chunk = appendOutput command chunk ""

appendStderr :: Concurrent :> es => Command -> Text -> Eff es ()
appendStderr command chunk = appendOutput command "" chunk

appendOutput :: Concurrent :> es => Command -> Text -> Text -> Eff es ()
appendOutput command stdoutChunk stderrChunk =
  MVar.modifyMVar_ command.state \case
    Running stdoutText stderrText -> pure (Running (stdoutText <> stdoutChunk) (stderrText <> stderrChunk))
    finished -> pure finished

waitCommand
  :: (Concurrency.Concurrency :> es, Concurrent :> es, Timeout :> es)
  => Int
  -> Command
  -> Eff es CommandStatus
waitCommand seconds command = do
  active <- MVar.readMVar command.worker
  for_ active \workerHandle -> void $ timeout (max 1 seconds * 1000000) (Concurrency.await workerHandle)
  queryCommand command
