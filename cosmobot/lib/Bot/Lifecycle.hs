{-# LANGUAGE TypeApplications #-}
{-|
Module      : Bot.Lifecycle
Description : Process lifecycle hooks
Stability   : experimental
-}

module Bot.Lifecycle
  ( runLifecycle
  , runLifecycleEffect
  )
where

import Bot.Core.Message (IncomingMessage)
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.Lifecycle as LifecycleEffect
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Media.Config as MediaConfig
import Bot.Prelude
import qualified Bot.Storage.Lifecycle as LifecycleStorage
import qualified Bot.Storage.RPC as RpcStorage
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Unique as Unique
import Effectful.FileSystem (FileSystem)
import qualified Effectful.Concurrent.MVar as MVar
import qualified System.Posix.Signals as Signals

data ExitReason
  = Stop
  | Restart
  deriving stock (Eq, Show)

runLifecycle
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, Media.Media :> es, Storage.Storage :> es, FileSystem :> es, Concurrent :> es, Prim :> es, IOE :> es, KatipE :> es)
  => MediaConfig.Config
  -> IORef Bool
  -> Eff (LifecycleEffect.Lifecycle : es) ()
  -> Eff es ()
runLifecycle mediaConfig restartRequested inner =
  withMediaGc mediaConfig $
    bracket_ runStartupActions runShutdownActions do
      reason <- runUntilExit inner
      when (reason == Restart) (writeIORef restartRequested True)

runUntilExit
  :: (Storage.Storage :> es, Concurrency.Concurrency :> es, Concurrent :> es, IOE :> es)
  => Eff (LifecycleEffect.Lifecycle : es) ()
  -> Eff es ExitReason
runUntilExit inner = do
  exitRequest <- MVar.newEmptyMVar
  result <- MVar.newEmptyMVar
  withExitSignalHandlers exitRequest $
    Concurrency.raceTasks_
      "main.inner"
      (runLifecycleEffect (queueRestart exitRequest) inner)
      "main.shutdown"
      (MVar.takeMVar exitRequest >>= MVar.putMVar result)
  MVar.tryTakeMVar result >>= \case
    Just reason -> pure reason
    Nothing -> fromMaybe Stop <$> MVar.tryTakeMVar exitRequest

withExitSignalHandlers
  :: (Concurrent :> es, IOE :> es)
  => MVar.MVar ExitReason
  -> Eff es a
  -> Eff es a
withExitSignalHandlers exitRequest =
  bracket install restore . const
  where
    install = liftIO do
      oldTerm <- Signals.installHandler Signals.sigTERM handler Nothing
      oldInt <- Signals.installHandler Signals.sigINT handler Nothing
      pure (oldTerm, oldInt)

    restore (oldTerm, oldInt) = liftIO do
      void $ Signals.installHandler Signals.sigTERM oldTerm Nothing
      void $ Signals.installHandler Signals.sigINT oldInt Nothing

    handler = Signals.Catch notify
    notify = void . runEff . runConcurrent $ MVar.tryPutMVar exitRequest Stop

queueRestart
  :: (Storage.Storage :> es, Concurrent :> es, IOE :> es)
  => MVar.MVar ExitReason
  -> IncomingMessage
  -> Text
  -> Eff es ()
queueRestart exitRequest message body = do
  unique <- liftIO Unique.newUnique
  void $ LifecycleStorage.enqueueStartupReply [i|restart-#{Unique.hashUnique unique}|] message body
  void $ MVar.tryPutMVar exitRequest Restart

runLifecycleEffect
  :: (IncomingMessage -> Text -> Eff es ())
  -> Eff (LifecycleEffect.Lifecycle : es) a
  -> Eff es a
runLifecycleEffect onRestart =
  interpret (\_ (LifecycleEffect.RequestRestart message body) -> onRestart message body)

runStartupActions
  :: (Chat.Chat :> es, Storage.Storage :> es, KatipE :> es)
  => Eff es ()
runStartupActions = do
  actions <- LifecycleStorage.loadStartupActions
  for_ actions \action@LifecycleStorage.StartupReply{actionId, message, body} -> do
    result <- trySync $
      (if Text.null (Text.strip body)
        then pure []
        else Chat.replyTo message body)
      `finally` LifecycleStorage.deleteStartupAction action
    case result of
      Right response -> do
        logInfo [i|Ran startup reply lifecycle action #{actionId}; response=#{show response :: Text}|]
      Left err -> do
        logWarning [i|Startup reply lifecycle action #{actionId} failed and was deleted: #{show err :: String}|]

withMediaGc
  :: (Concurrency.Concurrency :> es, Media.Media :> es, Storage.Storage :> es, FileSystem :> es, Concurrent :> es, IOE :> es, KatipE :> es)
  => MediaConfig.Config
  -> Eff es a
  -> Eff es a
withMediaGc mediaConfig inner
  | not mediaConfig.gc.enabled =
      inner
  | otherwise =
      Concurrency.withWorker "media.gc" (mediaGcLoop mediaConfig) inner

mediaGcLoop
  :: (Media.Media :> es, Storage.Storage :> es, FileSystem :> es, Concurrent :> es, IOE :> es, KatipE :> es)
  => MediaConfig.Config
  -> Eff es ()
mediaGcLoop mediaConfig =
  forever do
    runMediaGc mediaConfig
    threadDelay (hoursToMicroseconds (max 1 mediaConfig.gc.intervalHours))

runMediaGc
  :: (Media.Media :> es, Storage.Storage :> es, IOE :> es, KatipE :> es)
  => MediaConfig.Config
  -> Eff es ()
runMediaGc mediaConfig = do
  let maxAgeSeconds = daysToSeconds (max 0 mediaConfig.gc.olderThanDays)
  result <- trySync do
    retained <- Set.fromList <$> RpcStorage.referencedMediaFileIds
    Media.gcMediaCache maxAgeSeconds retained
  case result of
    Right deleted ->
      when (deleted > 0) $
        logInfo [i|Media cache GC deleted #{deleted} file(s)|]
    Left err ->
      logWarning [i|Media cache GC failed: #{show err :: String}|]

daysToSeconds :: Int -> Int
daysToSeconds days =
  days * 24 * 60 * 60

hoursToMicroseconds :: Int -> Int
hoursToMicroseconds hours =
  hours * 60 * 60 * 1000000

runShutdownActions :: KatipE :> es => Eff es ()
runShutdownActions =
  pure ()
