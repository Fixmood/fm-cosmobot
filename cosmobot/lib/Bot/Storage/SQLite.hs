{-|
Module      : Bot.Storage.SQLite
Description : SQLite interpreter for application storage
Stability   : experimental
-}

module Bot.Storage.SQLite
  ( runStorageSQLite
  , runStorageSQLitePath
  )
where

import Bot.Prelude
import qualified Bot.Effect.Storage as Storage
import qualified Database.Selda.Backend as SeldaBackend
import qualified Database.Selda.SQLite as SeldaSQLite
import qualified Effectful.Concurrent.STM as STM

runStorageSQLite
  :: (IOE :> es, Concurrent :> es)
  => SeldaBackend.SeldaConnection SeldaSQLite.SQLite
  -> Eff (Storage.Storage : es) a
  -> Eff es a
runStorageSQLite seldaConnection =
  runStorageSQLitePool [seldaConnection]

runStorageSQLitePool
  :: (IOE :> es, Concurrent :> es)
  => [SeldaBackend.SeldaConnection SeldaSQLite.SQLite]
  -> Eff (Storage.Storage : es) a
  -> Eff es a
runStorageSQLitePool connections inner = do
  pool <- STM.newTBQueueIO (fromIntegral (length connections))
  STM.atomically (traverse_ (STM.writeTBQueue pool) connections)
  interpret
    ( \_ -> \case
        Storage.RunSelda action ->
          bracket
            (STM.atomically (STM.readTBQueue pool))
            (STM.atomically . STM.writeTBQueue pool)
            \connection ->
              liftIO (SeldaBackend.runSeldaT action connection)
    )
    inner

runStorageSQLitePath
  :: (IOE :> es, Concurrent :> es)
  => FilePath
  -> Eff (Storage.Storage : es) a
  -> Eff es a
runStorageSQLitePath path inner = do
  bracket
    (openSQLiteConnections path)
    closeSQLiteConnections
    (\connections -> runStorageSQLitePool connections inner)

openSQLiteConnections
  :: IOE :> es
  => FilePath
  -> Eff es [SeldaBackend.SeldaConnection SeldaSQLite.SQLite]
openSQLiteConnections path
  | path == ":memory:" || null path =
      (: []) <$> liftIO (SeldaSQLite.sqliteOpen path)
  | otherwise =
      bracketOnError
        (openSQLiteConnection path configurePrimaryConnection)
        (liftIO . SeldaBackend.seldaClose)
        (\primary -> (primary :) <$> openAdditionalConnections path (sqlitePoolSize - 1))

openAdditionalConnections
  :: IOE :> es
  => FilePath
  -> Int
  -> Eff es [SeldaBackend.SeldaConnection SeldaSQLite.SQLite]
openAdditionalConnections _ 0 =
  pure []
openAdditionalConnections path count =
  bracketOnError
    (openSQLiteConnection path configureConnection)
    (liftIO . SeldaBackend.seldaClose)
    (\connection -> (connection :) <$> openAdditionalConnections path (count - 1))

openSQLiteConnection
  :: IOE :> es
  => FilePath
  -> (SeldaBackend.SeldaConnection SeldaSQLite.SQLite -> Eff es ())
  -> Eff es (SeldaBackend.SeldaConnection SeldaSQLite.SQLite)
openSQLiteConnection path configure =
  bracketOnError
    (liftIO (SeldaSQLite.sqliteOpen path))
    (liftIO . SeldaBackend.seldaClose)
    (\connection -> configure connection $> connection)

configurePrimaryConnection
  :: IOE :> es
  => SeldaBackend.SeldaConnection SeldaSQLite.SQLite
  -> Eff es ()
configurePrimaryConnection connection = do
  (_, rows) <- runSQLiteStatement connection "PRAGMA journal_mode = WAL;"
  case rows of
    [[SeldaBackend.SqlString "wal"]] -> pure ()
    _ -> throwIO (SeldaBackend.DbError ("SQLite refused WAL mode: " <> show rows))
  configureConnection connection

configureConnection
  :: IOE :> es
  => SeldaBackend.SeldaConnection SeldaSQLite.SQLite
  -> Eff es ()
configureConnection connection =
  void (runSQLiteStatement connection [i|PRAGMA busy_timeout = #{sqliteBusyTimeoutMilliseconds};|])

runSQLiteStatement
  :: IOE :> es
  => SeldaBackend.SeldaConnection SeldaSQLite.SQLite
  -> Text
  -> Eff es (Int, [[SeldaBackend.SqlValue]])
runSQLiteStatement connection statement =
  liftIO $
    SeldaBackend.runSeldaT
      (SeldaBackend.withBackend \backend -> liftIO (SeldaBackend.runStmt backend statement []))
      connection

closeSQLiteConnections
  :: IOE :> es
  => [SeldaBackend.SeldaConnection SeldaSQLite.SQLite]
  -> Eff es ()
closeSQLiteConnections =
  foldr
    (\connection closeRest -> liftIO (SeldaBackend.seldaClose connection) `finally` closeRest)
    (pure ())

sqlitePoolSize :: Int
sqlitePoolSize =
  4

sqliteBusyTimeoutMilliseconds :: Int
sqliteBusyTimeoutMilliseconds =
  5000
