{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module: Effectful.Katip
Description: Effect to use Katip
-}
module Effectful.Katip (
  -- * Framework Types
  Namespace (..),
  Environment (..),
  Severity (..),
  renderSeverity,
  textToSeverity,
  Verbosity (..),
  ToObject (..),
  LogItem (..),
  Item (..),
  ThreadIdText (..),
  PayloadSelection (..),
  Scribe (..),
  LogEnv (..),
  SimpleLogPayload,
  sl,
  defaultScribeSettings,
  ScribeSettings,
  scribeBufferSize,
  _scribeBufferSize,

  -- ** @lens@-Compatible Lenses
  itemApp,
  itemEnv,
  itemSeverity,
  itemThread,
  itemHost,
  itemProcess,
  itemPayload,
  itemMessage,
  itemTime,
  itemNamespace,
  itemLoc,
  logEnvHost,
  logEnvPid,
  logEnvApp,
  logEnvEnv,
  logEnvTimer,
  logEnvScribes,

  -- * Effect
  KatipE,

  -- ** Running The Effect
  runKatipE,
  runKatipContextE,
  startKatipE,
  startKatipContextE,

  -- * Initializing Loggers
  registerScribe,

  -- * Dropping Scribes Temporarily
  unregisterScribe,
  clearScribes,

  -- * Finalizing Scribes At Shutdown
  closeScribe,

  -- * Logging Functions
  LogStr (..),
  logStr,
  ls,
  showLS,

  -- ** Katip Logging Functions
  getLogEnv,
  localLogEnv,
  logF,
  logMsg,
  logT,
  logLoc,
  logItem,
  logKatipItem,
  logException,

  -- ** KatipContext Logging Functions
  getKatipContext,
  localKatipContext,
  getKatipNamespace,
  localKatipNamespace,
  logFM,
  logTM,
  logLocM,
  logItemM,
  logExceptionM,
  AnyLogContext,
  LogContexts,
  liftPayload,

  -- *** Temporarily Changing Log Behaviour
  katipAddNamespace,
  katipAddContext,
  katipNoLogging,

  -- * Included Scribes
  mkHandleScribe,
  mkHandleScribeWithFormatter,
  mkFileScribe,
  ColorStrategy (..),
  ItemFormatter,
  bracketFormat,
  jsonFormat,

  -- * Tools For Implementing Scribes
  PermitFunc,
  permitAND,
  permitOR,
  permitItem,
  payloadObject,
  itemJson,
) where

import Effectful
import Effectful.Dispatch.Static
import Katip (
  --  logging

  -- logContext
  AnyLogContext,
  -- formatting
  ColorStrategy (..),
  Environment (..),
  Item (..),
  ItemFormatter,
  LogContexts,
  LogEnv (..),
  LogItem (..),
  LogStr (..),
  Namespace (..),
  PayloadSelection (..),
  -- permission
  PermitFunc,
  Scribe (..),
  ScribeSettings,
  Severity (..),
  SimpleLogPayload,
  ThreadIdText (..),
  ToObject (..),
  Verbosity (..),
  bracketFormat,
  -- removing scribe

  clearScribes,
  defaultScribeSettings,
  itemApp,
  itemEnv,
  itemHost,
  itemJson,
  itemLoc,
  itemMessage,
  itemNamespace,
  itemPayload,
  itemProcess,
  itemSeverity,
  itemThread,
  itemTime,
  jsonFormat,
  liftPayload,
  logEnvApp,
  logEnvEnv,
  logEnvHost,
  logEnvPid,
  logEnvScribes,
  logEnvTimer,
  logStr,
  ls,
  payloadObject,
  permitAND,
  permitItem,
  permitOR,
  renderSeverity,
  scribeBufferSize,
  showLS,
  sl,
  textToSeverity,
  unregisterScribe,
  _scribeBufferSize,
 )

import Control.Monad.Catch (finally)
import Data.Text (Text)
import Katip qualified as K
import Katip.Core (getLocTH)
import Language.Haskell.TH (Loc)
import Language.Haskell.TH.Lib
import System.IO (Handle)

-- | A Effect you can use to run logging actions. there is only one effect as we can't have duplicated instances.
type KatipE :: Effect
data KatipE m a

type instance DispatchOf KatipE = Static WithSideEffects

data instance StaticRep KatipE = MkKatipE !LogEnv !LogContexts !Namespace

-- | Run a KatipE Effect without a 'Namespace' or a 'LogContexts'. This also calls closeScribes
runKatipE :: forall es a. (IOE :> es) => LogEnv -> Eff (KatipE : es) a -> Eff es a
runKatipE l act = finally (evalStaticRep (MkKatipE l mempty mempty) act) $ liftIO (K.closeScribes l)

-- | Run a KatipE Effect with a 'Namespace' and a 'LogContexts'. this also calls closeScribes
runKatipContextE :: forall es a a1. (LogItem a, IOE :> es) => LogEnv -> a -> Namespace -> Eff (KatipE : es) a1 -> Eff es a1
runKatipContextE l pl ns act = finally (evalStaticRep (MkKatipE l (liftPayload pl) ns) act) $ liftIO (K.closeScribes l)

instance forall es. (IOE :> es, KatipE :> es) => K.Katip (Eff es) where
  getLogEnv :: (IOE :> es, KatipE :> es) => Eff es LogEnv
  getLogEnv = getLogEnv
  localLogEnv ::
    (IOE :> es, KatipE :> es) =>
    (LogEnv -> LogEnv) ->
    Eff es a ->
    Eff es a
  localLogEnv = localLogEnv
instance forall es. (IOE :> es, KatipE :> es) => K.KatipContext (Eff es) where
  getKatipContext :: Eff es LogContexts
  getKatipContext = getKatipContext
  localKatipContext ::
    (IOE :> es, KatipE :> es) =>
    (LogContexts -> LogContexts) ->
    Eff es a ->
    Eff es a
  localKatipContext = localKatipContext
  getKatipNamespace :: (IOE :> es, KatipE :> es) => Eff es Namespace
  getKatipNamespace = getKatipNamespace
  localKatipNamespace ::
    (IOE :> es, KatipE :> es) =>
    (Namespace -> Namespace) ->
    Eff es a ->
    Eff es a
  localKatipNamespace = localKatipNamespace

-- | Run a KatipE Effect without a 'Namespace' or a 'LogContexts' and creating a 'LogEnv'
startKatipE :: (IOE :> es) => Namespace -> Environment -> Eff (KatipE : es) a -> Eff es a
startKatipE ns env act = liftIO (K.initLogEnv ns env) >>= \lenv -> runKatipE lenv act

-- | Run a KatipE Effect with a 'Namespace' and a 'LogContexts' and creating a 'LogEnv'
startKatipContextE :: (IOE :> es, LogItem a) => Environment -> a -> Namespace -> Eff (KatipE : es) a1 -> Eff es a1
startKatipContextE env a ns act = liftIO (K.initLogEnv ns env) >>= \e -> runKatipContextE e a ns act

-- | Add a scribe to the list.
-- All future log calls will go to this scribe in addition to the others.
-- Writes will be buffered per the ScribeSettings to prevent slow scribes from slowing down logging.
-- Writes will be dropped if the buffer fills.
registerScribe :: (KatipE :> es) => Text -> Scribe -> ScribeSettings -> Eff es ()
registerScribe txt scrb scrbs = do
  MkKatipE lenv lctx ns <- getStaticRep
  nlenv <- unsafeEff_ $ K.registerScribe txt scrb scrbs lenv
  putStaticRep $ MkKatipE nlenv lctx ns

-- | Finalize a scribe early. Note that it isn't necessary to call this as both 'runKatipE' and 'runKatipContextE' call 'K.closeScribes'
closeScribe :: (KatipE :> es) => Text -> Eff es ()
closeScribe name = do
  MkKatipE le lc ns <- getStaticRep
  newle <- unsafeEff_ $ K.closeScribe name le
  putStaticRep $ MkKatipE newle lc ns

-- | get the 'KatipE' 'LogEnv'
getLogEnv :: forall es. (KatipE :> es) => Eff es LogEnv
getLogEnv = do
  s <- getStaticRep @KatipE
  case s of
    MkKatipE le _ _ -> return le

-- | temporarily modify the 'LogEnv'
localLogEnv :: forall es a. (KatipE :> es) => (LogEnv -> LogEnv) -> Eff es a -> Eff es a
localLogEnv f = localStaticRep @KatipE $ \(MkKatipE le lc ns) -> MkKatipE (f le) lc ns

-- | get the 'KatipE' 'LogContexts'
getKatipContext :: (KatipE :> es) => Eff es LogContexts
getKatipContext = do
  s <- getStaticRep @KatipE
  case s of
    MkKatipE _ lc _ -> return lc

-- | temporarily modify the 'LogContexts'
localKatipContext :: forall es a. (KatipE :> es) => (LogContexts -> LogContexts) -> Eff es a -> Eff es a
localKatipContext f = localStaticRep @KatipE $ \(MkKatipE le lc ns) -> MkKatipE le (f lc) ns

getKatipNamespace :: forall es. (KatipE :> es) => Eff es Namespace

-- | get the 'KatipE' 'Namespace'
getKatipNamespace = do
  s <- getStaticRep @KatipE
  case s of
    MkKatipE _ _ ns -> return ns

-- | temporarily modify the 'Namespace'
localKatipNamespace :: forall es a. (KatipE :> es) => (Namespace -> Namespace) -> Eff es a -> Eff es a
localKatipNamespace f = localStaticRep @KatipE $ \(MkKatipE le lc ns) -> MkKatipE le lc $ f ns

type KatipStack = K.KatipContextT IO

-- | escape hatch for implementing your own scribes
unsafeEmbedIOE ::
  forall es a.
  (KatipE :> es) =>
  KatipStack a ->
  Eff es a
unsafeEmbedIOE act = do
  MkKatipE logEnv logContext namespace <- getStaticRep
  unsafeEff_
    . K.runKatipContextT logEnv logContext namespace
    $ act

-- | escape hatch for implementing your own scribes
unsafeEmbedIOE' ::
  forall es a.
  (KatipE :> es) =>
  ((forall r. Eff es r -> KatipStack r) -> KatipStack a) ->
  Eff es a
unsafeEmbedIOE' k = do
  MkKatipE logEnv logContext namespace <- getStaticRep
  unsafeSeqUnliftIO $ \unlift -> do
    K.runKatipContextT logEnv logContext namespace $
      k (liftIO . unlift)

-- | Log with full context, but without any code location.
logF :: forall a es. (LogItem a, KatipE :> es) => a -> Namespace -> Severity -> LogStr -> Eff es ()
logF a ns sev logs = unsafeEmbedIOE $ K.logF a ns sev logs

-- | Log a message without any payload/context or code location.
logMsg :: forall es. (KatipE :> es) => Namespace -> Severity -> LogStr -> Eff es ()
logMsg ns sev logs = unsafeEmbedIOE $ K.logMsg ns sev logs


-- | Loc-tagged logging when using template-haskell.
-- > $(logT) obj mempty InfoS "Hello world"
logT :: ExpQ
logT = [|\a ns sev msg -> logItem a ns (Just $(getLocTH)) sev msg|]

{-# INLINE logLoc #-}

-- | 'Loc'-tagged logging using 'GHC.Stack.Stack' when available.
-- This function does not require template-haskell as it automatically uses implicit-callstacks when the code is compiled using GHC > 7.8.
-- Using an older version of the compiler will result in the emission of a log line without any location information, so be aware of it.
--
-- @
-- logLoc obj mempty InfoS "Hello world"
-- @
logLoc :: (LogItem a, KatipE :> es, HasCallStack) => a -> Namespace -> Severity -> LogStr -> Eff es ()
logLoc a ns sev logs = unsafeEmbedIOE $ K.logLoc a ns sev logs

-- | Log with everything, including a source code location.
-- This is very low level and you typically can use 'logT' in its place.
logItem :: (LogItem a, KatipE :> es) => a -> Namespace -> Maybe Loc -> Severity -> LogStr -> Eff es ()
logItem a ns loc sev logs = unsafeEmbedIOE $ K.logItem a ns loc sev logs

-- | Log an already constructed 'Item'.
-- This is the lowest level function that other log* functions use.
-- It can be useful when implementing centralised logging services.
logKatipItem :: (LogItem a, KatipE :> es) => Item a -> Eff es ()
logKatipItem item = unsafeEmbedIOE $ K.logKatipItem item

-- | Perform an action while logging any exceptions that may occur.
--
-- @
-- >>> > logException () mempty ErrorS (error "foo")
-- @
logException :: (LogItem a, KatipE :> es) => a -> Namespace -> Severity -> Eff es b -> Eff es b
logException a ns sev act = unsafeEmbedIOE' $ \unlift -> K.logException a ns sev (unlift act)

-- | Log with full context, but without any code location.
-- Automatically supplies payload and namespace.
logFM :: (KatipE :> es) => Severity -> LogStr -> Eff es ()
logFM sev logs = unsafeEmbedIOE $ K.logFM sev logs

{-# INLINE logTM #-}

-- | 'Loc'-tagged logging when using template-haskell.
-- Automatically supplies payload and namespace.
-- > $(logTM) InfoS "Hello world"
logTM :: ExpQ
logTM = [|logItemM (Just $(getLocTH))|]

{-# INLINE logLocM #-}

-- | 'Loc'-tagged logging when using 'GHC.Stack.getCallStack' implicit-callstacks.
-- Automatically supplies payload and namespace.
-- Same consideration as 'logLoc' applies
-- By default, location will be logged from the module that invokes logLocM.
-- If you want to use logLocM in a helper,
-- wrap the entire helper in withFrozenCallStack to retain the callsite of the helper in the logs.
-- This function does not require template-haskell.
--
-- @
-- logLocM InfoS "Hello world"
-- @
logLocM :: (KatipE :> es, HasCallStack) => Severity -> LogStr -> Eff es ()
logLocM sev logs = unsafeEmbedIOE $ K.logLocM sev logs

-- | Log with everything, including a source code location.
-- This is very low level and you typically can use 'logTM' in its place.
-- Automatically supplies payload and namespace.
logItemM :: (KatipE :> es, HasCallStack) => Maybe Loc -> Severity -> LogStr -> Eff es ()
logItemM loc sev logs = unsafeEmbedIOE $ K.logItemM loc sev logs

-- | Perform an action while logging any exceptions that may occur.
--
-- @
-- >>> > error "foo" `logExceptionM` ErrorS
-- @
logExceptionM :: (KatipE :> es) => Eff es a -> Severity -> Eff es a
logExceptionM act sev = unsafeEmbedIOE' $ \unlift -> K.logExceptionM (unlift act) sev

-- | Append a namespace segment to the current namespace for the given monadic action,
-- then restore the previous state afterwards.
katipAddNamespace :: (KatipE :> es) => Namespace -> Eff es a -> Eff es a
katipAddNamespace ns = localKatipNamespace (<> ns)

-- | Append some context to the current context for the given monadic action,
-- then restore the previous state afterwards.
-- Important note: be careful using this in a loop.
-- If you're using something like forever or replicateM_ that does explicit sharing to avoid a memory leak,
-- youll be fine as it will *sequence* calls to katipAddNamespace,
-- so each loop will get the same context added.
-- If you instead roll your own recursion and you're recursing in the action you provide,
-- you'll instead accumulate tons of redundant contexts and even if they all merge on log,
-- they are stored in a sequence and will leak memory.
katipAddContext :: (KatipE :> es, LogItem i) => i -> Eff es a -> Eff es a
katipAddContext ctx = localKatipContext (<> liftPayload ctx)

-- | Disable all scribes for the given monadic action,
-- then restore them afterwards.
katipNoLogging :: (KatipE :> es) => Eff es a -> Eff es a
katipNoLogging = localLogEnv $ \lenv -> lenv{_logEnvScribes = mempty}

-- | Logs to a file handle such as stdout, stderr, or a file.
-- Contexts and other information will be flattened out into bracketed fields.
-- For example:
-- @
-- [2016-05-11 21:01:15][MyApp][Info][myhost.example.com][PID 1724][ThreadId 1154][main:Helpers.Logging Helpers/Logging.hs:32:7] Started
-- [2016-05-11 21:01:15][MyApp.confrabulation][Debug][myhost.example.com][PID 1724][ThreadId 1154][confrab_factor:42.0][main:Helpers.Logging Helpers/Logging.hs:41:9] Confrabulating widgets, with extra namespace and context
-- [2016-05-11 21:01:15][MyApp][Info][myhost.example.com][PID 1724][ThreadId 1154][main:Helpers.Logging Helpers/Logging.hs:43:7] Namespace and context are back to normal
-- @
-- Returns the newly-created 'Scribe'. The finalizer flushes the handle. Handle mode is set to 'System.IO.LineBuffering' automatically.
mkHandleScribe :: forall es. (KatipE :> es) => ColorStrategy -> Handle -> PermitFunc -> Verbosity -> Eff es Scribe
mkHandleScribe cs h pf v = unsafeEmbedIOE $ liftIO $ K.mkHandleScribe cs h pf v

-- | Logs to a file handle such as stdout, stderr, or a file.
-- Takes a custom 'ItemFormatter' that can be used to format 'Item' as needed.
-- Returns the newly-created 'Scribe'.
-- The finalizer flushes the handle.
-- Handle mode is set to 'System.IO.LineBuffering' automatically.
mkHandleScribeWithFormatter :: forall es. (KatipE :> es) => (forall a. (LogItem a) => ItemFormatter a) -> ColorStrategy -> Handle -> PermitFunc -> Verbosity -> Eff es Scribe
mkHandleScribeWithFormatter ifa cs h pf v = unsafeEmbedIOE $ liftIO $ K.mkHandleScribeWithFormatter ifa cs h pf v

-- | A specialization of 'mkHandleScribe' that takes a 'FilePath' instead of a 'Handle'. It is responsible for opening the file in 'System.IO.AppendMode' and will close the file handle on closeScribe/closeScribes. Does not do log coloring. Sets handle to 'System.IO.LineBuffering' mode.
mkFileScribe :: forall es. (KatipE :> es) => FilePath -> PermitFunc -> Verbosity -> Eff es Scribe
mkFileScribe fp pf v = unsafeEmbedIOE $ liftIO $ K.mkFileScribe fp pf v
