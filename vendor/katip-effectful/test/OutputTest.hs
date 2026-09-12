{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OutputTest where

import Control.Monad (replicateM)
import Control.Monad.Catch
import Data.Text.Lazy.Builder (Builder, fromText)
import Effectful
import Effectful.Katip
import Test.QuickCheck.Instances ()
import Test.Tasty.QuickCheck
import Data.Time

import Data.IORef
import Data.Text (Text)
import Katip (closeScribes)
import Katip qualified as K

--import Debug.Trace

getInteger :: Int -> Gen Int
getInteger s = chooseInt (0, s)

instance Arbitrary Namespace where
  arbitrary = do
    num <- sized getInteger
    Namespace <$> replicateM num arbitrary
  shrink = genericShrink
instance Arbitrary Environment where
  arbitrary = Environment <$> arbitrary
  shrink = genericShrink
instance Arbitrary Severity where
  arbitrary = chooseEnum (minBound, maxBound)
  shrink = genericShrink
instance Arbitrary Verbosity where
  arbitrary = chooseEnum (minBound, maxBound)
  shrink = genericShrink
instance Arbitrary LogStr where
  arbitrary = LogStr <$> arbitrary
  shrink = genericShrink
instance Arbitrary Builder where
  arbitrary = fromText <$> arbitrary
instance Arbitrary SimpleLogPayload where
  arbitrary = sl @Text <$> arbitrary <*> arbitrary

instance Show SimpleLogPayload where
  show = show . toObject

dummyScribe :: IORef [String] -> Scribe
dummyScribe ref = Scribe (appendIORef ref) (pure ()) (permitItem DebugS)

appendIORef :: forall a. (LogItem a) => IORef [String] -> Item a -> IO ()
appendIORef ref val = modifyIORef' ref (\prev -> prev <> [show . fmap toObject $ (val{_itemLoc = Nothing})])

zeroUTC :: UTCTime
zeroUTC = UTCTime (toEnum 0) 0

dummyTimer :: IO UTCTime
dummyTimer = pure zeroUTC

testKatipContextAction :: Namespace -> Eff '[KatipE, IOE] () -> K.KatipContextT IO () -> IO Bool
testKatipContextAction ns eff katip = do
  effRef <- (newIORef mempty :: IO (IORef [String]))
  katipRef <- (newIORef mempty :: IO (IORef [String]))
  leOld <- K.initLogEnv ns "testing"
  let le = leOld {_logEnvTimer = dummyTimer}
  katipLE <- K.registerScribe "test" (dummyScribe katipRef) defaultScribeSettings le
  runEff . startKatipContextE "testing" () ns . localLogEnv (\env -> env {_logEnvTimer = dummyTimer})$ registerScribe "test" (dummyScribe effRef) defaultScribeSettings >> eff
  K.runKatipContextT katipLE () ns $ katip
  _ <- closeScribes katipLE
  val1 <- readIORef effRef
  val2 <- readIORef katipRef
  pure $ val1 == val2

testKatipAction :: Namespace -> Eff '[KatipE, IOE] () -> K.KatipT IO () -> IO Bool
testKatipAction ns eff katip = do
  effRef <- (newIORef mempty :: IO (IORef [String]))
  katipRef <- (newIORef mempty :: IO (IORef [String]))
  leOld <- K.initLogEnv ns "testing"
  let le = leOld {_logEnvTimer = dummyTimer}
  katipLE <- K.registerScribe "test" (dummyScribe katipRef) defaultScribeSettings le
  runEff . startKatipE ns "testing" . localLogEnv (\env -> env {_logEnvTimer = dummyTimer})$ registerScribe "test" (dummyScribe effRef) defaultScribeSettings >> eff
  K.runKatipT katipLE $ katip
  _ <- closeScribes katipLE
  val1 <- readIORef effRef
  val2 <- readIORef katipRef
  pure $ val1 == val2

-- Bare
--
prop_LFBare :: SimpleLogPayload -> Namespace -> Severity -> LogStr -> Property
prop_LFBare slp ns severity str = ioProperty $ do
  testKatipAction ns (logF slp ns severity str) (K.logF slp ns severity str)
prop_LMsgBare :: Namespace -> Severity -> LogStr -> Property
prop_LMsgBare ns severity str = ioProperty $ do
  testKatipAction ns (logMsg ns severity str) (K.logMsg ns severity str)
prop_LTBare :: SimpleLogPayload -> Namespace -> Severity -> LogStr -> Property
prop_LTBare slp ns severity str = ioProperty $ do
  testKatipAction ns ($(logT) slp ns severity str) ($(K.logT) slp ns severity str)
prop_LLocBare :: SimpleLogPayload -> Namespace -> Severity -> LogStr -> Property
prop_LLocBare slp ns severity str = ioProperty $ do
  testKatipAction ns (logLoc slp ns severity str) (K.logLoc slp ns severity str)
prop_LItemBare :: SimpleLogPayload -> Namespace -> Severity -> LogStr -> Property
prop_LItemBare slp ns severity str = ioProperty $ do
  testKatipAction ns (logItem slp ns Nothing severity str) (K.logItem slp ns Nothing severity str)
-- surprisingly logException doesn't catch the exception
prop_LExceptionBare :: SimpleLogPayload -> Namespace -> Severity -> Property
prop_LExceptionBare slp ns severity = ioProperty $ do
  testKatipAction ns (logException slp ns severity (pure ())) (K.logException slp ns severity (pure ()))

prop_LFClass :: SimpleLogPayload -> Namespace -> Severity -> LogStr -> Property
prop_LFClass slp ns severity str = ioProperty $ do
  testKatipAction ns (K.logF slp ns severity str) (K.logF slp ns severity str)
prop_LMsgClass :: Namespace -> Severity -> LogStr -> Property
prop_LMsgClass ns severity str = ioProperty $ do
  testKatipAction ns (K.logMsg ns severity str) (K.logMsg ns severity str)
prop_LTClass :: SimpleLogPayload -> Namespace -> Severity -> LogStr -> Property
prop_LTClass slp ns severity str = ioProperty $ do
  testKatipAction ns ($(K.logT) slp ns severity str) ($(K.logT) slp ns severity str)
prop_LLocClass :: SimpleLogPayload -> Namespace -> Severity -> LogStr -> Property
prop_LLocClass slp ns severity str = ioProperty $ do
  testKatipAction ns (K.logLoc slp ns severity str) (K.logLoc slp ns severity str)
prop_LItemClass :: SimpleLogPayload -> Namespace -> Severity -> LogStr -> Property
prop_LItemClass slp ns severity str = ioProperty $ do
  testKatipAction ns (K.logItem slp ns Nothing severity str) (K.logItem slp ns Nothing severity str)
-- surprisingly logException doesn't catch the exception
prop_LExceptionClass :: SimpleLogPayload -> Namespace -> Severity -> Property
prop_LExceptionClass slp ns severity = ioProperty $ do
  testKatipAction ns (K.logException slp ns severity (pure ())) (K.logException slp ns severity (pure ()))

-- Context
--
prop_LFContext :: SimpleLogPayload -> Namespace -> Severity -> LogStr -> Property
prop_LFContext slp ns severity str = ioProperty $ do
  testKatipContextAction ns (logF slp ns severity str) (K.logF slp ns severity str)
prop_LMsgContext :: Namespace -> Severity -> LogStr -> Property
prop_LMsgContext ns severity str = ioProperty $ do
  testKatipContextAction ns (logMsg ns severity str) (K.logMsg ns severity str)
prop_LTContext :: SimpleLogPayload -> Namespace -> Severity -> LogStr -> Property
prop_LTContext slp ns severity str = ioProperty $ do
  testKatipContextAction ns ($(logT) slp ns severity str) ($(K.logT) slp ns severity str)
prop_LLocContext :: SimpleLogPayload -> Namespace -> Severity -> LogStr -> Property
prop_LLocContext slp ns severity str = ioProperty $ do
  testKatipContextAction ns (logLoc slp ns severity str) (K.logLoc slp ns severity str)
prop_LItemContext :: SimpleLogPayload -> Namespace -> Severity -> LogStr -> Property
prop_LItemContext slp ns severity str = ioProperty $ do
  testKatipContextAction ns (logItem slp ns Nothing severity str) (K.logItem slp ns Nothing severity str)
-- surprisingly logException doesn't catch the exception
prop_LExceptionContext :: SimpleLogPayload -> Namespace -> Severity -> Property
prop_LExceptionContext slp ns severity = ioProperty $ do
  testKatipContextAction ns (logException slp ns severity (pure ())) (K.logException slp ns severity (pure ()))

-- Monadic
prop_LFMEff :: Severity -> Namespace -> LogStr -> Property
prop_LFMEff severity ns str = ioProperty $ do
  testKatipContextAction ns (logFM severity str) (K.logFM severity str)
prop_LTMEff :: Severity -> Namespace -> LogStr -> Property
prop_LTMEff severity ns str = ioProperty $ do
  testKatipContextAction ns ($(logTM) severity str) ($(K.logTM) severity str)
prop_LLocMEff :: Severity -> Namespace -> LogStr -> Property
prop_LLocMEff severity ns str = ioProperty $ do
  testKatipContextAction ns (logLocM severity str) (K.logLocM severity str)
prop_LItemMEff :: Severity -> Namespace -> LogStr -> Property
prop_LItemMEff severity ns str = ioProperty $ do
  testKatipContextAction ns (logItemM Nothing severity str) (K.logItemM Nothing severity str)
prop_LExceptionMEff :: Severity -> Namespace -> Property
prop_LExceptionMEff severity ns = ioProperty $ do
  testKatipContextAction ns (logExceptionM (pure ()) severity) (K.logExceptionM (pure ()) severity)

-- -- class
prop_LFMClass :: Severity -> Namespace -> LogStr -> Property
prop_LFMClass severity ns str = ioProperty $ do
  testKatipContextAction ns (K.logFM severity str) (K.logFM severity str)
prop_LTMClass :: Severity -> Namespace -> LogStr -> Property
prop_LTMClass severity ns str = ioProperty $ do
  testKatipContextAction ns ($(K.logTM) severity str) ($(K.logTM) severity str)
prop_LLocMClass :: Severity -> Namespace -> LogStr -> Property
prop_LLocMClass severity ns str = ioProperty $ do
  testKatipContextAction ns (K.logLocM severity str) (K.logLocM severity str)
prop_LItemMClass :: Severity -> Namespace -> LogStr -> Property
prop_LItemMClass severity ns str = ioProperty $ do
  testKatipContextAction ns (K.logItemM Nothing severity str) (K.logItemM Nothing severity str)
prop_LExceptionMClass :: Severity -> Namespace -> Property
prop_LExceptionMClass severity ns = ioProperty $ do
  testKatipContextAction ns (K.logExceptionM (pure ()) severity) (K.logExceptionM (pure ()) severity)
