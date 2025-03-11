{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module EffectfulLogger where

import Control.Monad
import Control.Monad.Extra
import Data.IORef
import Effectful
import Effectful.Dispatch.Dynamic
import System.IO (hPutStrLn)
import UnliftIO.IO
import Prelude hiding (log)

data Severity = Info | Error | Fatal
  deriving (Show, Eq, Ord, Bounded)

data Logger :: Effect where
  Log :: Severity -> String -> Logger m ()

type instance DispatchOf Logger = Dynamic

log :: (Logger :> es) => Severity -> String -> Eff es ()
log severity message = send $ Log severity message

runNoLogger :: Eff (Logger : es) a -> Eff es a
runNoLogger = interpret $ \_ -> \case
  Log _ _ -> return ()

runLoggerAbove :: (Logger :> es) => Severity -> Eff (Logger : es) a -> Eff es a
runLoggerAbove minSeverity = interpret $ \_ -> \case
  Log severity message -> do
    when (severity > minSeverity) $ do
      log severity message

runLoggerAbove' :: (Logger :> es) => Severity -> Eff es a -> Eff es a
runLoggerAbove' minSeverity = interpose $ \_ -> \case
  Log severity message -> do
    when (severity > minSeverity) $ do
      log severity message

runLoggerAbove'' :: Severity -> EffectHandler Logger es -> EffectHandler Logger es -> Eff (Logger : es) a -> Eff es a
runLoggerAbove'' minSeverity h1 h2 = interpret $ \env op -> case op of
  Log severity _ ->
    if severity > minSeverity
      then h1 env op
      else h2 env op

runFileLogger :: (IOE :> es) => FilePath -> Eff (Logger : es) a -> Eff es a
runFileLogger path logger = do
  counter <- liftIO $ newIORef (0 :: Int)
  withFile path WriteMode $ \handle -> do
    interpretWith logger $ \_ -> \case
      Log severity message -> liftIO $ do
        atomicModifyIORef' counter $ \c -> (c + 1, ())
        hPutStrLn handle (prefix severity ++ " " ++ message)
        whenM ((== 10) <$> readIORef counter) $ do
          hFlush handle
          atomicModifyIORef' counter $ const (0, ())

prefix :: Severity -> String
prefix = \case
  Info -> "[INFO]"
  Error -> "[ERROR]"
  Fatal -> "[FATAL]"

program :: (Logger :> es) => Eff es ()
program = do
  log Info "Hello, world!"
  log Error "Something went wrong!"
  log Fatal "Everything is on fire!"

exampleRun :: IO ()
exampleRun = do
  runEff $ runFileLogger "example.log" $ runLoggerAbove' Info $ do
    program

-- >>> exampleRun
