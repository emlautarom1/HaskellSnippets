{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State

class Monad m => KVStore k v m where
  getAll :: m [(k, v)]
  insert :: k -> v -> m ()

data Cfg = MkCfg {dbPath :: String, debug :: Bool}

newtype AppM a = AppM {unAppM :: ReaderT Cfg IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Cfg)

runAppM :: Cfg -> AppM a -> IO a
runAppM config app = runReaderT (unAppM app) config

newtype TestM k v a = TestM {unTestM :: StateT [(k, v)] IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState [(k, v)])

runTestM :: [(k, v)] -> TestM k v a -> IO (a, [(k, v)])
runTestM init app = runStateT (unTestM app) init

instance KVStore k v AppM where
  getAll = do
    _ <- asks dbPath
    pure []
  insert _ _ = do
    liftIO $ putStrLn "Inserting key/value..."
    pure ()

instance KVStore k v (TestM k v) where
  getAll = get
  insert k v = do
    modify ((k, v) :)

application :: KVStore Char Int m => m [(Char, Int)]
application = do
  insert 'c' (5 :: Int)
  getAll

runApp :: AppM a -> IO a
runApp app = do
  let cfg = MkCfg "asdsad" False
  runAppM cfg app

runTest :: TestM Char Int a -> IO a
runTest app = do
  let s = [('x', 0)]
  (r, s') <- runTestM s app
  print s'
  return r

----------------------------------------
-- Main

main :: IO ()
main = putStrLn "Hello, Haskell!"
