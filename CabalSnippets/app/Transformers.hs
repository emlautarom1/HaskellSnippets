{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Transformers where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Map
import qualified Data.Map as Map
import Data.Maybe

alwaysFails :: String -> IO (Either String Int)
alwaysFails = pure . Left

alwaysSucceeds :: Int -> IO (Either String Int)
alwaysSucceeds = pure . Right

handleError' :: (Show e) => Either e a -> IO ()
handleError' (Left err) = print err
handleError' (Right _) = pure ()

exampleExceptT :: ExceptT String IO Int
exampleExceptT = do
  v1 <- ExceptT $ alwaysSucceeds 5
  v2 <- ExceptT $ alwaysFails "Whoops"
  pure (v1 + v2 + 1)

exampleMonadError :: (MonadIO m, MonadError String m) => m Int
exampleMonadError = do
  v1 <- liftEitherIO $ alwaysSucceeds 5
  v2 <- liftEitherIO $ alwaysFails "Whoops"
  pure (v1 + v2 + 1)

liftEitherIO :: (MonadError e m, MonadIO m) => IO (Either e a) -> m a
liftEitherIO a = liftIO a >>= liftEither

main :: IO ()
main = do
  res <- runExceptT exampleExceptT
  -- res <- runExceptT exampleMonadError
  handleError' res
  pure ()

----------------------------------------
-- Effects

class (Monad m) => Logger m where
  logMsg :: String -> m ()

class (Monad m) => MonadKVStore k v m where
  putValue :: k -> v -> m ()
  getValue :: k -> m (Maybe v)
  listValues :: m [v]

----------------------------------------
-- AppM

newtype AppM a = MkAppM {unAppM :: IO a}
  deriving (Functor, Applicative, Monad)

runAppM :: AppM a -> IO a
runAppM = unAppM

instance MonadKVStore Int String AppM where
  putValue k v = MkAppM $ do
    putStrLn $ "Writing " <> show k <> ": " <> show v
  getValue k = MkAppM $ do
    putStrLn $ "Getting value for key: " <> show k
    pure Nothing
  listValues = MkAppM $ pure []

instance Logger AppM where
  logMsg msg = MkAppM $ putStrLn $ "[LOG] " <> msg

----------------------------------------
-- TestM

newtype TestM a = MkTestM {unTestM :: WriterT [String] (State (Map Int String)) a}
  deriving (Functor, Applicative, Monad)

runTestM :: TestM a -> (a, [String])
runTestM = fst . flip runState Map.empty . runWriterT . unTestM

instance MonadKVStore Int String TestM where
  putValue k v = MkTestM $ modify (Map.insert k v)
  getValue k = MkTestM $ gets $ Map.lookup k
  listValues = MkTestM $ gets Map.elems

instance Logger TestM where
  logMsg :: String -> TestM ()
  logMsg msg = MkTestM $ tell [msg]

----------------------------------------
-- App code

example :: (Logger m, MonadKVStore Int String m) => m String
example = do
  putValue @Int 5 "Hello"
  v <- getValue @Int 5
  logMsg "Done with get"
  pure $ fromMaybe "No result" v

examplePure, exampleIO :: IO ()
examplePure = do
  let r = runTestM example
  print r
exampleIO = do
  r <- runAppM example
  print r
