{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Except
import Control.Exception (throw, Exception)

alwaysFails :: String -> IO (Either String Int)
alwaysFails = pure . Left

alwaysSucceeds :: Int -> IO (Either String Int)
alwaysSucceeds = pure . Right

handleError :: Show e => Either e a -> IO ()
handleError (Left err) = print err
handleError (Right _) = pure ()

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
  res <- runExceptT $ exampleExceptT
  res <- runExceptT $ exampleMonadError
  handleError res
  pure ()
