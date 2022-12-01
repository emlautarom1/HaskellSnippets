{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Exceptions where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Function
import Data.IORef
import Data.Typeable
import GHC.Conc
import System.Directory.Internal.Prelude (exitFailure)
import System.IO

data AppException = forall e. Exception e => AppException e

instance Show AppException where
  show (AppException e) = "AppException: " <> show e

instance Exception AppException

data InvalidTransaction
  = InvalidTransaction
  | NotEnoughFunds
  deriving (Show, Eq, Exception)

data UnexpectedInput = UnexpectedInput
  deriving (Show, Eq)

instance Exception UnexpectedInput where
  toException = toException . AppException

resourceExample :: IO ()
resourceExample = do
  bracket open close $ \_ -> do
    handle logAndDie $ do
      putStrLn "[program] Doing work"
      fail "[fail] Fatal failure"
  where
    open = putStrLn "[bracket] Opening resource"
    close _ = putStrLn "[bracket] Closing resource"
    logAndDie (e :: SomeException) = do
      putStrLn $ "[logAndDie] Top level exception: " <> displayException e
      exitFailure

main :: IO ()
main = do
  let throwAppExample = do
        putStrLn "Failing with a general AppException..."
        threadDelay 2_000_00
        throwIO $ AppException NotEnoughFunds
  let throwInvalidTransactionExample = do
        putStrLn "Failing with an specific Exception..."
        threadDelay 2_000_00
        throwIO InvalidTransaction
  let throwUnexpectedInputExample = do
        putStrLn "Failing with an specific Exception automatically lifted to AppException..."
        threadDelay 2_000_00
        throwIO UnexpectedInput

  resourceExample

  throwAppExample `catch` (\(e :: AppException) -> print e)
  throwInvalidTransactionExample `catch` (\(e :: InvalidTransaction) -> print e)
  throwUnexpectedInputExample `catch` (\(e :: AppException) -> print e)
