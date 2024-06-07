#!/usr/bin/env cabal
{- cabal:
build-depends:
  base <4.21,
  async ^>=2.2.5
-}
{-# LANGUAGE NumericUnderscores #-}

import Control.Concurrent (threadDelay)

import Control.Concurrent.Async (concurrently_)
import Control.Exception (bracket)
import Control.Monad (forever, void)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Timeout (timeout)

data Shared = MkShared deriving (Show)

buildShared :: IO Shared
buildShared = do
  putStrLn "Building Shared..."
  return MkShared

closeShared :: Shared -> IO ()
closeShared _ = putStrLn "\nClosing Shared..."

runServiceA :: Shared -> IO ()
runServiceA a = do
  putStrLn "Started service A"
  forever $ do
    putStr "A"
    threadDelay 105_000

runServiceB :: Shared -> IO ()
runServiceB a = do
  putStrLn "Started service B"
  forever $ do
    putStr "B"
    threadDelay 95_000

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  void $ timeout 2_000_000 $ bracket buildShared closeShared $ \s ->
    concurrently_ (runServiceA s) (runServiceB s)

  putStrLn "Done"
