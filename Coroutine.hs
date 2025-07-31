{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Coroutine (
  Coroutine,
  CoroutineCanceled,
  newCoroutine,
  resume,
  cancel,
  Stream,
  newStream,
  pull,
)
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef

data Coroutine i o = Coroutine
  { resume :: i -> IO (Maybe o)
  , cancel :: IO ()
  }

data CoroutineCanceled = CoroutineCanceled
  deriving (Show, Exception)

newCoroutine :: (i -> (o -> IO i) -> IO a) -> IO (Coroutine i o)
newCoroutine f = do
  cin <- newEmptyMVar
  cout <- newEmptyMVar
  running <- newIORef True

  let resume v = do
        isRunning <- readIORef running
        when isRunning $ putMVar cin v

        readMVar cout >>= \case
          Left e -> throwIO e
          Right Nothing -> return Nothing
          Right (Just v) -> do
            takeMVar cout
            return (Just v)
  let yield v = do
        putMVar cout (Right (Just v))
        takeMVar cin

  let task = do
        v <- takeMVar cin
        f v yield
  let complete res = uninterruptibleMask_ $ do
        writeIORef running False
        case res of
          Left e -> putMVar cout (Left e)
          Right _ -> putMVar cout (Right Nothing)

  tid <- forkFinally task complete
  let cancel = throwTo tid CoroutineCanceled

  return (Coroutine{resume, cancel})

type Stream o = Coroutine () o

pull :: Stream o -> IO (Maybe o)
pull (Coroutine resume _) = resume ()

newStream :: ((o -> IO ()) -> IO ()) -> IO (Stream o)
newStream f = newCoroutine $ \_ yield -> f yield

----------------------------------------
-- Example usages

coroutine :: IO ()
coroutine = do
  c <- newCoroutine $ \_ yield -> do
    _ <- yield "a"
    error "Whoops!"
    _ <- yield "b"
    return ()

  resume c () >>= print
  cancel c
  resume c () >>= print

stream :: IO ()
stream = do
  s <- newStream $ \yield -> do
    yield "a"
    yield "b"
    return ()

  pull s >>= print
  pull s >>= print
  pull s >>= print

main :: IO ()
main = do
  coroutine
  stream

-- See: https://research.swtch.com/coro

-- TODO: Stream composition and operators (filter, map, etc.)
