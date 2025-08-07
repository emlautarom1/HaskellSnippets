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

withAbort :: (IO () -> IO a) -> IO (Maybe a)
withAbort f = do
  let new = newCoroutine $ \_ yield -> do
        a <- Just <$> f (yield Nothing)
        yield a

  bracket new cancel $ \c -> do
    join <$> resume c ()

----------------------------------------
-- Operators

await :: Stream o -> (o -> IO ()) -> IO ()
await c f = do
  mv <- pull c
  case mv of
    Nothing -> cancel c
    Just v -> f v >> await c f

mapS :: (a -> b) -> Stream a -> IO (Stream b)
mapS f c = newStream $ \yield ->
  await c $ \v ->
    yield (f v)

filterS :: (a -> Bool) -> Stream a -> IO (Stream a)
filterS p c = newStream $ \yield -> do
  await c $ \v -> do
    when (p v) $ do
      yield v

forEach :: (o -> IO ()) -> Stream o -> IO ()
forEach f c = await c f

fromListS :: [o] -> IO (Stream o)
fromListS = newStream . forM_

toListS :: Stream a -> IO [a]
toListS s = go []
 where
  go !acc = do
    mv <- pull s
    case mv of
      Nothing -> do
        cancel s
        return (reverse acc)
      Just v -> go (v : acc)

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

abort :: IO ()
abort = do
  exit <- withAbort $ \abort -> do
    let x = 42
    abort
    return x

  print exit

numbers :: IO [Int]
numbers = do
  fromListS [1 .. 10]
    >>= mapS (* 2)
    >>= filterS (> 10)
    >>= toListS

main :: IO ()
main = do
  coroutine
  stream
  abort
