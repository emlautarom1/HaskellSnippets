{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- See /Coroutines for Go/, Russ Cox, 17-07-2024: https://research.swtch.com/coro
module Coroutine (
  -- * Coroutines
  Coroutine,
  CoroutineCanceled,
  newCoroutine,
  resume,
  cancel,

  -- * Streams
  Stream,
  newStream,
  pull,

  -- ** Operators
  await,
  mapS,
  filterS,
  forEach,
  fromListS,
  toListS,

  -- * Abort
  withAbort,
)
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef

-- | A 'Coroutine' is a lightweight, cooperative multitasking construct that allows for pausing and resuming computations.
-- It can be used to implement streams or other asynchronous patterns.
data Coroutine i o = Coroutine
  { resume :: i -> IO (Maybe o)
  -- ^
  -- Indicate to the coroutine to continue execution with the provided input. The resulting value is the next value yielded by the coroutine, or 'Nothing' if the coroutine has completed.
  --
  -- Any exception thrown by the coroutine will be propagated to the caller through 'resume'
  , cancel :: IO ()
  -- ^ Cancel the coroutine, which will stop its execution.
  --
  -- Resuming a canceled coroutine will throw a 'CoroutineCanceled' exception.
  }

-- | 'Exception' thrown when a coroutine is canceled.
--
-- Resuming a canceled coroutine will throw this exception in the caller thread.
data CoroutineCanceled = CoroutineCanceled
  deriving (Show, Exception)

-- | Create a new 'Coroutine' with the provided function. The function takes as an input a 'yield' function which can be used to yield values back to the caller and then waits for further input.
--
-- The 'yield' function should not escape the coroutine's scope.
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

-- | A 'Stream' is a specialized 'Coroutine' that yields values of type 'o' without requiring an specific input.
type Stream o = Coroutine () o

-- | Pull the next value from the stream. If the stream is exhausted, it returns 'Nothing'.
pull :: Stream o -> IO (Maybe o)
pull (Coroutine resume _) = resume ()

-- | Create a new 'Stream' from a function that yields values. The function should take a 'yield' function that is called to produce values for the stream.
newStream :: ((o -> IO ()) -> IO ()) -> IO (Stream o)
newStream f = newCoroutine $ \_ yield -> f yield

-- | Run a computation that can be aborted at any point.
--
-- If the computation is aborted, it returns 'Nothing'. Otherwise, it returns 'Just' 'a'.
withAbort :: (IO () -> IO a) -> IO (Maybe a)
withAbort f = do
  let new = newCoroutine $ \_ yield -> do
        a <- Just <$> f (yield Nothing)
        yield a

  bracket new cancel $ \c -> do
    join <$> resume c ()

----------------------------------------
-- Operators

-- | Consume all values from the stream until exhausted, applying the provided function to each value.
await :: Stream o -> (o -> IO ()) -> IO ()
await c f = do
  mv <- pull c
  case mv of
    Nothing -> cancel c
    Just v -> f v >> await c f

-- | Map a function over the values in the stream, producing a new stream with the transformed values.
mapS :: (a -> b) -> Stream a -> IO (Stream b)
mapS f c = newStream $ \yield ->
  await c $ \v ->
    yield (f v)

-- | Filter the values in the 'Stream' based on a predicate, producing a new 'Stream' with only the values that satisfy the predicate.
filterS :: (a -> Bool) -> Stream a -> IO (Stream a)
filterS p c = newStream $ \yield -> do
  await c $ \v -> do
    when (p v) $ do
      yield v

-- | Apply a function to each value in the 'Stream', consuming the 'Stream' in the process.
forEach :: (o -> IO ()) -> Stream o -> IO ()
forEach f c = await c f

-- | Create a new 'Stream' from a list of values, yielding each value in the list.
fromListS :: [o] -> IO (Stream o)
fromListS = newStream . forM_

-- | Convert a 'Stream' to a list by pulling all values from the 'Stream' until it is exhausted.
--
-- If the 'Stream' is infinite the operation blocks indefinitely.
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
