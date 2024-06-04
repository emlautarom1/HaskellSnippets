module Once (newOnce, runOnce) where

import Control.Concurrent
import Control.Monad
import Data.IORef

newtype Once = Once (MVar ())

newOnce :: IO Once
newOnce = Once <$> newEmptyMVar

runOnce :: Once -> IO () -> IO ()
runOnce (Once var) action = do
  first <- tryPutMVar var ()
  when first action

example :: IO Int
example = do
  o <- newOnce

  counter <- newIORef (0 :: Int)
  replicateM_ 1_000 $ forkIO $ do
    runOnce o $ do
      modifyIORef' counter (+ 1)

  threadDelay 100 -- Wait for threads to finish
  readIORef counter

-- >>> example
-- 1
