module Stream where

import Control.Concurrent hiding (yield)
import Control.Exception
import Control.Monad
import Control.Monad.Fix

-- TODO: Can we generalize to `Coroutine i o`
newtype Stream a = Stream {_var :: MVar (Either SomeException (Maybe a))}

run :: (Stream a -> IO ()) -> IO (Stream a)
run f = do
  _var <- newEmptyMVar
  let s = Stream _var

  void $ forkIO $ do
    res <- try $ f s
    case res of
      Left e -> putMVar _var (Left e)
      Right _ -> putMVar _var (Right Nothing)

  return s

yield :: Stream a -> a -> IO ()
yield s a = putMVar (_var s) (Right . Just $ a)

pull :: Stream a -> IO (Maybe a)
pull s = do
  res <- takeMVar (_var s)
  case res of
    Right m -> return m
    Left e -> throwIO e

mapS :: (a -> b) -> Stream a -> Stream b -> IO ()
mapS f input output = do
  fix $ \continue -> do
    res <- pull input
    case res of
      Nothing -> return ()
      Just v -> do
        yield output (f v)
        continue

filterS :: (a -> Bool) -> Stream a -> Stream a -> IO ()
filterS p input output = do
  fix $ \continue -> do
    res <- pull input
    case res of
      Nothing -> return ()
      Just v -> do
        when (p v) $ yield output v
        continue

toList :: Stream a -> IO [a]
toList s = reverse <$> go []
 where
  go !acc = do
    res <- pull s
    case res of
      Nothing -> return acc
      Just v -> go (v : acc)

integers :: Stream Int -> IO ()
integers s = do
  forM_ [1 .. 10] $ \n -> do
    yield s n

double :: Stream Int -> Stream Int -> IO ()
double = mapS (* 2)

main :: IO ()
main = do
  numbers <- run integers >>= toList
  print numbers

  -- TODO: Can we get a better composition operator
  s <- run integers >>= run . double >>= run . filterS (< 10)
  fix $ \continue -> do
    v <- pull s
    case v of
      Nothing -> putStrLn "Completed"
      Just n -> do
        putStrLn $ "Pulled: " ++ show n
        continue
