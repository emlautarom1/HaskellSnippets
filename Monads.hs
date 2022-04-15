{-# LANGUAGE NoMonomorphismRestriction #-}

module Monads where

import Prelude hiding (sequence)

ioPredicate :: Int -> IO Bool
ioPredicate n = return $ n > 5

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (ma : mas) = do
  a <- ma
  as <- sequence mas
  return $ a : as

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p [] = return []
filterM p (x : xs) = do
  bool <- p x
  rest <- filterM p xs
  return $ if bool then x : rest else rest

main :: IO ()
main = do
  res <- filterM ioPredicate [1 .. 10]
  print res
