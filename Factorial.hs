module Factorial where

main :: IO ()
main = do
  print $ fact 9999

-- Use Integer instead of Int to avoid common overflow issues
fact :: Integer -> Integer
fact n = product [1 .. n]
