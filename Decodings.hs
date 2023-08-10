module Decodings where

import Control.Monad (guard)
import Data.Char (isAsciiUpper)

-- Original problem:
-- https://leetcode.com/problems/decode-ways/

noLeadingZeros :: String -> Bool
noLeadingZeros = null . takeWhile (== '0')

parseChar :: String -> Maybe Char
parseChar s = do
  guard $ not . null $ s
  guard $ noLeadingZeros s
  let c = toEnum $ (+ 64) $ read s
  guard $ isAsciiUpper c
  return c

-- >>> parseChar ""
-- Nothing

-- >>> parseChar "1"
-- Just 'A'

-- >>> parseChar "26"
-- Just 'Z'

-- >>> parseChar "27"
-- Nothing

decode :: String -> [[Char]]
-- NOTE: The empty string has one decoding, ""
decode [] = [[]]
decode input = do
  toTake <- [1, 2]
  let (chunk, rest) = splitAt toTake input
  guard $ length chunk == toTake -- Required check to avoid duplicated results
  case parseChar chunk of
    Nothing -> []
    Just c -> (c :) <$> decode rest

-- >>> decode ""
-- [""]

-- >>> decode "12"
-- ["AB","L"]

-- >>> decode "226"
-- ["BBF","BZ","VF"]

-- >>> decode "06"
-- []

solve :: String -> Int
solve = length . decode
