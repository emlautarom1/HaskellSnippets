module RainWater where

{-
Trapping Raing Water
See: https://leetcode.com/problems/trapping-rain-water/
-}

solve :: [Int] -> Int
solve [] = 0
solve l@(x : xs) = drops + solve rest
  where
    (b, rest) = span (<= x) l
    bucket = case rest of
      [] -> reverse $ dropIncreasing $ reverse b
      (r : _) -> b <> [r]
    m = head bucket `min` last bucket
    drops = sum $ map (\h -> (m - h) `max` 0) bucket

dropIncreasing :: Ord a => [a] -> [a]
dropIncreasing [] = []
dropIncreasing [x] = [x]
dropIncreasing l@(x : x' : xs)
  | x < x' = dropIncreasing (x' : xs)
  | otherwise = l

testSuite :: Bool
testSuite =
  and
    [ solve [4, 0, 3, 6, 1, 3] == 7,
      solve [2, 0, 2] == 2,
      solve [3, 0, 2, 0, 4] == 7,
      solve [0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1] == 6
    ]
