module QuickSort (sort) where

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x : xs) = sort lesser ++ [x] ++ sort greater
  where
    lesser = filter (< x) xs
    greater = filter (>= x) xs
