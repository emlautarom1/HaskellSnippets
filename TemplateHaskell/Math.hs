module Math where

power :: Int -> Int -> Int
power a 0 = 1
power a n = a * power a (n -1)
