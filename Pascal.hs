module Pascal where

-- https://mathworld.wolfram.com/PascalsTriangle.html
--
-- Compute Pascal's triangle up to a given number of rows.
--
-- In Pascal's Triangle each number is computed by adding the numbers to the right and left of the current position in the previous row.
--
--     1
--    1 1
--   1 2 1
--  1 3 3 1
-- 1 4 6 4 1
-- # ... etc
-- pascal n
-- pascal 1 = [[1]]
-- pascal 2 = [[1], [1, 1]]
-- pascal 3 = [[1], [1, 1], [1, 2, 1]]
-- pascal 4 = [[1], [1, 1], [1, 2, 1], [1, 3, 3, 1]]
-- pascal 5 = [[1], [1, 1], [1, 2, 1], [1, 3, 3, 1], [1,4,6,4,1]]

nextStep :: [Int] -> [Int]
nextStep row = zipWith (+) (init extended) (tail extended)
  where
    extended = 0 : row ++ [0]

pascal :: Int -> [[Int]]
pascal n = take n $ iterate nextStep [1]

-- >>> pascal 4
-- [[1],[1,1],[1,2,1],[1,3,3,1]]