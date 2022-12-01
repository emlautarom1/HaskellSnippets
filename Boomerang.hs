import Data.List (tails)

isBoomerang :: Eq a => [a] -> Bool
isBoomerang [a, _, a'] = a == a'
isBoomerang _ = False

solve :: Eq a => [a] -> Int
solve = length . filter isBoomerang . map (take 3) . tails
