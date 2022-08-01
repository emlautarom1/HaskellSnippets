{-# OPTIONS_GHC -Wall #-}

module Utils where

indexOf :: Eq a => a -> [a] -> Maybe Int
indexOf e l = go (zip [0 ..] l)
  where
    go [] = Nothing
    go ((idx, x) : xs) = if e == x then Just idx else go xs

replaceAt :: Int -> a -> [a] -> [a]
replaceAt 0 rep (_ : xs) = rep : xs
replaceAt idx rep (x : xs) = x : replaceAt (idx - 1) rep xs
replaceAt _ _ [] = []

replaceWhen :: (a -> Bool) -> a -> [a] -> [a]
replaceWhen p rep = map (\e -> if p e then rep else e)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = let (ys, zs) = splitAt n xs in ys : chunks n zs

permutations :: (Eq a, Show a) => [a] -> [[a]]
permutations [] = [[]]
permutations xs = do
  x <- xs
  xs' <- permutations (filter (/= x) xs)
  return $ x : xs'
