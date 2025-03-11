{-# LANGUAGE LambdaCase #-}

module Merkle (computeMerkle) where

import Data.Hashable (Hashable, hash)

data Merkle a
  = Leaf !a
  | INode !Int !(Merkle a) !(Merkle a)
  deriving (Show, Eq)

merkleHash :: (Hashable a) => Merkle a -> Int
merkleHash = \case
  (Leaf a) -> hash a
  (INode h _ _) -> h

ppMerkle :: (Show a) => Merkle a -> IO ()
ppMerkle = go 0
  where
    go depth = \case
      Leaf v -> do
        putStrLn $ spaces ++ "> " ++ show v
      INode hash left right -> do
        putStrLn $ spaces ++ "# " ++ show hash
        go (depth + 1) left
        go (depth + 1) right
      where
        spaces = replicate depth ' '

computeMerkle :: (Hashable a) => [a] -> Merkle a
computeMerkle contents =
  let leaves = map Leaf contents
      [merkle] = iterateUntil isSingleton reduce leaves
   in merkle
  where
    reduce nodes = map merge $ chunksOf 2 nodes
    merge [a, b] = INode (hash (merkleHash a + merkleHash b)) a b
    merge [x] = x
    merge _ = error "impossible: chunks should have length 1 or 2"

-- >>> ppMerkle $ computeMerkle ["hello", "world", "hi", "there"]

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _ = False

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (c, rest) = splitAt n xs
   in c : chunksOf n rest

iterateUntil :: (a -> Bool) -> (a -> a) -> a -> a
iterateUntil p f x
  | p x = x
  | otherwise = iterateUntil p f (f x)
