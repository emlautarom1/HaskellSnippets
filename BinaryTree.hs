module BinaryTree where

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show)

treeAppend :: Ord a => a -> Tree a -> Tree a
treeAppend v Empty = Node v Empty Empty
treeAppend v (Node x lc rc) =
  if v > x
    then Node x lc (treeAppend v rc)
    else Node x (treeAppend v lc) rc

listToTree :: Ord a => [a] -> Tree a
listToTree = foldl (flip treeAppend) Empty

-- >>> listToTree [3,4,5,1,2]
-- Node 3 (Node 1 Empty (Node 2 Empty Empty)) (Node 4 Empty (Node 5 Empty Empty))
