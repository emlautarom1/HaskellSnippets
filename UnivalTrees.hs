module UnivalTrees
  ( Tree (..),
    isUnival,
    univalCount,
  )
where

data Tree a
  = -- | Creates an empty node, a "leaf" of a Tree
    Empty
  | -- | Creates a Tree from a Node with 2 children:
    -- a left and a right, and both are also a Tree
    Node a (Tree a) (Tree a)
  deriving (Show, Eq)

isUnival :: Eq a => Tree a -> Bool
isUnival tree = case tree of
  Empty -> False
  Node v l r -> case (l, r) of
    (Node lv _ _, Node rv _ _) ->
      v == lv && v == rv && isUnival l && isUnival r
    (Node lv _ _, Empty) -> v == lv && isUnival l
    (Empty, Node rv _ _) -> v == rv && isUnival r
    (Empty, Empty) -> True

-- |
-- * Test cases
-- >>> univalCount $ Node 1 (Node 1 Empty Empty) Empty
-- 2
-- >>> univalCount $ Node 0 (Node 1 Empty Empty) Empty
-- 1
-- >>> univalCount $ Node 1 (Node 1 Empty (Node 0 Empty Empty)) Empty
-- 1
univalCount :: Eq a => Tree a -> Int
univalCount tree = case tree of
  Empty -> 0
  Node _ l r -> if isUnival tree then 1 + childCount else childCount
    where
      childCount = univalCount l + univalCount r
