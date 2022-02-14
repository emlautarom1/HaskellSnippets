module Quiescent where
import Data.List (find)
import Data.Maybe (listToMaybe)

quiescent :: Eq t => (t -> t) -> t -> Bool
quiescent f v = v == f v

findQuiescent :: Eq a => (a -> a) -> a -> a
findQuiescent f seed = head $ removeProgress $ iterate f seed
  where
    removeProgress = dropWhile (not . quiescent f)

-------------------------------------------------
-- Example language with a simplification process

data Expr = Nat Int
          | Add Expr Expr
  deriving (Show, Eq)

simplify :: Expr -> Expr
simplify (Add l (Nat 0)) = simplify l
simplify (Add (Nat 0) r) = simplify r
simplify (Add l r)       = Add (simplify l) (simplify r)
simplify (Nat x)         = Nat x

someExpr :: Expr
someExpr = Add _1 (Add (Add _0 _0) (Add _0 _0))
  where
    _0 = Nat 0
    _1 = Nat 1

-- >>> findQuiescent simplify someExpr
-- Nat 1