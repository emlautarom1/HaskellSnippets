module Natural where

data Natural
  = Zero
  | Succ Natural
  deriving (Eq, Ord)

instance Show Natural where
  show n = go 0 n
    where
      go :: Int -> Natural -> String
      go acc Zero = show acc
      go acc (Succ n) = go (acc + 1) n

instance Num Natural where
  Zero + y = y
  (Succ x) + y = x + Succ y
  abs = id
  signum = const 1
  negate = id
  Zero * y = Zero
  (Succ Zero) * y = y
  (Succ n) * y = n * (y + y)
  fromInteger 0 = Zero
  fromInteger n = Succ (fromInteger (n - 1))
