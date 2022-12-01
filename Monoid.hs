data Nat = Z | S Nat deriving (Eq)

instance Show Nat where
  show n = "Nat " <> show (go 0 n)
    where
      go acc Z = acc
      go acc (S n) = go (acc + 1) n

instance Semigroup Nat where
  Z <> a = a
  (S l) <> r = S (l <> r)

instance Monoid Nat where
  mempty = Z

-- >>> S (S (S Z)) <> S (S Z)
-- Nat 5
