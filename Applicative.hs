module Applicative where

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = pure f <*> a <*> b

sumA :: (Foldable t, Applicative f, Num b) => t (f b) -> f b
sumA = foldr (liftA2 (+)) (pure 0)

sum :: (Foldable t, Num b) => t b -> b
sum = foldr (+) 0

-- >>> Prelude.sum $ [1..10]
-- 55

-- >>> sumA $ map Just [1..10]
-- Just 55
