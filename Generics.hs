{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Generics where

import GHC.Generics

data Player = MkPlayer {name :: String, age :: Int}
  deriving (Show, Eq, Generic, Sizeable)

data User
  = AdminUser
  | NormalUser {isActive :: Bool}
  deriving (Show, Eq, Generic, Sizeable)

instance Sizeable a => Sizeable [a] where
  sizeOf = sum . map sizeOf

instance Sizeable Bool where
  sizeOf _ = 1

instance Sizeable Int where
  sizeOf _ = 1

instance Sizeable Char where
  sizeOf _ = 1

instance (Sizeable a, Sizeable b) => Sizeable (a, b) where
  sizeOf (a, b) = sizeOf a + sizeOf b

class Sizeable a where
  -- | Compute the size of a value.
  sizeOf :: a -> Int
  default sizeOf :: (Generic a, GSizeable (Rep a)) => a -> Int
  sizeOf = gsizeOf . from

-- data    V1        p                       -- lifted version of Empty
-- data    U1        p = U1                  -- lifted version of ()
-- data    (:+:) f g p = L1 (f p) | R1 (g p) -- lifted version of Either
-- data    (:*:) f g p = (f p) :*: (g p)     -- lifted version of (,)
-- newtype K1    i c p = K1 { unK1 :: c }    -- a container for a c
-- newtype M1  i t f p = M1 { unM1 :: f p }  -- a wrapper

class GSizeable f where
  gsizeOf :: f t -> Int

instance GSizeable V1 where
  gsizeOf _ = 0

instance GSizeable U1 where
  gsizeOf _ = 0

instance (GSizeable l, GSizeable r) => GSizeable (l :+: r) where
  gsizeOf (L1 x) = gsizeOf x
  gsizeOf (R1 x) = gsizeOf x

instance (GSizeable l, GSizeable r) => GSizeable (l :*: r) where
  gsizeOf (l :*: r) = gsizeOf l + gsizeOf r

instance (Sizeable c) => GSizeable (K1 i c) where
  gsizeOf (K1 c) = sizeOf c

instance (GSizeable f) => GSizeable (M1 i t f) where
  gsizeOf (M1 x) = gsizeOf x

----------------------------------------
-- Tests

-- >>> sizeOf (undefined :: Player)
