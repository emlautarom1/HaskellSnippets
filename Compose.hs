module Compose where

newtype Compose f g a = Compose {getCompose :: f (g a)}

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose c) = Compose (fmap (fmap f) c)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure
  (Compose a1) <*> (Compose a2) = Compose (fmap (<*>) a1 <*> a2)

instance (Applicative f, Applicative g, Semigroup a) => Semigroup (Compose f g a) where
  a1 <> a2 = fmap (<>) a1 <*> a2

instance (Monad f, Monad g) => Monad (Compose f g) where
  _ >>= _ = error "no, can't do"
