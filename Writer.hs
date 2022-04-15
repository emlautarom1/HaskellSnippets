module Writer where

import Control.Monad.Trans.Writer ()

data Writer a = Writer a [String]
  deriving (Show, Eq)

instance Functor Writer where
  fmap f (Writer a ms) = Writer (f a) ms

instance Applicative Writer where
  pure = (flip Writer) []
  (Writer fa ms) <*> (Writer a ns) = Writer (fa a) (ms ++ ns)

instance Monad Writer where
  (>>=) = bindWriter

bindWriter :: Writer a -> (a -> Writer b) -> Writer b
bindWriter (Writer a ms) f =
  let Writer b ns = f a
   in Writer b (ms ++ ns)

tell :: [String] -> Writer ()
tell = Writer ()

number :: (Show a, Num a) => a -> Writer a
number n = Writer n ["number: " ++ show n]

example :: Writer Integer
example =
  number 5 `bindWriter` \a ->
    number 6 `bindWriter` \b ->
      number 7 `bindWriter` \c ->
        let s = a + b + c
         in tell ["sum: " ++ show s] `bindWriter` \s ->
              number (a + b + c)

exampleDo :: Writer Integer
exampleDo = do
  a <- number 5
  b <- number 6
  c <- number 7
  let s = a + b + c
  tell ["sum: " ++ show s]
  number (a + b + c)