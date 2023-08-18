{-# LANGUAGE DeriveAnyClass #-}

module ErrorHandling where

import Control.Exception
import Control.Monad
import Text.Read

data Person = Person {age :: Int, alive :: Bool}
  deriving (Show, Eq)

mkPerson1 :: String -> String -> Person
mkPerson1 sAge sAlive = do
  let age = read sAge
      alive = read sAlive
   in Person {age = age, alive = alive}

mkPerson2 :: String -> String -> Maybe Person
mkPerson2 sAge sAlive = do
  age <- readMaybe sAge
  guard (age >= 0)
  alive <- readMaybe sAlive
  return Person {age = age, alive = alive}

data BuildPersonException
  = InvalidAge String
  | InvalidAlive String
  deriving (Show, Eq, Exception)

mkPerson3 :: String -> String -> Either BuildPersonException Person
mkPerson3 sAge sAlive = do
  age <- maybeToEither (InvalidAge sAge) $ readMaybe sAge
  alive <- maybeToEither (InvalidAlive sAlive) $ readMaybe sAlive
  return Person {age = age, alive = alive}

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither err Nothing = Left err
maybeToEither _ (Just a) = Right a
