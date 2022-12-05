{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- import Optics
import Control.Lens
import Data.Foldable (for_)
import Lib

someDog :: Dog
someDog = MkDog {dogName = "", age = 5}

someCat :: Cat
someCat = MkCat {catName = "", age = 6}

somePets :: [Pet]
somePets = [Dog someDog, Cat someCat]

main :: IO ()
main = do
  for_ somePets $ \pet -> do
    print $ pet ^. #age

    case pet of
      Dog d -> print $ d ^. #age
      Cat c -> print $ c ^. #age

  let pet = Dog someDog
  print $ pet ^. #age
  print @Pet $ pet & #age .~ 2000
  print $ pet & #age %~ (* 2)
