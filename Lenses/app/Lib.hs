{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}

module Lib
  ( Pet (..),
    Dog (..),
    Cat (..),
  )
where

-- import Optics
import Control.Lens
import Data.Generics.Labels ()
import Data.Generics.Product
import GHC.Generics (Generic)

data Pet = Dog Dog | Cat Cat deriving (Show, Generic)

data Dog = MkDog {dogName :: String, age :: Int} deriving (Show, Generic)

data Cat = MkCat {catName :: String, age :: Int} deriving (Show, Generic)

ageL :: Lens' Pet Int
ageL = lens getter setter
  where
    getter = \case
      Dog d -> d ^. #age
      Cat c -> c ^. #age
    setter pet v = case pet of
      Dog d -> Dog $ d & #age .~ v
      Cat c -> Cat $ c & #age .~ v

instance {-# OVERLAPPING #-} HasField "age" Pet Pet Int Int where
  field = ageL

instance {-# OVERLAPPING #-} HasField' "age" Pet Int where
  field' = ageL

-- instance LabelOptic "age" A_Lens Pet Pet Int Int where
--   labelOptic = ageL
