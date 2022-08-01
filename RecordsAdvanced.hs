{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RecordsAdvanced where

import GHC.Records
import GHC.OverloadedLabels

data Player = MkPlayer
  { id :: Int,
    age :: Int
  }
  deriving (Show)

data GameMap = MkGameMap
  { id :: Int,
    tiles :: [[Int]]
  }

instance IsLabel "id" (Player -> Int) where
  fromLabel = getField @"id"

instance IsLabel "id" (GameMap -> Int) where
  fromLabel = getField @"id"

player1 :: Player
player1 = MkPlayer {id = 420, age = 20}

main :: IO ()
main = do
  print (#id player1 :: Int)