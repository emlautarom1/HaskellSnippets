{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module RecordsAdvanced where

import GHC.OverloadedLabels
import GHC.Records

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
