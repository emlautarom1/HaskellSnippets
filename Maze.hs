{-# OPTIONS_GHC -Wall #-}

module Maze where

import Data.List (elemIndex)

type Position = (Int, Int)

type Maze = [[Char]]

findPositionOf :: Char -> Maze -> Position
findPositionOf c maze = go maze 0
  where
    go (r : rows) rowNum = case elemIndex c r of
      Just colNum -> (rowNum, colNum)
      Nothing -> go rows (1 + rowNum)
    go [] _ = error $ "could not find " ++ show c

isInBounds :: Position -> Maze -> Bool
isInBounds (row, col) maze =
  row >= 0
    && col >= 0
    && (row < length maze)
    && (col < length (head maze))

isNotWall :: Position -> Maze -> Bool
isNotWall (row, col) maze = (maze !! row) !! col /= '#'

adjacentPositions :: Position -> [Position]
adjacentPositions (row, col) =
  [ (row - 1, col), -- Up
    (row, col -1), -- Left
    (row, col + 1), -- Right
    (row + 1, col) -- Down
  ]

solveMaze :: Maze -> [Char]
solveMaze m = asDirections $ go [[start]] []
  where
    start = findPositionOf 'S' m
    end = findPositionOf 'E' m
    go [] _ = error "no solution"
    go (path : paths) visited
      | currentPos == end = path
      | currentPos `elem` visited = go paths visited
      | otherwise =
        let nextPositions = [next | next <- adjacentPositions currentPos, isInBounds next m, isNotWall next m]
         in go (paths ++ map (\next -> path ++ [next]) nextPositions) (currentPos : visited)
      where
        currentPos = last path

asDirections :: [Position] -> [Char]
asDirections (p : p' : ps) = toDirection p p' : asDirections (p' : ps)
asDirections _ = []

toDirection :: Position -> Position -> Char
toDirection (r, c) (r', c')
  | r - 1 == r' && c == c' = 'U'
  | r == r' && c - 1 == c' = 'L'
  | r == r' && c + 1 == c' = 'R'
  | r + 1 == r' && c == c' = 'D'
  | otherwise = error "invalid direction"