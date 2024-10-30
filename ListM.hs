module ListM where

import Control.Monad

harrySue :: [String]
harrySue = do
  name <- ["Harry", "Sue"]
  action <- ["loves", "hates"]
  item <- ["kale", "honey"]
  return $ name ++ " " ++ action ++ " " ++ item ++ "."

exampleList :: [Int]
exampleList = [x | x <- [1 .. 10], even x]

exampleGuard :: [Int]
exampleGuard = do
  x <- [1 .. 10]
  guard (even x)
  return x

-- >>> harrySue
-- ["Harry loves kale.","Harry loves honey.","Harry hates kale.","Harry hates honey.","Sue loves kale.","Sue loves honey.","Sue hates kale.","Sue hates honey."]

-- >>> exampleGuard
-- [2,4,6,8,10]
