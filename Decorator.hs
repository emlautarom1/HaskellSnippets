module Decorator where

import Data.Char (toUpper)

----------------------------------------
-- Library

type Decorator a = a -> a

-- | A base case for a handler that always fails.
noImpl :: Show a => a -> any
noImpl m = error $ "No implementation for method '" <> show m <> "'"

----------------------------------------
-- User code

data Method = Move | Say | Attack
  deriving (Show, Eq)

moveQuickly :: Decorator (Method -> IO ())
moveQuickly _ (Move) = putStrLn "Running forward..."
moveQuickly f m = f m

shout :: String -> Decorator (Method -> IO ())
shout roar _ Say = putStrLn $ (map toUpper roar) <> "!"
shout _ f m = f m

attackWithAxe :: Decorator (Method -> IO ())
attackWithAxe _ (Attack) = putStrLn "Swinging axe..."
attackWithAxe f m = f m

main :: IO ()
main = do
  let viking = shout "Come here" . attackWithAxe . moveQuickly $ noImpl
  let actions = [Move, Say, Attack]
  mapM_ viking actions
