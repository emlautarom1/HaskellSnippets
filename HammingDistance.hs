#!/usr/bin/env runghc

import Control.Monad (when)
import Data.Bits (shiftL, xor, (.&.))
import Data.List (intercalate)

-- | Convert an unsigned Int to its Bit representation
unsignedToBitSequence :: Int -> [Int]
unsignedToBitSequence n
  | n == 0 = [0]
  | n < 0 = error "Solo valores sin signo"
  | otherwise = go n []
  where
    go 0 r = r
    go k rs = go (k `div` 2) (k `mod` 2 : rs)

-- | Reads an Int from the console (stdin)
readInt :: IO Int
readInt = read <$> getLine

-- | Pad a list of Ints with zeros for a given list size
zeroPadLeft :: Int -> [Int] -> [Int]
zeroPadLeft size xs
  | length'xs >= size = xs
  | otherwise = replicate (size - length'xs) 0 ++ xs
  where
    length'xs = length xs

-- | Solves the problem
solve :: Int -> Int -> [Int]
solve start end = go [start] 1
  where
    go steps mask
      | currentStep == end = steps
      | otherwise =
        let needsToSwap = (distance .&. mask) /= 0
            nextStep = [currentStep `xor` mask | needsToSwap]
         in go (steps ++ nextStep) (mask `shiftL` 1)
      where
        currentStep = last steps
        distance = start `xor` end

-- | Formats the solution in a human redable way
prettySolve :: Int -> [Int] -> String
prettySolve dimention steps =
  intercalate " -> " $
    map (show . zeroPadLeft dimention . unsignedToBitSequence) steps

-- | Validates that a node can be represented in a given dimention
validateNode :: Int -> Int -> IO ()
validateNode dimention node =
  when (node < 0 || node > (2 ^ dimention) - 1) $ error "Valor invalido"

main :: IO ()
main = do
  putStrLn "Indique la dimension a tratar:"
  dimention <- readInt
  when (dimention < 1) $ error "Dimension invalida"
  putStrLn "Indique el primer nodo:"
  start <- readInt
  validateNode dimention start
  putStrLn "Indique el segundo nodo:"
  end <- readInt
  validateNode dimention end
  putStrLn "Un camino es:"
  putStrLn $ prettySolve dimention $ solve start end
