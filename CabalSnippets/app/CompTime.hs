{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module CompTime where

import Language.Haskell.TH.Syntax
import Math
import SystemInfo

main :: IO ()
main = do
  putStrLn $ "Compile time power is: " ++  show @Int $(lift $ power 2 4)
  putStr $(systemInfo)
