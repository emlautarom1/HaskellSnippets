{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH.Syntax
import Math (power)

main :: IO ()
main = do
  print $(lift $ power 2 4)
