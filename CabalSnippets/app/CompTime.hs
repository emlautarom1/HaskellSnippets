{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module CompTime where

import Language.Haskell.TH.Syntax
import Math

main :: IO ()
main = do
  print @Int $(lift $ power 2 4)
