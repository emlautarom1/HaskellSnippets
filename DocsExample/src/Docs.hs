{- |
How to use "MyLib" tutorial
-}
module Docs where

import MyLib (someFunc, someOtherFunc)

-- | This is an example function that uses 'MyLib'.
example :: IO ()
example = do
  someFunc
  someOtherFunc "someOtherFunc"
  return ()
