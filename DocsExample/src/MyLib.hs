{- | An awesome library
-}
module MyLib (someFunc, someOtherFunc) where

-- | Some function.
someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | A more powerful variant of 'someFunc'.
someOtherFunc :: String -> IO ()
someOtherFunc = putStrLn
