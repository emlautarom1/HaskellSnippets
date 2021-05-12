{-# LANGUAGE TemplateHaskell #-}

import SystemInfo (systemInfo)

main :: IO ()
main = do
  putStrLn $ "Compile time info\n" <> $(systemInfo)