module I where

import Unsafe.Coerce (unsafeCoerce)

-- Equivalent to `Given` from `Data.Reflection`

class With a where
  summon :: a

newtype Handler a o = Handler ((With a) => o)

using :: forall a o. a -> ((With a) => o) -> o
using handler f = unsafeCoerce (Handler f :: Handler a o) handler

newtype Logger = MkLogger
  { _logMessage :: String -> IO ()
  }

logMessage :: (With Logger) => String -> IO ()
logMessage = _logMessage (summon @Logger)

consoleLogger :: Logger
consoleLogger =
  MkLogger
    { _logMessage = putStrLn
    }

nullLogger :: Logger
nullLogger =
  MkLogger
    { _logMessage = \_ -> return ()
    }

main :: IO ()
main = do
  -- This does not work as intended:
  -- The top level logger takes precedence, so we cannot "override"
  using consoleLogger $ do
    using nullLogger $ do
      logMessage "Hello, World!"