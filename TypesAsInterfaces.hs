{-# LANGUAGE DataKinds #-}

-- | Source: https://two-wrongs.com/types-as-interfaces
module TypesAsInterfaces where

import GHC.Records

type PlayerId = Int

type UTCTime = String

data Quote = Quote
  { _proposal :: String
  , _premium :: Int
  , _share :: Int
  }

data Msg a = Msg
  { _sender :: PlayerId
  , _recipient :: PlayerId
  , _payload :: a
  }

data Timestamped a = Timestamped
  { _timestamp :: UTCTime
  , _contents :: a
  }

example1 :: Timestamped (Msg Quote)
example1 =
  Timestamped
    { _timestamp = "2024"
    , _contents =
        Msg
          { _sender = 1
          , _recipient = 2
          , _payload = Quote {_proposal = "hello", _premium = 1, _share = 2}
          }
    }

example2 :: Msg (Timestamped Quote)
example2 =
  Msg
    { _sender = 1
    , _recipient = 2
    , _payload =
        Timestamped
          { _timestamp = "2024"
          , _contents = Quote {_proposal = "hello", _premium = 1, _share = 2}
          }
    }

msgFor :: Msg a -> PlayerId
msgFor = _recipient

-- NOTE: Does not compile
-- msgForExample1 :: PlayerId
-- msgForExample1 = msgFor example1

-- This one compiles
msgForExample2 :: PlayerId
msgForExample2 = msgFor example2

-- How can we write `msgFor` to be compatible with any ordering?
-- We need to use type classes:
class HasRecipient a where
  recipient :: a -> PlayerId

instance HasRecipient (Msg a) where
  recipient = _recipient

instance (HasRecipient a) => HasRecipient (Timestamped a) where
  recipient = recipient . _contents

msgFor' :: (HasRecipient a) => a -> PlayerId
msgFor' = recipient

-- NOTE: Now this works
msgForExample1' :: PlayerId
msgForExample1' = msgFor' example1

msgForExample2' :: PlayerId
msgForExample2' = msgFor' example2

----------------------------------------
-- Generic approach using `HasField`

-- This class and instances could be derived automatically
instance HasField "recipient" (Msg a) PlayerId where
  getField = _recipient

instance (HasField "recipient" a PlayerId) => HasField "recipient" (Timestamped a) PlayerId where
  getField = getField @"recipient" . _contents

msgFor'' :: (HasField "recipient" a PlayerId) => a -> PlayerId
msgFor'' = getField @"recipient"

msgForExample1'' :: PlayerId
msgForExample1'' = msgFor'' example1

msgForExample2'' :: PlayerId
msgForExample2'' = msgFor'' example2
