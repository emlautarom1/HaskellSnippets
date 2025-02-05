{-# LANGUAGE PatternSynonyms #-}

module Eff (main) where

import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Data.IORef

----------------------------------------
-- `Eff` monad, essentially `ReaderT env IO`

newtype Eff env a = MkEff (env -> IO a)

instance Functor (Eff env) where
  fmap f (MkEff ea) = MkEff $ \env -> do
    a <- ea env
    let b = f a
    return b
  {-# INLINE fmap #-}

instance Applicative (Eff env) where
  pure x = MkEff $ \_ -> return x
  {-# INLINE pure #-}
  MkEff eab <*> MkEff ea = MkEff $ \env -> do
    ab <- eab env
    a <- ea env
    let b = ab a
    return b
  {-# INLINE (<*>) #-}

instance Monad (Eff env) where
  return = pure
  {-# INLINE return #-}
  MkEff ea >>= faeb = MkEff $ \env -> do
    a <- ea env
    let (MkEff eb) = faeb a
    eb env
  {-# INLINE (>>=) #-}

runEff :: env -> Eff env a -> IO a
runEff env (MkEff f) = f env
{-# INLINE runEff #-}

----------------------------------------
-- Minimal `Has` class `(:>)` with tuples as heterogeneous lists

class a :> t where
  impl :: t -> a

type a ::: b = (a, b)

pattern (:::) :: a -> b -> (a, b)
pattern a ::: b = (a, b)

infixr 1 :::

instance a :> a where
  impl a = a
  {-# INLINE impl #-}

instance {-# OVERLAPPING #-} a :> (a ::: x) where
  impl (a, _) = a
  {-# INLINE impl #-}

instance {-# OVERLAPPABLE #-} (a :> r) => a :> (l ::: r) where
  impl (_, r) = impl r
  {-# INLINE impl #-}

----------------------------------------
-- User code

newtype Logger = Logger
  { _logMsg :: String -> IO ()
  }

logMsg :: (Logger :> env) => String -> Eff env ()
logMsg msg = MkEff $ \env -> _logMsg (impl env) msg
{-# INLINE logMsg #-}

newtype MsgProvider a = MsgProvider
  { _getMsg :: IO a
  }

getMsg :: (MsgProvider a :> env) => Eff env a
getMsg = MkEff $ \env -> _getMsg (impl env)
{-# INLINE getMsg #-}

newtype Abort = Abort
  { _abort :: String -> IO ()
  }

abort :: (Abort :> env) => String -> Eff env ()
abort cause = MkEff $ \env -> _abort (impl env) cause
{-# INLINE abort #-}

defAbort :: Abort
defAbort = Abort {_abort = throwIO . userError}

echoServer :: (Logger :> es, MsgProvider String :> es, Abort :> es) => Eff es ()
echoServer = do
  logMsg "echo server; type 'exit' to quit"
  fix $ \continue -> do
    getMsg >>= \msg -> case msg of
      "exit" -> do
        logMsg "goodbye"
      "abort" -> do
        abort "something went wrong!"
      _ -> do
        logMsg msg
        continue

main :: IO ()
main = do
  let logger = Logger {_logMsg = putStrLn}
  let noLogger = Logger {_logMsg = \_ -> return ()}

  let stdinMsgProvider = MsgProvider {_getMsg = getLine}
  fixedMsgProvider <- do
    msgs <- newIORef ["Hello", "World", "exit"]
    return $ MsgProvider {_getMsg = atomicModifyIORef msgs $ \msgs -> (tail msgs, head msgs)}

  runEff (logger ::: stdinMsgProvider ::: defAbort) $ do
    echoServer
