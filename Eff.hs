{-# LANGUAGE PatternSynonyms #-}

module Eff (main) where

import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
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

instance MonadIO (Eff env) where
  liftIO io = MkEff $ const io
  {-# INLINE liftIO #-}

runEff :: env -> Eff env a -> IO a
runEff env (MkEff f) = f env
{-# INLINE runEff #-}

locally :: (e :> es) => (e -> e) -> Eff es a -> Eff es a
locally f (MkEff run) = MkEff $ \env ->
  let currImpl = extract env
      newImpl = f currImpl
   in run (replace newImpl env)
{-# INLINE locally #-}

locally_ :: (e :> es) => e -> Eff es a -> Eff es a
locally_ newImpl = locally (const newImpl)
{-# INLINE locally_ #-}

using :: e -> Eff (e ::: es) () -> Eff es ()
using impl (MkEff run) = MkEff $ \env -> run (impl ::: env)
{-# INLINE using #-}

----------------------------------------
-- Minimal `Has` class `(:>)` with tuples as heterogeneous lists

class a :> t where
  {-# MINIMAL extract, replace #-}
  extract :: t -> a
  replace :: a -> t -> t

type a ::: b = (a, b)

pattern (:::) :: a -> b -> (a, b)
pattern a ::: b = (a, b)

infixr 1 :::

instance a :> a where
  extract a = a
  {-# INLINE extract #-}
  replace _ a = a
  {-# INLINE replace #-}

instance {-# OVERLAPPING #-} a :> (a ::: x) where
  extract (a, _) = a
  {-# INLINE extract #-}
  replace a (_, x) = (a, x)
  {-# INLINE replace #-}

instance {-# OVERLAPPABLE #-} (a :> r) => a :> (l ::: r) where
  extract (_, r) = extract r
  {-# INLINE extract #-}
  replace a (l, r) = (l, replace a r)
  {-# INLINE replace #-}

----------------------------------------
-- User code

newtype Logger = Logger
  { _logMsg :: String -> IO ()
  }

logMsg :: (Logger :> env) => String -> Eff env ()
logMsg msg = MkEff $ \env -> _logMsg (extract env) msg
{-# INLINE logMsg #-}

newtype MsgProvider a = MsgProvider
  { _getMsg :: IO a
  }

getMsg :: (MsgProvider a :> env) => Eff env a
getMsg = MkEff $ \env -> _getMsg (extract env)
{-# INLINE getMsg #-}

newtype Abort = Abort
  { _abort :: String -> IO ()
  }

abort :: (Abort :> env) => String -> Eff env ()
abort cause = MkEff $ \env -> _abort (extract env) cause
{-# INLINE abort #-}

defAbort :: Abort
defAbort = Abort {_abort = throwIO . userError}

echoServer :: (Logger :> es, MsgProvider String :> es, Abort :> es) => Eff es ()
echoServer = do
  logMsg "echo server; type 'exit' to quit"
  locally_ noLogger $ do
    logMsg "secret"
  fix $ \continue -> do
    getMsg >>= \msg -> case msg of
      "exit" -> do
        tracing "exit" $
          logMsg "goodbye"
      "abort" -> do
        abort "something went wrong!"
      _ -> do
        logMsg msg
        continue

noLogger :: Logger
noLogger = Logger {_logMsg = \_ -> return ()}

tracing :: (Logger :> es) => String -> Eff es a -> Eff es a
tracing msg action = do
  logMsg $ ">>> starting: " ++ msg
  a <- action
  logMsg $ "<<< done: " ++ msg
  return a

main :: IO ()
main = do
  let logger = Logger {_logMsg = putStrLn}
  let stdinMsgProvider = MsgProvider {_getMsg = getLine}
  fixedMsgProvider <- do
    msgs <- newIORef ["Hello", "World", "exit"]
    return $ MsgProvider {_getMsg = atomicModifyIORef' msgs $ \msgs -> (tail msgs, head msgs)}

  runEff (logger ::: fixedMsgProvider ::: defAbort) $ do
    echoServer
