{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Eff (main) where

import Control.Exception
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

instance Applicative (Eff env) where
  pure x = MkEff $ \_ -> return x
  MkEff eab <*> MkEff ea = MkEff $ \env -> do
    ab <- eab env
    a <- ea env
    let b = ab a
    return b

instance Monad (Eff env) where
  return = pure
  MkEff ea >>= faeb = MkEff $ \env -> do
    a <- ea env
    let (MkEff eb) = faeb a
    eb env

instance MonadIO (Eff env) where
  liftIO io = MkEff $ const io

runEff :: es -> Eff es a -> IO a
runEff env (MkEff f) = f env

handler :: (e :> es) => (e -> Eff es a) -> Eff es a
handler f = do
  h <- extract <$> MkEff return
  f h

locally :: (e :> es) => (e -> e) -> Eff es a -> Eff es a
locally f (MkEff run) = MkEff $ \env ->
  let currImpl = extract env
      newImpl = f currImpl
   in run (replace newImpl env)

locally_ :: (e :> es) => e -> Eff es a -> Eff es a
locally_ newImpl = locally (const newImpl)

using :: e -> Eff (e ::: es) a -> Eff es a
using impl (MkEff run) = MkEff $ \env -> run (impl ::: env)

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
  replace _ a = a

instance {-# OVERLAPPING #-} a :> (a ::: x) where
  extract (a, _) = a
  replace a (_, x) = (a, x)

instance {-# OVERLAPPABLE #-} (a :> r) => a :> (l ::: r) where
  extract (_, r) = extract r
  replace a (l, r) = (l, replace a r)

----------------------------------------
-- User code

newtype Logger = Logger
  { _logMsg :: forall es. String -> Eff es ()
  }

logMsg :: (Logger :> es) => String -> Eff es ()
logMsg msg = handler $ \Logger {..} -> _logMsg msg

noLogger :: Logger
noLogger = Logger {_logMsg = \_ -> return ()}

stdoutLogger :: Logger
stdoutLogger = Logger {_logMsg = liftIO . putStrLn}

newtype MsgProvider a = MsgProvider
  { _getMsg :: forall es. Eff es a
  }

getMsg :: (MsgProvider a :> es) => Eff es a
getMsg = handler $ \MsgProvider {..} -> _getMsg

stdinMsgProvider :: MsgProvider String
stdinMsgProvider = MsgProvider {_getMsg = liftIO getLine}

mkFixedMessageProvider :: [a] -> IO (MsgProvider a)
mkFixedMessageProvider msgs = do
  msgs <- newIORef msgs
  return $ MsgProvider {_getMsg = liftIO $ atomicModifyIORef' msgs $ \msgs -> (tail msgs, head msgs)}

newtype Abort = Abort
  { _abort :: forall es. String -> Eff es ()
  }

abort :: (Abort :> env) => String -> Eff env ()
abort cause = handler $ \Abort {..} -> _abort cause

throwAbort :: Abort
throwAbort = Abort {_abort = liftIO . throwIO . userError}

newtype Trace = Trace
  { _tracing :: forall es a. String -> Eff es a -> Eff es a
  }

tracing :: (Trace :> es) => String -> Eff es a -> Eff es a
tracing label action = handler $ \Trace {..} -> _tracing label action

noTracing :: Trace
noTracing = Trace $ \_ action -> action

logTracing :: Logger -> Trace
logTracing logger =
  Trace $ \label action -> do
    using logger $ do
      logMsg $ ">>> in: " ++ label
    a <- action
    using logger $ do
      logMsg $ "<<< out: " ++ label
    return a

echoServer :: (Logger :> es, MsgProvider String :> es, Abort :> es, Trace :> es) => Eff es ()
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

main :: IO ()
main = do
  let logger = stdoutLogger
  let abort = throwAbort
  let trace = noTracing
  msgProvider <- mkFixedMessageProvider ["hello", "world", "exit"]

  runEff (logger ::: abort ::: trace ::: msgProvider) $ do
    echoServer
