{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Eff (main) where

import Control.Exception
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.IORef

----------------------------------------
-- `Eff` monad, essentially `ReaderT env IO`

newtype Eff es a = MkEff (es -> IO a)

instance Functor (Eff es) where
  fmap f (MkEff ea) = MkEff $ \env -> do
    a <- ea env
    let b = f a
    return b

instance Applicative (Eff es) where
  pure x = MkEff $ \_ -> return x
  MkEff eab <*> MkEff ea = MkEff $ \env -> do
    ab <- eab env
    a <- ea env
    let b = ab a
    return b

instance Monad (Eff es) where
  return = pure
  MkEff ea >>= faeb = MkEff $ \env -> do
    a <- ea env
    let (MkEff eb) = faeb a
    eb env

instance MonadIO (Eff es) where
  liftIO io = MkEff $ const io

runEff :: es -> Eff es a -> IO a
runEff env (MkEff f) = f env

unliftIO :: ((forall a. Eff es a -> IO a) -> IO b) -> Eff es b
unliftIO f = MkEff $ \env -> f (runEff env)

handler :: (e :> es) => (e -> Eff es a) -> Eff es a
handler f = do
  impl <- extract <$> MkEff return
  f impl

locally :: (e :> es) => (e -> e) -> Eff es a -> Eff es a
locally f (MkEff run) = MkEff $ \env -> run (alter f env)

locally_ :: (e :> es) => e -> Eff es a -> Eff es a
locally_ newImpl = locally (const newImpl)

using :: e -> Eff (e ::: es) a -> Eff es a
using impl (MkEff run) = MkEff $ \env -> run (impl ::: env)

----------------------------------------
-- Minimal `Has` class `(:>)` with tuples as heterogeneous lists

class a :> t where
  {-# MINIMAL extract, alter #-}
  extract :: t -> a
  alter :: (a -> a) -> t -> t

type a ::: b = (a, b)

pattern (:::) :: a -> b -> (a, b)
pattern a ::: b = (a, b)

infixr 1 :::

instance a :> a where
  extract a = a
  alter f = f

instance {-# OVERLAPPING #-} a :> (a ::: x) where
  extract (a, _) = a
  alter f (a, x) = (f a, x)

instance {-# OVERLAPPABLE #-} (a :> r) => a :> (l ::: r) where
  extract (_, r) = extract r
  alter f (l, r) = (l, alter f r)

----------------------------------------
-- User code

data State a = State
  { _get :: forall es. Eff es a
  , _modify :: forall es. (a -> a) -> Eff es ()
  }

get :: (State a :> es) => Eff es a
get = handler $ \State {..} -> _get

modify :: (State a :> es) => (a -> a) -> Eff es ()
modify f = handler $ \State {..} -> _modify f

put :: (State a :> es) => a -> Eff es ()
put a = modify (const a)

newLocalState :: a -> IO (State a)
newLocalState a = do
  ref <- newIORef a
  return $
    State
      { _get = liftIO $ readIORef ref
      , _modify = liftIO . modifyIORef' ref
      }

newtype Reader a = Reader
  { _ask :: forall es. Eff es a
  }

ask :: (Reader a :> es) => Eff es a
ask = handler $ \Reader {..} -> _ask

reader :: a -> Reader a
reader a = Reader {_ask = return a}

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

newFixedMessageProvider :: [a] -> IO (MsgProvider a)
newFixedMessageProvider msgs = do
  ref <- newIORef msgs
  return $ MsgProvider {_getMsg = liftIO $ atomicModifyIORef' ref $ \msgs -> (tail msgs, head msgs)}

newtype Abort = Abort
  { _abort :: forall es a. String -> Eff es a
  }

abort :: (Abort :> es) => String -> Eff es a
abort cause = handler $ \Abort {..} -> _abort cause

throwAbort :: Abort
throwAbort = Abort {_abort = liftIO . throwIO . userError}

newtype Trace = Trace
  { _tracing :: forall es a. String -> Eff es a -> Eff es a
  }

tracing :: (Trace :> es) => String -> Eff es a -> Eff es a
tracing label eff = handler $ \Trace {..} -> _tracing label eff

noTracing :: Trace
noTracing = Trace $ \_ eff -> eff

logTracing :: Logger -> Trace
logTracing logger =
  Trace $ \label eff -> do
    using logger $ do
      logMsg $ ">>> in: " ++ label
    a <- eff
    using logger $ do
      logMsg $ "<<< out: " ++ label
    return a

echoServer :: (Logger :> es, MsgProvider String :> es, Abort :> es, Trace :> es, Reader String :> es) => Eff es ()
echoServer = do
  logMsg "echo server; type 'exit' to quit"
  locally_ noLogger $ do
    secret <- ask
    logMsg $ "secret is " ++ secret
  fix $ \continue -> do
    getMsg >>= \msg -> case msg of
      "exit" -> do
        tracing "exit" $ do
          logMsg "goodbye"
      "abort" -> do
        abort "something went wrong!"
        logMsg "unreachable"
      _ -> do
        logMsg msg
        continue

main :: IO ()
main = do
  let logger = stdoutLogger
  let abort = throwAbort
  let trace = logTracing logger
  msgProvider <- newFixedMessageProvider ["hello", "world", "exit"]
  state <- newLocalState (0 :: Int)

  runEff (logger ::: abort ::: trace ::: msgProvider ::: reader "42") $ do
    echoServer
