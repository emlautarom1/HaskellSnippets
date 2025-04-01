{-# LANGUAGE RecordWildCards #-}

module Eff (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
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

runEff :: Eff () a -> IO a
runEff (MkEff run) = run ()

unliftIO :: ((forall a. Eff es a -> IO a) -> IO b) -> Eff es b
unliftIO f = MkEff $ \env -> f (\(MkEff run) -> run env)

request :: (e :> es) => Eff es e
request = extract <$> MkEff return

locally :: (e :> es) => (e -> e) -> Eff es a -> Eff es a
locally f (MkEff run) = MkEff $ \env -> run (alter f env)

using :: e -> Eff (e ::: es) a -> Eff es a
using impl (MkEff run) = MkEff $ \env -> run (impl ::: env)

usingM :: Eff es e -> Eff (e ::: es) a -> Eff es a
usingM implM inner = implM >>= \impl -> using impl inner

use :: (e -> Eff es a) -> e -> Eff es a
use f = f

useM :: Eff es (e -> Eff es a) -> e -> Eff es a
useM fM inner = fM >>= \f -> f inner

----------------------------------------
-- Minimal `Has` class `(:>)` with a custom tuple as heterogeneous lists

data a ::: b = (:::) !a !b

infixr 1 :::

class a :> t where
  {-# MINIMAL extract, alter #-}
  extract :: t -> a
  alter :: (a -> a) -> t -> t

instance a :> a where
  extract a = a
  alter f = f

instance {-# OVERLAPPING #-} a :> (a ::: x) where
  extract (a ::: _) = a
  alter f (a ::: x) = f a ::: x

instance {-# OVERLAPPABLE #-} (a :> r) => a :> (l ::: r) where
  extract (_ ::: r) = extract r
  alter f (l ::: r) = l ::: alter f r

----------------------------------------
-- User code

data State a = State
  { _get :: forall es. Eff es a
  , _modify :: forall es. (a -> a) -> Eff es ()
  }

get :: (State a :> es) => Eff es a
get = request >>= \State {..} -> _get

modify :: (State a :> es) => (a -> a) -> Eff es ()
modify f = request >>= \State {..} -> _modify f

put :: (State a :> es) => a -> Eff es ()
put a = modify (const a)

localState :: s -> IO (State s)
localState s = do
  ref <- newIORef s
  return $
    State
      { _get = liftIO $ readIORef ref
      , _modify = liftIO . modifyIORef' ref
      }

runState :: State s -> Eff (State s ::: es) a -> Eff es (a, s)
runState impl inner =
  using impl $ do
    a <- inner
    s <- get
    return (a, s)

evalState :: State s -> Eff (State s ::: es) a -> Eff es a
evalState impl inner =
  using impl $ do
    inner

execState :: State s -> Eff (State s ::: es) a -> Eff es s
execState impl inner =
  using impl $ do
    _ <- inner
    get

newtype Reader a = Reader
  { _ask :: forall es. Eff es a
  }

ask :: (Reader a :> es) => Eff es a
ask = request >>= \Reader {..} -> _ask

reader :: a -> Reader a
reader a = Reader {_ask = return a}

newtype Logger = Logger
  { _logMsg :: forall es. String -> Eff es ()
  }

logMsg :: (Logger :> es) => String -> Eff es ()
logMsg msg = request >>= \Logger {..} -> _logMsg msg

noLogger :: Logger
noLogger = Logger {_logMsg = \_ -> return ()}

stdoutLogger :: Logger
stdoutLogger = Logger {_logMsg = liftIO . putStrLn}

stateLogger :: State [String] -> Eff (Logger ::: es) a -> Eff es (a, [String])
stateLogger impl inner = do
  let logger = Logger {_logMsg = \msg -> using impl $ do modify (msg :)}
  r <- using logger $ do inner
  logs <- using impl $ do get
  return (r, reverse logs)

newtype MsgProvider a = MsgProvider
  { _getMsg :: forall es. Eff es a
  }

getMsg :: (MsgProvider a :> es) => Eff es a
getMsg = request >>= \MsgProvider {..} -> _getMsg

stdinMsgProvider :: MsgProvider String
stdinMsgProvider = MsgProvider {_getMsg = liftIO getLine}

newFixedMessageProvider :: [a] -> IO (MsgProvider a)
newFixedMessageProvider initial = do
  ref <- newIORef initial
  return $ MsgProvider {_getMsg = liftIO $ atomicModifyIORef' ref $ \msgs -> (tail msgs, head msgs)}

data Abort = AbortE

newtype AbortException = AbortException {_cause :: String}
  deriving (Show)
instance Exception AbortException

abort :: (Abort :> es) => String -> Eff es a
abort = liftIO . throwIO . AbortException

abortEither :: Eff (Abort ::: es) a -> Eff es (Either String a)
abortEither inner = do
  unliftIO $ \run -> do
    handle
      (\AbortException {..} -> return $ Left _cause)
      (run (using AbortE $ Right <$> inner))

abortThrowIO :: Eff (Abort ::: es) a -> Eff es a
abortThrowIO inner = do
  unliftIO $ \run -> do
    handle
      (\e@(AbortException {}) -> throwIO e)
      (run (using AbortE inner))

newtype Resource = Resource
  { _alloc :: forall es a. Eff es a -> (a -> Eff es ()) -> Eff es ()
  }

alloc :: (Resource :> es) => Eff es a -> (a -> Eff es ()) -> Eff es ()
alloc acquire release = request >>= \Resource {..} -> _alloc acquire release

defer :: (Resource :> es) => Eff es () -> Eff es ()
defer action = alloc (return ()) (const action)

resource :: Eff (Resource ::: es) a -> Eff es a
resource inner = unliftIO $ \run -> do
  ref <- liftIO $ newIORef (return ())
  let impl =
        Resource
          { _alloc = \acquire release -> unliftIO $ \run -> mask_ $ do
              a <- run acquire
              atomicModifyIORef' ref (\finalizer -> (run (release a) `finally` finalizer, ()))
          }
  let release = join $ liftIO $ readIORef ref
  run (using impl inner) `finally` release

newtype Trace = Trace
  { _tracing :: forall es a. String -> Eff es a -> Eff es a
  }

tracing :: (Trace :> es) => String -> Eff es a -> Eff es a
tracing label eff = request >>= \Trace {..} -> _tracing label eff

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

echoServer :: (Logger :> es, MsgProvider String :> es, Abort :> es, Trace :> es, Reader String :> es, State Int :> es, Resource :> es) => Eff es ()
echoServer = do
  logMsg "echo server; type 'exit' to quit"
  defer $ logMsg "echo server; goodbye"

  locally (const noLogger) $ do
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
        modify (+ (1 :: Int))
        logMsg msg
        continue

main :: IO ()
main = do
  let logger = stdoutLogger
  let abort = abortEither
  let trace = logTracing logger

  putStrLn "main: begin"
  (_, count) <-
    runEff
      $ useM (runState <$> liftIO (localState (0 :: Int)))
        . usingM (liftIO $ newFixedMessageProvider ["hello", "world", "exit"])
        . use resource
        . use abort
        . using logger
        . using trace
        . using (reader "42")
      $ echoServer

  putStrLn $ "main: processed " ++ show count ++ " messages"
