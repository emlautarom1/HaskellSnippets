{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Reader
import Data.Bifunctor
import Data.Has
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)

runApplication :: env -> ReaderT env IO a -> IO a
runApplication = flip runReaderT

-- Each 'Service' is a record of functions.
--
-- We can have as many implementations as we want.
-- Implementations are just values of records of functions.
newtype MessageProvider = MessageProvider {_getMessage :: IO String}

-- We make functions "private", only exposing an utility that gets the resource from the environment
-- and triggers the behavior in a single action,
getMessage :: (MonadIO m, MonadReader e m, Has MessageProvider e) => m String
getMessage = asks getter >>= \r -> liftIO $ _getMessage r

-- | Get's a message from the console
consoleMessageProvider :: MessageProvider
consoleMessageProvider = MessageProvider $ do
  putStrLn "Enter a message: "
  getLine

-- | Get's a constant message
constantMessageProvider :: String -> MessageProvider
constantMessageProvider msg = MessageProvider $ return msg

newtype Logger = Logger {_logMessage :: String -> IO ()}

logMessage :: (MonadIO m, MonadReader e m, Has Logger e) => String -> m ()
logMessage msg = asks getter >>= \r -> liftIO $ _logMessage r msg

-- | Log to the console
consoleLogger :: Logger
consoleLogger = Logger putStrLn

-- | Accumulate logs in a List stored in a IORef
accumLogger :: IO (IORef [String], Logger)
accumLogger = do
  logs <- newIORef []
  return (logs, Logger $ \msg -> liftIO $ modifyIORef' logs (msg :))

-- We can also implement decorators!

-- | Prefix a Logger with any prefix message, like "[LOG]"
prefixLogger :: String -> Logger -> Logger
prefixLogger prefix logger = Logger $ \msg -> _logMessage logger $ prefix ++ msg

newtype MessageConsumer = MessageConsumer {_consumeMessage :: String -> IO ()}

consumeMessage :: (MonadIO m, MonadReader e m, Has MessageConsumer e) => String -> m ()
consumeMessage msg = asks getter >>= \r -> liftIO $ _consumeMessage r msg

-- | Consume a message by printing it to the console
consoleMessageConsumer :: MessageConsumer
consoleMessageConsumer = MessageConsumer putStrLn

-- | Discards all messages
nullMessageConsumer :: MessageConsumer
nullMessageConsumer = MessageConsumer $ const $ return ()

-- Actual business logic
program :: (MonadIO m, MonadReader e m, Has Logger e, Has MessageProvider e, Has MessageConsumer e) => m ()
program = do
  logMessage "Initializing Program"
  msg <- getMessage
  logMessage $ "Got message: " ++ msg
  subprogram msg
  logMessage "Program has completed succesfully"

  return ()

-- We can have as many subprograms as we want and they can have their own dependencies
-- Dependencies get automatically added to the caller's environment constraints
subprogram :: (MonadIO m, MonadReader e m, Has MessageConsumer e) => String -> m ()
subprogram msg = do
  consumeMessage msg

data Env = Env
  { logger :: Logger,
    messageProvider :: MessageProvider,
    messageConsumer :: MessageConsumer
  }

-- It would be nice if we could derive these instances.
-- See: https://github.com/winterland1989/data-has/issues/3
instance Has Logger Env where
  getter = logger
  modifier f e = e {logger = f (logger e)}

instance Has MessageProvider Env where
  getter = messageProvider
  modifier f e = e {messageProvider = f (messageProvider e)}

instance Has MessageConsumer Env where
  getter = messageConsumer
  modifier f e = e {messageConsumer = f (messageConsumer e)}

runInTestMode :: ReaderT Env IO a -> IO ()
runInTestMode app = do
  putStrLn "> Running in test mode"

  (logs, logger) <- second (prefixLogger "[LOG] ") <$> accumLogger
  let messageProvider = constantMessageProvider "constant message"
  let messageConsumer = nullMessageConsumer
  let env = Env {..}

  runApplication env app

  putStrLn "> 'AccumLogger' logs:"
  ls <- reverse <$> readIORef logs
  mapM_ putStrLn ls

-- You can also avoid the 'Env' type by just using a tuple
runInTestNoEnv :: ReaderT (Logger, MessageProvider, MessageConsumer) IO a -> IO ()
runInTestNoEnv app = do
  putStrLn "> Running in test mode (no 'Env')"

  (logs, logger) <- second (prefixLogger "[LOG] ") <$> accumLogger
  let messageProvider = constantMessageProvider "constant message"
  let messageConsumer = nullMessageConsumer
  let env = (logger, messageProvider, messageConsumer)

  runApplication env app

  putStrLn "> 'AccumLogger' logs:"
  ls <- reverse <$> readIORef logs
  mapM_ putStrLn ls

runInProdMode :: ReaderT Env IO a -> IO ()
runInProdMode app = do
  putStrLn "> Running in prod mode"

  let logger = prefixLogger "[LOG] " consoleLogger
  let messageProvider = consoleMessageProvider
  let messageConsumer = consoleMessageConsumer
  let env = Env {..}

  runApplication env app

  return ()

main :: IO ()
main = do
  putStrLn $ replicate 10 '-'
  runInTestMode program
  putStrLn $ replicate 10 '-'
  runInTestNoEnv program
  putStrLn $ replicate 10 '-'
  runInProdMode program
