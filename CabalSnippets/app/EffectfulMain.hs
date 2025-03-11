{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module EffectfulMain where

import Data.List.Extra
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic
import Effectful.State.Static.Local

newtype Key = Key Int

newtype Item = Item String

data DB e :: Effect where
  DBWrite :: Item -> (DB e) m (Either e ())
  DBRead :: Key -> (DB e) m (Either e (Maybe Item))

data DBError = DBDeadLock | DBConnectionError
  deriving (Show, Eq)

type instance DispatchOf (DB e) = Dynamic

dbWrite :: (HasCallStack, DB e :> es, Error e :> es, Show e) => Item -> Eff es ()
dbWrite item = do
  send (DBWrite item) >>= \case
    Left e -> throwError e
    Right _ -> return ()

dbRead :: (HasCallStack, DB e :> es, Error e :> es, Show e) => Key -> Eff es (Maybe Item)
dbRead key = do
  send (DBRead key) >>= \case
    Left e -> throwError e
    Right item -> return item

tryWriteDBOrSendEmailPage :: (DB DBError :> es, Error DBError :> es) => Item -> Eff es ()
tryWriteDBOrSendEmailPage item = do
  dbWrite item `catchError` (\_callstack _error -> return ()) -- Local error handling

runDBIO :: (IOE :> es) => Eff (DB DBError : es) a -> Eff es a
runDBIO = interpret $ \_ -> \case
  DBWrite _ -> do
    liftIO $ putStrLn "DBWrite" -- Could fail due to IO, we might want to use `catchIO`
    return (Right ())
  DBRead _ -> do
    liftIO $ putStrLn "DBRead"
    return (Right Nothing)

runDBPure :: Eff (DB DBError : es) a -> Eff es a
runDBPure = reinterpret (evalState ([] :: [Item], 0 :: Int)) $ \_ -> \case
  DBWrite item -> do
    modify (\(items, idx) -> (items ++ [item], succ idx))
    return (Right ())
  DBRead (Key idx) -> do
    items <- gets fst
    return $ Right (items !? idx) -- Never fails since it's pure

main :: IO ()
main = do
  result <- runEff . runError . runDBIO $ do
    tryWriteDBOrSendEmailPage (Item "Foo")
  case result of
    Left (_callstack, _dbError) -> return () -- Access to top level unhandled Error
    Right () -> return ()
