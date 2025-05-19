module HKT (main) where

import Control.Concurrent.STM
import Data.Coerce

newtype Identity a = Identity {runIdentity :: a}

instance Show a => Show (Identity a) where
  show (Identity a) = show a

data Block' s = Block
  { id :: s Int
  , hash :: s Word
  , transactions :: s [Int]
  }

type BlockBuilder = Block' TVar
type Block = Block' Identity

deriving instance Show (Block' Identity)

newBlock :: STM BlockBuilder
newBlock = do
  id <- newTVar 0
  hash <- newTVar 0
  transactions <- newTVar []
  return $ Block id hash transactions

snapshot :: BlockBuilder -> STM Block
snapshot (Block id hash transactions) = do
  id' <- coerce <$> readTVar id
  hash' <- coerce <$> readTVar hash
  transactions' <- coerce <$> readTVar transactions
  return $ Block id' hash' transactions'

addTx :: BlockBuilder -> Int -> STM ()
addTx (Block _ _ transactions) tx = do
  modifyTVar transactions (++ [tx])

main :: IO ()
main = do
  builder <- atomically newBlock

  atomically $ do
    addTx builder 1
    addTx builder 2

  atomically $ do
    addTx builder 3
    addTx builder 4

  block <- atomically $ do
    snapshot builder

  print block
