{-# LANGUAGE CApiFFI #-}

module FFI () where

import Foreign
import Foreign.C.Types
import System.IO.Unsafe

data MyStruct = MyStruct
  { a :: !CInt
  , b :: !CInt
  }
  deriving (Show)

instance Storable MyStruct where
  sizeOf _ = 2 * sizeOf (undefined :: CInt)
  alignment _ = sizeOf (undefined :: CInt)
  peek ptr = do
    aVal <- peekByteOff ptr 0
    bVal <- peekByteOff ptr (sizeOf (undefined :: CInt))
    return $ MyStruct aVal bVal
  poke ptr (MyStruct aVal bVal) = do
    pokeByteOff ptr 0 aVal
    pokeByteOff ptr (sizeOf (undefined :: CInt)) bVal

foreign import capi "header.h do_something" c_doSomething :: Ptr MyStruct -> Int

doSomething :: MyStruct -> Int
doSomething ms = unsafeDupablePerformIO $ do
  alloca $ \ptr -> do
    poke ptr ms
    return $ c_doSomething ptr