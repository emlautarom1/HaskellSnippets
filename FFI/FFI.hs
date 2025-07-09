{-# LANGUAGE CApiFFI #-}

module FFI where

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
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr (sizeOf (undefined :: CInt))
    return $ MyStruct a b

  poke ptr (MyStruct a b) = do
    pokeByteOff ptr 0 a
    pokeByteOff ptr (sizeOf (undefined :: CInt)) b

foreign import capi "header.h do_something" c_doSomething :: Ptr MyStruct -> IO Int

doSomething :: MyStruct -> Int
doSomething ms = unsafeDupablePerformIO $ do
  with ms c_doSomething