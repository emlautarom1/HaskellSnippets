module MicroRpc (main) where

import Control.Monad (forM_)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Typeable (Proxy (..), TypeRep, Typeable, typeRep)
import Text.Read (readMaybe)

----------------------------------------
-- Lib

data Method = Method
  { f :: String -> Maybe (IO String)
  , expectedTy :: TypeRep
  }

mkMethod :: forall i o. (Typeable i, Read i, Show o) => (i -> IO o) -> Method
mkMethod f =
  Method
    { f = \s -> case readMaybe s of
        Just args -> do
          Just $ show <$> f args
        Nothing -> Nothing
    , expectedTy = typeRep (Proxy :: Proxy i)
    }

data RpcError
  = MismatchedArgs {ty :: TypeRep, value :: String}
  | MethodNotFound {name :: String}

instance Show RpcError where
  show (MismatchedArgs ty value) = "`" <> value <> "` cannot be read as `" <> show ty <> "`"
  show (MethodNotFound name) = "Method `" <> name <> "` not found"

runRpc :: Map String Method -> (String, String) -> IO (Either RpcError String)
runRpc methods (method, args) = do
  case M.lookup method methods of
    Nothing -> return $ Left $ MethodNotFound {name = method}
    Just (Method f expectedTy) -> do
      case f args of
        Nothing -> return $ Left $ MismatchedArgs {ty = expectedTy, value = args}
        Just io -> Right <$> io

----------------------------------------
-- Program

main :: IO ()
main = do
  let even_ :: Int -> Bool
      even_ v = v `mod` 2 == 0

  let reverse_ :: String -> String
      reverse_ str = reverse str

  let custom :: ([Int], Char, String) -> Int
      custom ([_, _, code], _, _) = code

  counter <- newIORef 0
  let faa :: () -> IO Int
      faa () = atomicModifyIORef' counter (\x -> (x + 1, x))

  let methods =
        M.fromList
          [ ("even", mkMethod $ pure . even_)
          , ("reverse", mkMethod $ pure . reverse_)
          , ("faa", mkMethod faa)
          , ("custom", mkMethod $ pure . custom)
          ]
  forM_
    [ ("even", show (1 :: Int))
    , ("reverse", show "hello")
    , ("custom", show ([1, 2, 3], 'c', "FOO"))
    , ("faa", show ())
    , ("faa", show ())
    , ("not-found", show ())
    , ("even", show "boo!")
    ]
    $ \rpc -> do
      result <- runRpc methods rpc
      putStr $ "(" ++ fst rpc ++ ") "
      case result of
        Right v -> putStrLn $ "Result: " <> v
        Left e -> putStrLn $ "Error: " <> show e
