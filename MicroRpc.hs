module MicroRpc (main) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Typeable (Proxy (..), TypeRep, Typeable, typeRep)
import Text.Read (readMaybe)

----------------------------------------
-- Lib

type MethodName = String

type Methods m = Map MethodName (Method m)

type Rpc = (MethodName, String)

data Method m = Method
  { run :: String -> Maybe (m String)
  , expectedTy :: TypeRep
  }

mkMethod :: forall m i o. (Monad m, Typeable i, Read i, Show o) => (i -> m o) -> Method m
mkMethod f =
  Method
    { run = \str -> case readMaybe str of
        Just args -> do
          Just $ show <$> f args
        Nothing -> Nothing
    , expectedTy = typeRep (Proxy :: Proxy i)
    }

data RpcError
  = MismatchedArgs {ty :: TypeRep, args :: String}
  | MethodNotFound {name :: MethodName}

instance Show RpcError where
  show (MismatchedArgs ty value) = "`" <> value <> "` cannot be read as `" <> show ty <> "`"
  show (MethodNotFound name) = "Method `" <> name <> "` not found"

runRpc :: (Monad m) => Methods m -> Rpc -> m (Either RpcError String)
runRpc methods (name, args) = do
  case M.lookup name methods of
    Nothing -> return $ Left $ MethodNotFound {name = name}
    Just (Method run expectedTy) -> do
      case run args of
        Nothing -> return $ Left $ MismatchedArgs {ty = expectedTy, args = args}
        Just mv -> Right <$> mv

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
  let faa :: (MonadIO m, MonadReader Int m) => () -> m Int
      faa () = do
        increment <- ask
        liftIO $ atomicModifyIORef' counter (\x -> (x + increment, x))

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
      result <- flip runReaderT 1 $ runRpc methods rpc
      putStr $ "(" ++ fst rpc ++ ") "
      case result of
        Right v -> putStrLn $ "Result: " <> v
        Left e -> putStrLn $ "Error: " <> show e
