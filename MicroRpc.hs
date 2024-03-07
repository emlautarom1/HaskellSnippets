import Control.Monad (forM_)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Typeable
import Text.Read (readMaybe)

----------------------------------------
-- Lib

data Method = Method
  { f :: String -> Maybe String
  , expectedArgs :: TypeRep
  }

mkMethod :: forall i o. (Typeable i, Read i, Show o) => (i -> o) -> Method
mkMethod f =
  Method
    { f = \s -> case readMaybe s of
        Just args -> Just $ show $ f args
        Nothing -> Nothing
    , expectedArgs = typeRep (Proxy :: Proxy i)
    }

data RpcError
  = MismatchedArgs TypeRep
  | MethodNotFound String

instance Show RpcError where
  show (MismatchedArgs ty) = "Mismatched types, expected `" <> show ty <> "`"
  show (MethodNotFound name) = "Method `" <> name <> "` not found"

runRpc :: Map String Method -> (String, String) -> Either RpcError String
runRpc methods (method, args) = do
  Method func ty <- M.lookup method methods !? MethodNotFound method
  func args !? MismatchedArgs ty

(!?) :: Maybe a -> e -> Either e a
ma !? e = maybe (Left e) Right ma

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

  let methods =
        M.fromList
          [ ("even", mkMethod even_)
          , ("reverse", mkMethod reverse_)
          , ("custom", mkMethod custom)
          ]
  forM_
    [ ("even", show (1 :: Int))
    , ("reverse", show "hello")
    , ("custom", show ([1, 2, 3], 'c', "FOO"))
    , ("not-found", show ())
    , ("even", show "boo!")
    ]
    $ \rpc -> do
      let result = runRpc methods rpc
      putStr $ "(" ++ fst rpc ++ ") "
      case result of
        Right v -> putStrLn $ "Result: " ++ v
        Left e -> print e
