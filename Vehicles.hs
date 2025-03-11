{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Vehicles where

import Data.List (intercalate)

newtype ParseError = ParseError String
  deriving (Show)

type CarBrand = String
type PlaneSize = String

data Vehicle = Car CarBrand | Airplane PlaneSize
  deriving (Show)

parseVehicle :: String -> Either ParseError Vehicle
parseVehicle _ = Left $ ParseError "not implemented"

warnAboutParseError :: ParseError -> IO ()
warnAboutParseError parseError = do
  putStrLn $ "Error happened while parsing the vehicle: " <> show parseError
  putStrLn $ "Expected `" <> intercalate "` or `" expected <> "`."
  where
    expected = flip map [Car "toyota", Airplane "jumbo"] $ \case
      Car brand -> "car:" <> brand
      Airplane size -> "airplane:" <> size

