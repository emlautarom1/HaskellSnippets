module CaesarEncoding
  ( encodeCaesar,
    decodeCaesar,
  )
where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

alphabet :: [Char]
alphabet = ['a' .. 'z'] ++ ['A' .. 'Z']

alphabetCharIndex :: Char -> Int
alphabetCharIndex chr = fromJust $ elemIndex chr alphabet

encodeCaesar :: String -> Int -> String
encodeCaesar input offset = map (\chr -> cycle alphabet !! encodedIndex chr) input
  where
    encodedIndex chr = alphabetCharIndex chr + offset

decodeCaesar :: String -> Int -> String
decodeCaesar input offset = map (\chr -> cycle alphabet !! decodedIndex chr) input
  where
    decodedIndex chr
      | idx < 0 = alphabetLength + idx
      | otherwise = idx
      where
        alphabetLength = length alphabet
        idx = alphabetCharIndex chr - realOffset
        realOffset = offset `mod` alphabetLength
