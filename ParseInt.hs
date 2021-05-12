module ParseInt
  ( parseInt,
    unsafeParseInt,
  )
where

import Data.Char as Char (digitToInt, isDigit)

safeDigitToInt :: Char -> Maybe Int
safeDigitToInt c =
  if Char.isDigit c
    then Just $ Char.digitToInt c
    else Nothing

{- HLINT ignore "Use <$>" -}
parseInt :: String -> Maybe Int
parseInt "" = Nothing
parseInt str = do
  digits <- reverseDigits
  pure $ foldr (\(idx, n) res -> res + n * 10 ^ idx) 0 (zip [0 ..] digits)
  where
    reverseDigits = sequence $ safeDigitToInt <$> reverse str

unsafeParseInt :: String -> Int
unsafeParseInt str = foldr (\(idx, n) res -> res + n * 10 ^ idx) 0 (zip [0 ..] reverseDigits)
  where
    reverseDigits = Char.digitToInt <$> reverse str
