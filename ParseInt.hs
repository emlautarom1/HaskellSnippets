module ParseInt
  ( parseInt
  , unsafeParseInt
  )
where

import Data.Char as Char (digitToInt, isDigit)
import Data.Foldable (foldl')

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
    reverseDigits = mapM safeDigitToInt (reverse str)

parseInt' :: String -> Maybe Int
parseInt' "" = Nothing
parseInt' ('-' : n) = negate <$> parseInt n
parseInt' str = do
  digits <- traverse safeDigitToInt str
  return $ foldl' (\num digit -> num * 10 + digit) 0 digits

unsafeParseInt :: String -> Int
unsafeParseInt str = foldr (\(idx, n) res -> res + n * 10 ^ idx) 0 (zip [0 ..] reverseDigits)
  where
    reverseDigits = Char.digitToInt <$> reverse str

-- >>> parseInt' "-1"
-- Just (-1)
