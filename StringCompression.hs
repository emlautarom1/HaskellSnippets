module StringCompression
{-
  Compress and decompress a String (Run Length Encoding)
  See: https://dev.to/akhilpokle/string-compression-facebook-interview-question-3d2o
-}
  ( compress,
    decompress,
  )
where

import Data.Char (digitToInt)

-- | Compress a String (RLE)
--
-- __Examples__
--
-- >>> compress "aabb"
-- "a3b2"
-- >>> compress "aaabbcccccd"
-- "a3b2c5d1"
compress :: String -> String
compress input = concatMap (\(char, count) -> char : show count) $ foldr step [] input
  where
    step char [] = [(char, 1)]
    step char acc@((previousChar, previousCount) : previous) =
      if previousChar == char
        then (char, previousCount + 1) : previous
        else (char, 1) : acc

-- | Decompress a String (RLE)
--
-- The input 'String' should satisfy the following pattern:
-- > char1 : count1 : char2 : count2 : ...
--
-- __Examples__
--
-- >>> (decompress . compress $ "aabb") == "aabb"
-- True
-- >>> (decompress . compress $ "aaabbcccccd") == "aaabbcccccd"
-- True
decompress :: String -> String
decompress input =
  foldr (\[char, count] res -> replicate (digitToInt count) char ++ res) [] (chunks 2 input)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, zs) = splitAt n xs
   in ys : chunks n zs
