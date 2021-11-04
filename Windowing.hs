{-# OPTIONS_GHC -Wall #-}

{- |
[...] you may [...] be stuck with data containing two-digit year fields. You have tried to get it expanded to four digits at the source, but it simply cannot be done.

In this case, you need to adopt a technique called "windowing". Windowing means taking the two-digit year and applying common sense to determine the century that it belongs in.

Obviously, windowing cannot be used for data that might span a period greater than 100 years. For instance, the birth years of people in the general population. There have always been a number of people living beyond the age of 100, and as health care improves that number can only increase. If you code a birth year as "96", that could be either 1996, which would indicate a two-year-old, or 1896, which could indicate a 102-year-old. For a short time after the year 2000, there will be people alive who were born in three different centuries! 

Source: https://www.uic.edu/depts/accc/software/isodates/fixing.html
-}
module Windowing where

import Control.Monad (forM_)

-- | Strategy for solving the @windowing@ problem
type Strategy
  =  Int  -- ^ Reference year
  -> Int  -- ^ Input year
  -> Int  -- ^ Computed year

{- |
2-digit dates are ambiguous on the century. It is interpreted accordingly to the provided reference year as follows:
  - For values between 0 and the reference decade, it's the reference's century
  - For values greater than the current decade, it's the century before the reference's century

Source: EMANUEL, Lautaro Martin
-}
myStrategy :: Strategy
myStrategy refYear n
  | n <= refDecade = n + refCentury * 100
  | n <= 99        = n + (refCentury - 1) * 100
  | otherwise      = n
  where
    refCentury = refYear `div` 100
    refDecade = refYear `mod` 100

{- |
For parsing with the abbreviated year pattern ("y" or "yy"), @SimpleDateFormat@ must interpret the abbreviated year relative to some century. It does this by adjusting dates to be within 80 years before and 20 years after the time the [...] instance is created.

Source: https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html
-}
javaStrategy :: Strategy
javaStrategy refYear n
  | n > 99 = n
  | n - refDecade > 20  = n + (refCentury - 1) * 100
  | refDecade - n >= 80 = n + (refCentury + 1) * 100
  | otherwise           = n + refCentury * 100
  where
    refDecade = refYear `mod` 100
    refCentury = refYear `div` 100

{- |
When a century is not otherwise specified, values in the range [69,99] shall refer to years 1969 to 1999 inclusive, and values in the range [00,68] shall refer to years 2000 to 2068 inclusive [...].

Source: https://pubs.opengroup.org/onlinepubs/009695399/functions/strptime.html
-}
posixStrategy :: Strategy
posixStrategy _refYear n
  | n < 69    = n + 2000
  | n <= 99   = n + 1900
  | otherwise = n

{- |
If you enter a date with a two-digit year in a text formatted cell or as a text argument in a function [...] Excel interprets the year as follows:
  - @00 through 29@: is interpreted as the years 2000 through 2029.
  - @30 through 99@: is interpreted as the years 1930 through 1999. 

Source: https://docs.microsoft.com/en-us/office/troubleshoot/excel/two-digit-year-numbers
-}
officeStrategy :: Strategy
officeStrategy _refYear n
  | n <= 29   = n + 2000
  | n <= 99   = n + 1900
  | otherwise = n

{- |
Date values with 2-digit years are ambiguous because the century is unknown. Such values must be interpreted into 4-digit form because MySQL stores years internally using 4 digits.

For @DATETIME@, @DATE@, and @TIMESTAMP@ types, MySQL interprets dates specified with ambiguous year values using these rules:
  - Year values in the range 00-69 become 2000-2069.
  - Year values in the range 70-99 become 1970-1999.

Source: https://dev.mysql.com/doc/refman/5.7/en/two-digit-years.html
-}
mySqlStrategy :: Strategy
mySqlStrategy _refYear n
  | n <= 69   = n + 2000
  | n <= 99   = n + 1900
  | otherwise = n

{- |
If a 2-digit year is moved to a 4-digit year, the century (1st 2 digits of the year) are chosen as follows:
  - If the 2-digit year is greater than or equal to 40, the century used is 1900. In other words, 19 becomes the first 2 digits of the 4-digit year.
  - If the 2-digit year is less than 40, the century used is 2000. In other words, 20 becomes the first 2 digits of the 4-digit year.

Source: https://www.ibm.com/docs/en/i/7.2?topic=mcdtdi-conversion-2-digit-years-4-digit-years-centuries
-}
ibmIStrategy :: Strategy
ibmIStrategy _refYear n
  | n < 40    = n + 2000
  | n <= 99   = n + 1900
  | otherwise = n

main :: IO ()
main = do
  let inputs   = [95, 74, 50, 34, 26, 18, 2]
  let refYears = [2034, 2015, 1997, 1986]
  let strat    = javaStrategy
  let stratN   = "javaStrategy"

  putStrLn $ "Using strategy '" ++ stratN ++ "'"
  forM_ refYears $ \refYear -> do
    putStrLn $ "With refYear = '" ++ show refYear ++ "':"
    forM_ inputs $ \input -> do
      let result = strat refYear input
      putStrLn $ "  '" ++ show input ++ "' -> '" ++ show result ++ "'"

{-
Other strategies:
- https://github.com/date-fns/date-fns/issues/514
- https://community.alteryx.com/t5/Alteryx-Designer-Discussions/Dates-with-2-digit-year-a-comment/td-p/81236
-}
