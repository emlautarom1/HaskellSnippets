-- Prints the `prime_n` prime number
-- Change value to print a different prime number

main :: IO ()
main = let prime_n = 50 in print $ primes !! prime_n

primes :: [Int]
primes = filterPrime [2 ..]
  where
    filterPrime (p : xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]
    filterPrime [] = error "Branch should neber be executed"
