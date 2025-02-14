isPrime2 :: Integer -> Bool
isPrime2 2 = True
isPrime2 3 = True
isPrime2 n 
  | even n = False 
  | n `mod` 3 == 0 = False 
  | otherwise = isPrime2' n 5
    where isPrime2' n i
            | i * i > n = True
            | n `mod` i == 0 = False 
            | n `mod` (i + 2) == 0 = False
            | otherwise = isPrime2' n (i + 6)

primes = filter isPrime2 [2..]

example = sum $ takeWhile (< 10) primes

-- takes about 30 secs but works fine
result = sum $ takeWhile (< 2000000) primes

expected = result == 142913828922