-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- What is the 10,001st prime number?

-- simple isPrime test, very slow
isPrime :: Integer -> Bool
isPrime n = isPrime' n 2
  where isPrime' n d
          | n == 1 = False
          | n == d = True
          | n `mod` d == 0 = False
          | otherwise = isPrime' n (d + 1)

-- faster isPrime test, special casing 2 and 3, then checking 6k +/- 1
-- all primes > 3 are of form 6k+1 or 6k+5 (~6k-1)
-- so here we start at 5 (6-1) and check (5+2) (6+1), then increment by 6
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

example = take 6 primes

result = primes !! 10000

expected = result == 104743
