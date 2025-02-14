-- prime factors

-- an extremely simple prime factorization algorithm
factors :: Integer -> [Integer]
factors n = factors' n 2
  where factors' n d
          | n == 1 = []
          | n `mod` d == 0 = d : factors' (n `div` d) d
          | otherwise = factors' n (d + 1)


example = factors 13195

result = maximum $ factors 600851475143

expected = result == 6857