import Data.Numbers.Primes (primeFactors)
import Data.List (subsequences, nub)

-- scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
-- scanl1 f [x1, x2, x3,...] == [x1, x1 `f` x2, (x1 `f` x2) `f` x3, ...]

-- scanl is like foldl but it keeps a list of the intermediate states
-- it can be interesting to use scanl to see the intermediate states of a fold

triangle = scanl1 (+) [1..]

-- distinct product combinations of prime factors
factorsOf n = nub $ filter (\i -> n `mod` i == 0) $ map product $ subsequences $ primeFactors n
    where f = primeFactors n

factors = map factorsOf triangle

example = last $ head $ filter (\x -> length x > 5) factors

result = last $ head $ filter (\x -> length x > 500) factors

expected = result == 76576500
