
-- simple recursive fibonacci
-- this is highly inefficient because fib(n-1) and fib(n-2) do almost
-- entirely the same calculation
fib 1 = 1
fib 2 = 2
fib n = fib (n-1) + fib (n-2)

-- zipWith fibonacci
-- this is more efficient than the simple recursive fibonacci 
-- because it doesn't recalculate the same values
-- it uses the laziness in haskell to only calculate the values up to where needed
fibonacci = 1 : 2 : zipWith (+) fibonacci (tail fibonacci)


result = sum $ filter even $ takeWhile (<= 4000000) $ map fib [1..]

result2 = sum $ filter even $ takeWhile (<= 4000000) fibonacci

expected = result2 == 4613732

main = do 
    print result2
    print expected