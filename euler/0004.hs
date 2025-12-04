

-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 * 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

palindrome :: Integer -> Bool
palindrome n = show n == reverse (show n)

example = maximum $ [x * y | x <- [10..99], y <- [10..99], palindrome (x * y)]

result = maximum $ [x * y | x <- [100..999], y <- [(x+1)..999], palindrome (x * y)]

expected = result == 906609

main = do 
    print result
    print expected