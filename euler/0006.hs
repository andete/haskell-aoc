
-- The sum of the squares of the first ten natural numbers is,
-- 1^2 + 2^2 + ... + 10^2 = 385
-- The square of the sum of the first ten natural numbers is,
-- (1 + 2 + ... + 10)^2 = 55^2 = 3025
-- Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 - 385 = 2640.
-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

sumSquares :: [Integer] -> Integer
sumSquares = sum . map (^2)

squareSums :: [Integer] -> Integer
squareSums = (^2) . sum

diff :: [Integer] -> Integer
diff xs = squareSums xs - sumSquares xs

example = diff [1..10]

result = diff [1..100]

expected = result == 25164150