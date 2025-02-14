
sumSquares :: [Integer] -> Integer
sumSquares = sum . map (^2)

squareSums :: [Integer] -> Integer
squareSums = (^2) . sum

diff :: [Integer] -> Integer
diff xs = squareSums xs - sumSquares xs

example = diff [1..10]

result = diff [1..100]

expected = result == 25164150