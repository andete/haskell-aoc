import Aoc


blinkOne :: Integer -> [Integer]
blinkOne 0 = [1]
blinkOne i
    | even l = let (a,b) = splitAt (l `div` 2) s in [read a, read b]
    | otherwise = [i* 2024]
        where s = show i
              l = length s

blinkAll :: [Integer] -> [Integer]
blinkAll = concatMap blinkOne

part1_example = do
    part1 7 "day11/example.txt" (day11part1 1)


part1_example2 = do
    part1 22 "day11/example2.txt" (day11part1 6)

part1_input = do
    part1 183248 "day11/input.txt" (day11part1 25)

day11part1 :: Int -> [String] -> Int
day11part1 times field = length $ last $ take (times+1) $ iterate blinkAll rocks
    where rocks :: [Integer] = map read $ words $ head field