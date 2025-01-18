import Util.Aoc

increasing xs = and $ zipWith (<=) xs (tail xs)
decreasing xs = and $ zipWith (>=) xs (tail xs)
diffOk xs = and $ zipWith (\x y -> let z = abs (x - y) in z >= 1 && z <= 3) xs (tail xs)

part1_example = do
    part1 2 "2024/day02/example.txt" day02part1

part1_input = do
    part1 479 "2024/day02/input.txt" day02part1

part2_example = do
    part2 4 "2024/day02/example.txt" day02part2

part2_input = do
    part2 531 "2024/day02/input.txt" day02part2

safeReport :: (Ord a, Num a) => [a] -> Bool
safeReport xs = (increasing xs || decreasing xs) && diffOk xs

safeReport2 xs = safeReport xs || or [safeReport (take i xs ++ drop (i+1) xs) | i <- [0..length xs - 1]]

day02part1 :: [String] -> Int
day02part1 xs = length $ filter safeReport (map (listToNums . words) xs)

day02part2 :: [String] -> Int
day02part2 xs = length $ filter safeReport2 (map (listToNums . words) xs)