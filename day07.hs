{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Aoc
import Debug.Trace (trace)
import Text.Printf (printf)

part1_example = do
    part1 3749 "day07/example.txt" day07part1

part1_input = do
    part1 850435817339 "day07/input.txt" day07part1


part2_example = do
    part2 11387 "day07/example.txt" day07part2

part2_input = do
    part2 104824810233437 "day07/input.txt" day07part2


parse:: String -> (Integer, [Integer])
parse s =
    let result = read $ take (length first - 1) first
        values = map read (tail w)
        in (result, values)
    where w = words s
          first = head w

canMake :: Integer -> [Integer] -> Bool
canMake a [b] = a == b
canMake a (b:c:bs) = canMake a ((b + c) : bs) || canMake a ((b * c) : bs)
--sadly is slower: canMake a (b:c:bs) = any (\f -> canMake a (f b c : bs)) [(+), (*)]


canMake2 :: Integer -> [Integer] -> Bool
canMake2 a [b] = a == b
canMake2 a (b:c:bs) = canMake2 a ((b * c) : bs) || canMake2 a ((b + c) : bs) || canMake2 a (read (printf "%d%d" b c) : bs)
-- sasly is slightly slower: canMake2 a (b:c:bs) = any (\f -> canMake2 a (f b c : bs)) [(+), (*), \x y -> read (printf "%d%d" x y)]

day07part1 :: [String] -> Integer
day07part1 field = sum $ map fst $ filter (uncurry canMake) parsed
    where parsed = map parse field

day07part2 :: [String] -> Integer
day07part2 field = sum $ map fst $ filter (uncurry canMake2) parsed
    where parsed = map parse field