import System.IO
import Data.List (sort)
import Control.Exception (assert)
import Util.Aoc

part1_example = do
    part1 11 "2024/day01/example.txt" day01part1

part1_example' :: IO ()
part1_example' = do
    part1 11 "2024/day01/example.txt" day01part1'

part1_input = do
    part1 2057374 "2024/day01/input.txt" day01part1'

part2_example = do
    part2 31 "2024/day01/example.txt" day01part2

part2_input = do
    part2 23177084 "2024/day01/input.txt" day01part2

day01part1 :: [String] -> Integer
day01part1 xs = sum $ zipWith (\x y -> abs (x - y)) (sort $ map (head . listToNums . words) xs) (sort $ map (last . listToNums . words) xs)


day01part1' :: [String] -> Integer
day01part1' xs = sum $ zipWith (\x y -> abs (x - y)) (convert head) (convert last)
    where convert taker = sort $ map (taker . listToNums . words) xs


day01part2 :: [String] -> Int
day01part2 xs = sum $ map (\x -> x * length (filter (== x) lasts)) firsts
    where convert taker = sort $ map (taker . listToNums . words) xs
          firsts = convert head
          lasts = convert last