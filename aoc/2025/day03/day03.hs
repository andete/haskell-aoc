import System.IO
import Data.List (sort)
import Control.Exception (assert)
import Util.Aoc
import Debug.Trace (trace)
import Data.List.Split (splitOn)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

part1_example = do
    part1 357 "2025/day03/example.txt" day03part1

part1_input = do
    part1 17263 "2025/day03/input.txt" day03part1

part2_example = do
    part2 3121910778619 "2025/day03/example.txt" day03part2

part2_input = do
    part2 170731717900423 "2025/day03/input.txt" day03part2

largestJoltage :: String -> Integer
largestJoltage s = read ([firstLargest, secondLargest])
  where allExceptLast = take (length s - 1) s
        firstLargest = maximum allExceptLast
        pos = fromJust $ elemIndex firstLargest s
        secondLargest = maximum $ drop (pos + 1) s

largestJoltage2 :: Integer -> Int -> String -> Integer
largestJoltage2 acc 0 s = acc
largestJoltage2 acc n s = largestJoltage2 (acc * 10 + firstLargestInteger) (n - 1) rem
  where allExceptLast = take (length s - n + 1) s
        firstLargest = maximum allExceptLast
        pos = fromJust $ elemIndex firstLargest s
        rem = drop (pos + 1) s
        firstLargestInteger = read [firstLargest] :: Integer

day03part1 :: [String] -> Integer
day03part1 xn = sum $ map largestJoltage xn


day03part2 :: [String] -> Integer
day03part2 xn = sum $ map (largestJoltage2 0 12) xn