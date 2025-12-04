import System.IO
import Data.List (sort)
import Control.Exception (assert)
import Util.Aoc
import Debug.Trace (trace)
import Data.List.Split (splitOn)

part1_example = do
    part1 1227775554 "2025/day02/example.txt" day01part1

part1_input = do
    part1 18893502033 "2025/day02/input.txt" day01part1

part2_example = do
    part2 4174379265 "2025/day02/example.txt" day01part2

part2_input = do
    part2 26202168557 "2025/day02/input.txt" day01part2


range :: String -> [Integer]
range s = [a..b] where [a, b] = splitOn "-" s >>= \x -> [read x :: Integer]

invalid :: Integer -> Bool
invalid x = firstHalf == secondHalf
    where s = show x
          half = length s `div` 2
          firstHalf = take half s
          secondHalf = drop half s

okay :: String -> Int -> Bool
okay s n = s == composed
  where section = take n s
        repeatCount = (length s) `div` n
        composed = concat $ replicate repeatCount section

invalid2 :: Integer -> Bool
invalid2 x = any (okay s) options
    where s = show x
          halve = length s `div` 2
          options = [1..halve]

day01part1 :: [String] -> Integer
day01part1 xn = sum invalids
   where ranges = map range $ splitOn "," (head xn)
         invalids = concat $ map (\r -> filter invalid r) ranges


day01part2 :: [String] -> Integer
day01part2 xn = sum invalids
   where ranges = map range $ splitOn "," (head xn)
         invalids = concat $ map (\r -> filter invalid2 r) ranges