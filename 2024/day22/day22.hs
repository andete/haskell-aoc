import Util.Aoc
import Data.Bits (xor)

next :: Integer -> Integer
next x = s3 
    where s1 = ((x * 64) `xor` x) `mod` 16777216
          s2 = (s1 `div` 32 `xor` s1) `mod` 16777216
          s3 = (s2 * 2048 `xor` s2) `mod` 16777216

part1_example = do
  part1 37327623 "2024/day22/example.txt" day22part1

part1_input = do
  part1 13234715490 "2024/day22/input.txt" day22part1

part1_test :: Integer
part1_test = iterate next 123 !! 10

day22part1 :: [String] -> Integer
day22part1 input = sum $ map (\x -> iterate next x !! 2000) initial
    where initial = map read input