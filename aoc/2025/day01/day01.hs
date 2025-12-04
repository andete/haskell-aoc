import System.IO
import Data.List (sort)
import Control.Exception (assert)
import Util.Aoc
import Debug.Trace (trace)

part1_example = do
    part1 3 "2025/day01/example.txt" day01part1

part1_input = do
    part1 1011 "2025/day01/input.txt" day01part1

part2_example = do
    part2 6 "2025/day01/example.txt" day01part2

part2_input = do
    part2 5937 "2025/day01/input.txt" day01part2

dial :: (Integer, Integer) -> String -> (Integer, Integer)
dial (input, count) action = (new, newCount)
    where direction = head action
          step = read (tail action) :: Integer
          new = case direction of
            'R' -> (input + step) `mod` 100
            'L' -> (input - step) `mod` 100
          newCount = if new == 0 then count + 1 else count

dial2 :: (Integer, Integer) -> String -> (Integer, Integer)
dial2 (input, count) action = (newMod, count + newZeros)
    where direction = head action
          step = read (tail action) :: Integer
          (new, opString) = case direction of
            'R' -> (input + step, "+")
            'L' -> (input - step, "-")
          newMod = new `mod` 100
          modZero x = x `mod` 100 == 0
          range = if new > input then [(input+1)..new] else [new..(input-1)]
          zeros = filter modZero range
          newZeros = toInteger $ length zeros

day01part1 :: [String] -> Integer
day01part1 = snd . foldl dial (50, 0)


day01part2 :: [String] -> Integer
day01part2 = snd . foldl dial2 (50, 0)
