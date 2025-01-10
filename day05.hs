
import Data.List.Split (splitOn)
import Aoc
import Debug.Trace
import Data.List (elemIndex)
import Text.Printf (printf)

parse :: [String] -> ([(Integer, Integer)], [[Integer]])
parse lines =
    let rules = map (\l -> let xn = splitOn "|" l in (read $ head xn, read $ xn !! 1)) $ filter (elem '|') lines
        updates = map (map read . splitOn ",") $ filter (elem ',') lines in
        (rules, updates)

ruleValid :: (Integer, Integer) -> [Integer] -> Bool
ruleValid (x, y) update
    | x `notElem` update = True
    | y `notElem` update = True
    | otherwise = elemIndex x update < elemIndex y update

rulesValid rules update = all (`ruleValid` update) rules

middle update = update !! (length update `div` 2)

part1_example = do
    part1 143 "day05/example.txt" day05part1

part1_input = do
    part1 7074 "day05/input.txt" day05part1

day05part1 :: [String] -> Integer
day05part1 field = let (rules, updates) = parse field in
    sum $ map middle (filter (rulesValid rules) updates)