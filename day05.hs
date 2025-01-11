
import Data.List.Split (splitOn)
import Aoc
import Debug.Trace
import Data.List (elemIndex)
import Text.Printf (printf)
import Control.Monad.Fix (fix)
import Data.Foldable (Foldable(fold))

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

fixRule :: [Integer] -> (Integer, Integer)  -> [Integer]
fixRule update (x, y) = let ix = elemIndex x update in
    let iy = elemIndex y update in
        case (ix, iy) of
            (Just ix, Just iy) | ix > iy -> let (a, b) = splitAt iy update in
                trace (show (x,y, a,b)) $ let (c,d) = splitAt (ix - length a - 1) (tail b) in
                    trace (show (x,y, c,d)) $ a ++ [x] ++ c ++ [y] ++ tail d
            _ -> update

fixAll :: [(Integer, Integer)] -> [Integer] -> [Integer]
fixAll rules update = foldl fixRule update rules

part1_example :: IO ()
part1_example = do
    part1 143 "day05/example.txt" day05part1

part1_input = do
    part1 7074 "day05/input.txt" day05part1

part2_example = do
    part2 123 "day05/example.txt" day05part2

part2_input = do
    part2 4828 "day05/input.txt" day05part2

day05part1 :: [String] -> Integer
day05part1 field = let (rules, updates) = parse field in
    sum $ map middle (filter (rulesValid rules) updates)

day05part2 :: [String] -> Integer
day05part2 field = let (rules, updates) = parse field in
    let invalid = filter (not . rulesValid rules) updates in
        let f = fixAll rules in
        let fixed = map (f. f .f .f) invalid in
            sum $ map middle fixed
