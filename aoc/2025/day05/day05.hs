import System.IO
import Data.List (sort)
import Control.Exception (assert)
import Util.Aoc
import Debug.Trace (trace)
import Data.List.Split (splitOn)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Util.Maze as Maze
import qualified Data.HashSet as HS

part1_example = do
    part1 3 "2025/day05/example.txt" day05part1

part1_input = do
    part1 1537 "2025/day05/input.txt" day05part1

part2_example = do
    part2 14 "2025/day05/example.txt" day05part2

part2_input = do
    part2 357674099117260 "2025/day05/input.txt" day05part2

day05part1 :: [String] -> Integer
day05part1 xn = toInteger $ length freshIngredients
    where ranges = filter (\line -> length line > 0 && '-' `elem` line) xn
          ingredients :: [Integer] = map read $ filter (\line -> length line > 0 && not ('-' `elem` line)) xn
          ranges2 = map (\[a,b] -> (read a :: Integer, read b :: Integer)) $ map (splitOn "-") ranges
          freshIngredients = filter (\ing -> any (\(r1, r2) -> ing >= r1 && ing <= r2) ranges2) ingredients

mergeRanges :: [(Integer, Integer)] -> [(Integer, Integer)]
mergeRanges [] = []
mergeRanges (r:[]) = [r]
mergeRanges ((r1,r2):(s1,s2):t) = if (s1 <= r2)
    then mergeRanges ((r1, max r2 s2):t)
    else (r1,r2) : mergeRanges ((s1,s2):t)

day05part2 :: [String] -> Integer
day05part2 xn = totalCovered
  where ranges = filter (\line -> length line > 0 && '-' `elem` line) xn
        ranges2 = map (\[a,b] -> (read a :: Integer, read b :: Integer)) $ map (splitOn "-") ranges
        rangesSorted = sort ranges2
        mergedRanges = mergeRanges rangesSorted
        totalCovered = sum $ map (\(a,b) -> b - a + 1) mergedRanges