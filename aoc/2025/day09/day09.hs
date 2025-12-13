import System.IO
import Data.List (sort, sortBy)
import Control.Exception (assert)
import Util.Aoc
import Debug.Trace (trace)
import Data.List.Split (splitOn)
import Data.List (elemIndex, nub)
import Data.Maybe (fromJust)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as H
import qualified Util.Location as L

combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 1 xn = map (:[]) xn
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

part1_example = do
    part1 50 "2025/day09/example.txt" day09part1

part1_input = do
    part1 4752484112 "2025/day09/input.txt" day09part1

part2_example = do
    part2 25272 "2025/day09/example.txt" day09part2

part2_input = do
    part2 221371496188107 "2025/day09/input.txt" day09part2

day09part1 :: [String] -> Integer
day09part1 xn = trace (show biggest) $ result
  where locations = map (L.fromList . map read . splitOn ",") xn :: [L.Location]
        combinationLocs = combinations 2 locations
        sorted = sortBy (\[a,b] [c,d] -> compare (L.hammingDistance c d) (L.hammingDistance a b)) combinationLocs
        biggest = head sorted
        result = ((toInteger . abs) (L.x (biggest !! 0) - L.x (biggest !! 1)) + 1)
             * ((toInteger . abs) (L.y (biggest !! 0) - L.y (biggest !! 1)) + 1)

day09part2 :: [String] -> Integer
day09part2 xn = 42