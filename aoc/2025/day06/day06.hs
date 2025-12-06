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
    part1 4277556 "2025/day06/example.txt" day06part1

part1_input = do
    part1 4277556 "2025/day06/input.txt" day06part1

part2_example = do
    part2 3263827 "2025/day06/example.txt" day06part2

part2_input = do
    part2 9630000828442 "2025/day06/input.txt" day06part2

splitOnSpaces :: String -> [String]
splitOnSpaces s = filter (/="") (splitOn " " s)

day06part1 :: [String] -> Integer
day06part1 xn = sum results
  where operators = splitOnSpaces (xn !! (length xn - 1))
        others = map splitOnSpaces $ take (length xn - 1) xn
        othersNum = map (map read) others :: [[Integer]]
        size = length operators
        operator x = if (x == "*") then \a b -> a * b else \a b -> a + b
        operatorAt i = operators !! i
        operatorFunAt = operator . operatorAt
        operatorStartAt i = if (operatorAt i == "*") then 1 else 0
        resultAtColumn i = foldl (\acc row -> (operatorFunAt i) acc (row !! i)) (operatorStartAt i) othersNum
        results = map resultAtColumn [0..(size - 1)]

day06part2 :: [String] -> Integer
day06part2 xn = sum results
  where operators = splitOnSpaces (xn !! (length xn - 1))
        size = length operators
        operator x = if (x == "*") then \a b -> a * b else \a b -> a + b
        operatorAt i = operators !! i
        operatorFunAt = operator . operatorAt
        others = take (length xn - 1) xn
        maxColumns = maximum $ map length others
        columnIndices = [0..(maxColumns - 1)]
        columns = map (\i -> map (\row -> (row !! i)) others) columnIndices
        strippedColumns = map (filter (/=' ')) columns
        groups = map (map read) $ splitOn [""] strippedColumns :: [[Integer]]
        operatorStartAt i = if (operatorAt i == "*") then 1 else 0
        resultAtGroup i = foldl (operatorFunAt i) (operatorStartAt i) (groups !! i)
        results = map resultAtGroup [0..(size - 1)]
            
        
