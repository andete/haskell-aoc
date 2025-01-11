import Text.Printf (printf)
import Aoc
import Debug.Trace (trace)
import qualified CharMaze

part1_example = do
    part1 143 "day06/example.txt" day06part1

day06part1 :: [String] -> Integer
day06part1 field = trace (CharMaze.show maze []) 0
    where maze = CharMaze.parse field