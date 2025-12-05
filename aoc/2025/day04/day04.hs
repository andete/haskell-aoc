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
    part1 13 "2025/day04/example.txt" day04part1

part1_input = do
    part1 1537 "2025/day04/input.txt" day04part1

part2_example = do
    part2 43 "2025/day04/example.txt" day04part2

part2_input = do
    part2 8707 "2025/day04/input.txt" day04part2

day04part1 :: [String] -> Integer
day04part1 xn = trace (Maze.showMaze (:[]) maze HS.empty) (toInteger $ length accessibles)
  where maze = Maze.parse id xn
        rollLocations = Maze.findAll maze '@'
        accessibles = filter accessible rollLocations
        accessible (Maze.Located loc _) = (length $ filter (\(Maze.Located _ c)  -> c =='@') $ Maze.neighbours8 maze loc) < 4

reduceRolls :: Integer -> Maze.Maze Char -> (Integer, Maze.Maze Char)
reduceRolls count maze = (newCount, newMaze)
    where rollLocations = Maze.findAll maze '@'
          accessibles = filter accessible rollLocations
          accessible (Maze.Located loc _) = (length $ filter (\(Maze.Located _ c)  -> c =='@') $ Maze.neighbours8 maze loc) < 4
          numAccessible = toInteger (length accessibles)
          (newCount, newMaze) = if numAccessible == 0 then (count, maze)
           else reduceRolls (count + numAccessible) (foldl (\m (Maze.Located loc _) -> Maze.set '.' loc m) maze accessibles)

day04part2 :: [String] -> Integer
day04part2 xn = fst $ reduceRolls 0 maze
    where maze = Maze.parse id xn
