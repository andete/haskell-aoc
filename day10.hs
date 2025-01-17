import Aoc
import qualified Maze
import Debug.Trace (trace)
import Location
import Data.List (nub)
import Located

part1_example = do
    part1 1 "day10/example.txt" day10part1

part1_example2 = do
    part1 36 "day10/example2.txt" day10part1

part1_input = do
    part1 496 "day10/input.txt" day10part1

part2_example2 = do
    part2 81 "day10/example2.txt" day10part2

part2_input = do
    part2 1120 "day10/input.txt" day10part2

summits :: Maze.Maze Int -> Located Int -> [Located Int]
summits maze loc 
    | i == 9 = [loc]
    | otherwise = concatMap (summits maze) neighbours
    where Located l i = loc
          neighbours = filter (\l -> Located.value l == i + 1) $ Maze.neighbours maze l
          
score :: Maze.Maze Int -> Located Int -> Int
score maze loc = length $ nub $ summits maze loc

rating :: Maze.Maze Int -> Located Int -> Int
rating maze loc 
    | i == 9 = 1
    | otherwise = sum $ map (rating maze) neighbours
    where Located l i = loc
          neighbours = filter (\l -> Located.value l == i + 1) $ Maze.neighbours maze l

day10part1 :: [String] -> Int
day10part1 field = trace (show beachHeads) $ trace (Maze.showMaze show maze []) $
    sum $ map (score maze) beachHeads
    where maze = Maze.parse (\x -> read [x] :: Int) field
          beachHeads = Maze.findAll maze 0

day10part2 :: [String] -> Int
day10part2 field = trace (show beachHeads) $ trace (Maze.showMaze show maze []) $
    sum $ map (rating maze) beachHeads
    where maze = Maze.parse (\x -> read [x] :: Int) field
          beachHeads = Maze.findAll maze 0