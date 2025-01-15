import Aoc
import qualified Maze
import Debug.Trace (trace)
import Location
part1_example = do
    part1 1 "day10/example.txt" day10part1

part1_example2 = do
    part1 36 "day10/example2.txt" day10part1

score :: Maze.Maze Int -> Location -> Int
score maze beachHead = 0

day10part1 :: [String] -> Int
day10part1 field = trace (show beachHeads) $ trace (Maze.showMaze maze []) $
    sum $ map (score maze) beachHeads
    where maze = Maze.parse (\x -> read [x] :: Int) field
          beachHeads = Maze.findAll maze 0