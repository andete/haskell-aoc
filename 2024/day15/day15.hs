import qualified Util.Maze as Maze
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Util.Direction4 as Direction
import Util.Aoc
import Debug.Trace (trace)

type Direction = Direction.Direction4
type Maze = Maze.Maze Char
charToDirection :: Char -> Direction
charToDirection '^' = Direction.North
charToDirection '>' = Direction.East
charToDirection 'v' = Direction.South
charToDirection '<' = Direction.West

parse :: [String] -> (Maze, [Direction])
parse s = (maze, moves)
    where pos = fromJust $ elemIndex "" s
          mazeLines = take pos s
          maze = Maze.parse id mazeLines
          moves = map charToDirection $ concat $ drop (pos + 1) s

part1_example1 = do
    part1 12 "2024/day15/example1.txt" day15part1
part1_example2 = do
    part1 12 "2024/day15/example2.txt" day15part1
part1_example3 = do
    part1 12 "2024/day15/example3.txt" day15part1
part1_input = do
    part1 12 "2024/day15/input.txt" day15part1

day15part1 :: [String] -> Integer
day15part1 s = trace (show moves) $ trace (Maze.showMaze (: []) maze []) 0
    where (maze, moves) = parse s