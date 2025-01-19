import qualified Util.Maze as Maze
import Data.List (elemIndex)
import Data.Maybe (fromJust, fromMaybe)
import qualified Util.Direction4 as Direction
import Util.Aoc
import Debug.Trace (trace)
import qualified Util.Location as Location
import qualified Util.Located as Located
import qualified Util.Maze as Located

type Direction = Direction.Direction4
type Maze = Maze.Maze Char
type Located = Located.Located Char
type Location = Location.Location

parse :: [String] -> (Maze, [Direction])
parse s = (maze, moves)
    where pos = fromJust $ elemIndex "" s
          mazeLines = take pos s
          maze = Maze.parse id mazeLines
          moves = map Direction.fromChar $ concat $ drop (pos + 1) s

moveMazeItem :: Char -> Location -> Location -> Maze -> Maybe Maze
moveMazeItem item targetLocation sourceLocation maze = Just $ Maze.set item targetLocation $ Maze.set '.' sourceLocation maze

move :: Maze -> Located -> Direction -> Maybe Maze
move maze itemLocated direction = case targetItem of
    '#' -> Nothing
    '.' -> Just $ Maze.set item targetLocation $ Maze.set '.' itemLocation maze
    'O' -> move maze targetLocated direction >>= moveMazeItem item targetLocation itemLocation
    _ -> error "Unknown item"
    where itemLocation = Located.location itemLocated
          item = Located.value itemLocated
          targetLocated = fromJust $ Maze.at maze $ itemLocation Direction.+| direction
          targetItem = Located.value targetLocated
          targetLocation = Located.location targetLocated


gpsSum :: Maze -> Int
gpsSum maze = sum $ map ((\(Location.Location x y) -> x + y * 100) . Located.location) (Maze.findAll maze 'O')

part1_example1 = do
    part1 2028 "2024/day15/example1.txt" day15part1
part1_example2 = do
    part1 10092 "2024/day15/example2.txt" day15part1
part1_input = do
    part1 1499739 "2024/day15/input.txt" day15part1

day15part1 :: [String] -> Int
day15part1 s = trace (show moves) $ trace (Maze.showMaze (: []) maze []) $
    gpsSum $ foldl (\ma mo -> fromMaybe ma $ move ma (head (Maze.findAll ma '@')) mo) maze moves
    where (maze, moves) = parse s
