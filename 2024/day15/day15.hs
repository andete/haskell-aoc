import qualified Util.Maze as Maze
import Data.List (elemIndex)
import Data.Maybe (fromJust, fromMaybe)
import qualified Util.Direction4 as Direction
import Util.Aoc
import Debug.Trace (trace)
import qualified Util.Location as Location
import qualified Util.Located as Located
import qualified Util.Maze as Located
import qualified Data.Vector as V
import Util.Direction4 ((+|))

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
    '.' -> moveMazeItem item targetLocation itemLocation maze
    'O' -> move maze targetLocated direction >>= moveMazeItem item targetLocation itemLocation
    _ -> error "Unknown item"
    where itemLocation = Located.location itemLocated
          item = Located.value itemLocated
          targetLocated = fromJust $ Maze.at maze $ itemLocation +| direction
          targetItem = Located.value targetLocated
          targetLocation = Located.location targetLocated



move2 :: Located -> Direction -> Maze -> Maybe Maze
move2 itemLocated direction maze
   | targetItem == '#' = Nothing
   | targetItem == '.' = moveMazeItem item targetLocation itemLocation maze
   | (targetItem == '[' || targetItem == ']') && eastWest =
        move2 targetLocated direction maze >>= moveMazeItem item targetLocation itemLocation
   | targetItem == '[' && northSouth =
        move2 targetLocated direction maze >>= move2 targetEast direction >>= moveMazeItem item targetLocation itemLocation
   | targetItem == ']' && northSouth =
        move2 targetLocated direction maze >>= move2 targetWest direction >>= moveMazeItem item targetLocation itemLocation
   | otherwise = error "Unknown item"
    where itemLocation = Located.location itemLocated
          item = Located.value itemLocated
          targetLocated = fromJust $ Maze.at maze $ itemLocation +| direction
          targetItem = Located.value targetLocated
          targetLocation = Located.location targetLocated
          eastWest = direction == Direction.East || direction == Direction.West
          northSouth = direction == Direction.North || direction == Direction.South
          targetWest = fromJust $ Maze.at maze $ targetLocation +| Direction.West
          targetEast = fromJust $ Maze.at maze $ targetLocation +| Direction.East

gpsSum :: Char -> Maze -> Int
gpsSum c maze = sum $ map ((\(Location.Location x y) -> x + y * 100) . Located.location) (Maze.findAll maze c)

double :: Maze -> Maze
double (Maze.Maze vnn) = Maze.Maze $ V.map mapRow vnn
    where mapRow = V.concatMap (\(Located.Located loc c) -> doubleOne loc c)
          doubleOne loc c = V.fromList [Located.Located loc1 c1, Located.Located loc2 c2]
            where Location.Location x y = loc
                  loc1 = Location.Location (2 * x) y
                  loc2 = Location.Location (2 * x + 1) y
                  [c1,c2] = case c of
                      'O' -> "[]"
                      '#' -> "##"
                      '.' -> ".."
                      '@' -> "@."

part1_example1 = do
    part1 2028 "2024/day15/example1.txt" day15part1
part1_example2 = do
    part1 10092 "2024/day15/example2.txt" day15part1
part1_input = do
    part1 1499739 "2024/day15/input.txt" day15part1

part2_example1 = do
    part2 1751 "2024/day15/example1.txt" day15part2

part2_example2 = do
    part2 9021 "2024/day15/example2.txt" day15part2

part2_input = do
    part2 1522215 "2024/day15/input.txt" day15part2

day15part1 :: [String] -> Int
day15part1 s = trace (show moves) $ trace (Maze.showMaze (: []) maze []) $
    gpsSum 'O' $ foldl (\ma mo -> fromMaybe ma $ move ma (head (Maze.findAll ma '@')) mo) maze moves
    where (maze, moves) = parse s


day15part2 :: [String] -> Int
day15part2 s = trace (show moves) $ trace (Maze.showMaze (: []) maze2 []) $
    gpsSum '[' $ foldl (\ma mo -> fromMaybe ma $ move2 (head (Maze.findAll ma '@')) mo ma) maze2 moves
    where (maze, moves) = parse s
          maze2 = double maze
          -- traceMaze mo x = trace (show mo ++ "\n" ++ Maze.showMaze (: []) x []) x
