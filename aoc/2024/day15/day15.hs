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
import qualified Data.HashSet as HS
import Graphics.Gloss
import Util.MazePicture

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

charToPicture :: Float -> Int -> Int -> Char -> Picture
charToPicture size _ _ c = case c of
    '#' -> Color red rect
    '.' -> Blank
    'O' -> Color green rect
    '[' -> translate 0.5 0 $ Color green $ rectangleSolid size (size - 1)
    ']' -> translate (-0.5) 0 $ Color green $ rectangleSolid size (size - 1)
    '@' -> Color white $ circleSolid (size / 2)
    _ -> Blank
    where rect = rectangleSolid (size - 1) (size - 1)

part1_example1 = do
    part1 2028 "aoc/2024/day15/example1.txt" day15part1
part1_example2 = do
    part1 10092 "aoc/2024/day15/example2.txt" day15part1
part1_input = do
    part1 1499739 "aoc/2024/day15/input.txt" day15part1

part2_example1 = do
    part2 1751 "aoc/2024/day15/example1.txt" day15part2

part2_example2 = do
    part2 9021 "aoc/2024/day15/example2.txt" day15part2

part2_input = do
    part2 1522215 "aoc/2024/day15/input.txt" day15part2

day15part1 :: [String] -> Int
day15part1 s = trace (show moves) $ trace (Maze.showMaze (: []) maze HS.empty) $
    gpsSum 'O' $ foldl (\ma mo -> fromMaybe ma $ move ma (head (Maze.findAll ma '@')) mo) maze moves
    where (maze, moves) = parse s


day15part2 :: [String] -> Int
day15part2 s = trace (show moves) $ trace (Maze.showMaze (: []) maze2 HS.empty) $
    gpsSum '[' $ foldl (\ma mo -> fromMaybe ma $ move2 (head (Maze.findAll ma '@')) mo ma) maze2 moves
    where (maze, moves) = parse s
          maze2 = double maze
          -- traceMaze mo x = trace (show mo ++ "\n" ++ Maze.showMaze (: []) x []) x

fix :: Int -> Int -> Picture -> Picture
fix width height = flipY . translate (-(w / 2)) (-(h / 2))
  where w = fromIntegral width
        h = fromIntegral height

part1_input_show = do
    lines <- readLines "aoc/2024/day15/input.txt"
    let (maze, moves) = parse lines in
     let (x, y, picture) = trace (Maze.showMaze (: []) maze HS.empty) $ mazeToPicture 15 maze (charToPicture 15) in
      display
        (InWindow
       "AOC 2024 15 Part 1 Input"     -- window title
       (x, y)     -- window size
       (100, 100))     -- window position
       black         -- background color
       (fix x y picture)       -- picture to display

part1_input_animate = do
    lines <- readLines "aoc/2024/day15/input.txt"
    let (startMaze, moves) = parse lines
        mazes = reverse $ snd $ foldl (\(ma,acc) mo -> let next = fromMaybe ma $ move ma (head (Maze.findAll ma '@')) mo in (next,next:acc)) (startMaze,[startMaze]) moves
        (x, y, _) = mazeToPicture 15 startMaze (charToPicture 15)
        frame time =
            let (_, _, picture) = mazeToPicture 15 maze (charToPicture 15) in fix x y picture
            where maze = mazes !! (round (time * 30) `mod` length mazes)
        in
     animate
      (InWindow
        "AOC 2024 Day 15 Part 1"     -- window title
        (x, y)     -- window size
        (100, 100))     -- window position
      black         -- background color
      frame       -- picture to display

part2_input_animate = do
    lines <- readLines "aoc/2024/day15/input.txt"
    let (startMaze, moves) = parse lines
        doubleMaze = double startMaze
        mazes = reverse $ snd $ foldl (\(ma,acc) mo -> let next = fromMaybe ma $ move2 (head (Maze.findAll ma '@')) mo ma in (next,next:acc)) (doubleMaze,[doubleMaze]) moves
        (x, y, _) = mazeToPicture 15 doubleMaze (charToPicture 15)
        frame time =
            let (_, _, picture) = mazeToPicture 15 maze (charToPicture 15) in fix x y picture
            where
                index =  round (time * 30) `mod` length mazes -- a new index 30 times / sec
                prevIndex = (index - 1 + length mazes) `mod` length mazes
                maze = mazes !! index
                prevMaze = mazes !! prevIndex
        in
     animate
      (InWindow
        "AOC 2024 Day 15 Part 2"     -- window title
        (x, y)     -- window size
        (100, 100))     -- window position
      black         -- background color
      frame       -- picture to display


data World = World {mazes :: [Maze], moves :: [Direction], index :: Int }

next :: World -> World
next (World mazes moves index) = if index == length mazes - 1 then World mazes moves index else World mazes moves (index + 1)


part1_input_simulate = do
    lines <- readLines "aoc/2024/day15/input.txt"
    let (startMaze, moves) = parse lines
        mazes = reverse $ snd $ foldl (\(ma,acc) mo -> let next = fromMaybe ma $ move ma (head (Maze.findAll ma '@')) mo in (next,next:acc)) (startMaze,[startMaze]) moves
        (x, y, _) = mazeToPicture 15 startMaze (charToPicture 15)
        showWorld (World mazes _ index) = let (_, _, picture) = mazeToPicture 15 (mazes !! index) (charToPicture 15) in fix x y picture
     in
     simulate
     (InWindow
        "AOC 2024 Day 15 Part 1"     -- window title
        (x, y)     -- window size
        (100, 100))     -- window position
      black -- background color
      480 -- number of steps per second
      (World mazes moves 0) -- initial World
      showWorld
      (\_ _ w -> next w) -- update World

part2_input_simulate = do
    lines <- readLines "aoc/2024/day15/input.txt"
    let (startMaze, moves) = parse lines
        doubleMaze = double startMaze
        mazes = reverse $ snd $ foldl (\(ma,acc) mo -> let next = fromMaybe ma $ move2 (head (Maze.findAll ma '@')) mo ma in (next,next:acc)) (doubleMaze,[doubleMaze]) moves
        (x, y, _) = mazeToPicture 15 doubleMaze (charToPicture 15)
        showWorld (World mazes _ index) = let (_, _, picture) = mazeToPicture 15 (mazes !! index) (charToPicture 15) in fix x y picture
     in
     simulate
     (InWindow
        "AOC 2024 Day 15 Part 2"     -- window title
        (x, y)     -- window size
        (100, 100))     -- window position
      black -- background color
      (60*10) -- number of steps per second
      (World mazes moves 0) -- initial World
      showWorld
      (\_ _ w -> next w) -- update World


main = part2_input_simulate
