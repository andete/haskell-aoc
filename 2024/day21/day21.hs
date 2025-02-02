{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
import Data.List.Split (splitOn)
import qualified Util.Maze as M
import Util.Aoc
import Debug.Trace (trace)
import qualified Data.HashSet as HS
import Util.AStar
import Util.Located
import qualified Util.Direction4 as Direction
import Data.Maybe (fromJust, mapMaybe, catMaybes)
import Data.List (elemIndex, nub)

type Code = String
type Maze = M.Maze Char

keypad = splitOn "," "789,456,123, 0A"

directionalKeypad = splitOn "," " ^A,<v>"

pressesForPath :: [Located Char] -> [Char]
pressesForPath p = map (Direction.toChar . uncurry Direction.directionFromLocations) zipped
    where p1 = map location $ init p
          p2 = map location $ tail p
          zipped = zip p1 p2

neighbours :: Maze -> (Located Char, Direction.Direction4) -> [(Located Char, Direction.Direction4)]
neighbours maze (loc, dir) = newLocs ++ a
    where l = location loc
          loc' = l Direction.+| dir
          a = case M.at maze loc' of
              Just (Located a c) -> [(Located a c, dir) | c /= ' ']
              Nothing -> []
          newDirs = filter (/= dir) Direction.all'
          newLocs = map (\d -> (loc, d)) newDirs

pressesOne :: Maze -> Located Char -> Char -> (Located Char, [Char])
pressesOne maze loc key =
    if v == key then (loc, "A") else
        let p = nub $ map fst $ fromJust $ path astar in
            (last p, pressesForPath p ++ "A")
    where v = value loc
          loc' = (loc, Direction.West)
          astar = AStar {
              getStart = loc',
              getGoal = \(l,_) -> value l == key,
              getCost = \(l,_) (l2,_) -> if l == loc && l2 == loc then 1 else 10,
              getNeighbours = neighbours maze,
              getHeuristic = const 0
          }

presses :: Maze -> Located Char -> Code -> (Located Char, [Char])
presses maze loc = foldl (\(l, movements) key ->
    let (newLoc, newMovements) = pressesOne maze l key in
        (newLoc, movements ++ newMovements) ) (loc, "")


day20part1sub :: Int -> [Code] -> String
day20part1sub step input = [keyPadPresses, directionalPresses, directionalPresses2] !! step
    where keyPadMaze = M.parse id keypad
          keyPadStart = head $ M.findAll keyPadMaze 'A'
          (newLoc, keyPadPresses) = presses keyPadMaze keyPadStart (head input)
          directionalMaze = M.parse id directionalKeypad
          directionalStart = head $ M.findAll directionalMaze 'A'
          (newLoc2, directionalPresses) = presses directionalMaze directionalStart keyPadPresses
          (newLoc3, directionalPresses2) = presses directionalMaze directionalStart directionalPresses

findSequence :: Code -> String
findSequence input = directionalPresses2
    where keyPadMaze = M.parse id keypad
          keyPadStart = head $ M.findAll keyPadMaze 'A'
          (newLoc, keyPadPresses) = presses keyPadMaze keyPadStart input
          directionalMaze = M.parse id directionalKeypad
          directionalStart = head $ M.findAll directionalMaze 'A'
          (newLoc2, directionalPresses) = presses directionalMaze directionalStart keyPadPresses
          (newLoc3, directionalPresses2) = presses directionalMaze directionalStart directionalPresses

day20part1raw :: [Code] -> [(Int, Int)]
day20part1raw input = zip lengths numeric
    where sequences = map findSequence input
          lengths = map length sequences
          numeric = map (\x -> read (init x) :: Int) input

day20part1 :: [Code] -> Int
day20part1 input = sum $ map (uncurry (*)) $ day20part1raw input

part1_example_robot1 = do
    part1 "<A^A>^^AvvvA" "2024/day21/example.txt" (day20part1sub 0)

part1_example_robot2 = do
    part1 "v<<A>>^A<A>AvA<^AA>A<vAAA>^A" "2024/day21/example.txt" (day20part1sub 1)

part1_example_user = do
    part1 "<vA<AA>>^AvAA<^A>Av<<A>>^AvA^A<vA>^Av<<A>^A>AAvA^Av<<A>A>^AAAvA<^A>A" "2024/day21/example.txt" (day20part1sub 2)

part1_example_raw = do
    part1 [(68,29),(60,980),(68,179),(64,456),(64,379)]  "2024/day21/example.txt" day20part1raw


part1_example = do
    part1 126384  "2024/day21/example.txt" day20part1

part1_input = do
    part1 126384  "2024/day21/input.txt" day20part1