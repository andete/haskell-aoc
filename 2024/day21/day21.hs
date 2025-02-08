{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

import Data.HashMap.Strict qualified as H
import Data.HashSet qualified as HS
import Data.List (elemIndex, nub)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Debug.Trace (trace)
import Util.AStar
import Util.Aoc
import Util.Direction4 qualified as Direction
import Util.Located
import Util.Maze qualified as M

type Code = String

type Maze = M.Maze Char

keypad = splitOn "," "789,456,123, 0A"

directionalKeypad = splitOn "," " ^A,<v>"

pressesForPath :: [Located Char] -> [Char]
pressesForPath p = map (Direction.toChar . uncurry Direction.directionFromLocations) zipped
  where
    p1 = map location $ init p
    p2 = map location $ tail p
    zipped = zip p1 p2

neighbours :: Maze -> (Located Char, Direction.Direction4) -> [(Located Char, Direction.Direction4)]
neighbours maze (loc, dir) = newLocs ++ a
  where
    l = location loc
    loc' = l Direction.+| dir
    a = case M.at maze loc' of
      Just (Located a c) -> [(Located a c, dir) | c /= ' ']
      Nothing -> []
    newDirs = filter (/= dir) Direction.all'
    newLocs = map (\d -> (loc, d)) newDirs

type PressesCache = H.HashMap (Located Char, Char) (Located Char, [Char])

pressesOneCached :: PressesCache -> Maze -> Located Char -> Char -> (PressesCache, Located Char, [Char])
pressesOneCached cache maze loc key =
  case H.lookup (loc, key) cache of
    Just (newLoc, movements) -> (cache, newLoc, movements)
    Nothing ->
      let (newLoc, movements) = pressesOne maze loc key
       in let newCache = H.insert (loc, key) (newLoc, movements) cache
           in (newCache, newLoc, movements)

pressesOne :: Maze -> Located Char -> Char -> (Located Char, [Char])
pressesOne maze loc key =
  if v == key
    then (loc, "A")
    else
      let p = nub $ map fst $ fromJust $ path astar
       in (last p, pressesForPath p ++ "A")
  where
    v = value loc
    loc' = (loc, Direction.West)
    astar =
      AStar
        { getStart = loc',
          getGoal = \(l, _) -> value l == key,
          getCost = \(l, _) (l2, _) -> if l == loc && l2 == loc then 1 else 10,
          getNeighbours = neighbours maze,
          getHeuristic = const 0
        }

presses :: Maze -> Located Char -> Code -> [Char]
presses maze loc code =
  snd $
    foldl
      ( \(l, movements) key ->
          let (newLoc, newMovements) = pressesOne maze l key
           in (newLoc, movements ++ newMovements)
      )
      (loc, "")
      code

pressesCached :: PressesCache -> Maze -> Located Char -> Code -> (PressesCache, [Char])
pressesCached cache maze loc code = (newCache, movements)
  where
    (newCache, _, movements) = foldl (\(oldCache, located, movements) key -> let (newCache, newLoc, newMovements) = pressesOneCached oldCache maze located key in (newCache, newLoc, movements ++ newMovements)) (cache, loc, "") code

day20part1sub :: Int -> [Code] -> String
day20part1sub step input = [keyPadPresses, directionalPresses, directionalPresses2] !! step
  where
    keyPadMaze = M.parse id keypad
    keyPadStart = head $ M.findAll keyPadMaze 'A'
    keyPadPresses = presses keyPadMaze keyPadStart (head input)
    directionalMaze = M.parse id directionalKeypad
    directionalStart = head $ M.findAll directionalMaze 'A'
    directionalPresses = presses directionalMaze directionalStart keyPadPresses
    directionalPresses2 = presses directionalMaze directionalStart directionalPresses

findSequence :: Code -> String
findSequence input = directionalPresses2
  where
    keyPadMaze = M.parse id keypad
    keyPadStart = head $ M.findAll keyPadMaze 'A'
    keyPadPresses = presses keyPadMaze keyPadStart input
    directionalMaze = M.parse id directionalKeypad
    directionalStart = head $ M.findAll directionalMaze 'A'
    directionalPresses = presses directionalMaze directionalStart keyPadPresses
    directionalPresses2 = presses directionalMaze directionalStart directionalPresses

findSequence2 :: Int -> Code -> String
findSequence2 times input = directionalPresses2
  where
    keyPadMaze = M.parse id keypad
    keyPadStart = head $ M.findAll keyPadMaze 'A'
    keyPadPresses = presses keyPadMaze keyPadStart input
    directionalMaze = M.parse id directionalKeypad
    directionalStart = head $ M.findAll directionalMaze 'A'
    directionalPresses = presses directionalMaze directionalStart keyPadPresses
    directionalPresses2 =
      iterate
        (presses directionalMaze directionalStart)
        directionalPresses
        !! times

findSequence2Cached :: Maze -> Located Char -> Maze -> Located Char -> PressesCache -> Int -> Code -> (PressesCache, String)
findSequence2Cached keyPadMaze keyPadStart directionalMaze directionalStart cache times input = directionalPresses2
  where
    keyPadPresses = presses keyPadMaze keyPadStart input
    directionalPresses = presses directionalMaze directionalStart keyPadPresses
    directionalPresses2 =
      iterate
        (\(oldCache, p) -> pressesCached oldCache directionalMaze directionalStart p)
        (cache, directionalPresses)
        !! times

day20part1raw :: [Code] -> [(Int, Int)]
day20part1raw input = zip lengths numeric
  where
    sequences = map findSequence input
    lengths = map length sequences
    numeric = map (\x -> read (init x) :: Int) input

day20part2raw :: Int -> [Code] -> [(Int, Int)]
day20part2raw times input = zip lengths numeric
  where
    sequences = map (findSequence2 times) input
    lengths = map length sequences
    numeric = map (\x -> read (init x) :: Int) input

day20part2rawCached :: Int -> [Code] -> [(Int, Int)]
day20part2rawCached times input = zip lengths numeric
  where
    keyPadMaze = M.parse id keypad
    keyPadStart = head $ M.findAll keyPadMaze 'A'
    directionalMaze = M.parse id directionalKeypad
    directionalStart = head $ M.findAll directionalMaze 'A'
    cache = H.empty
    sequences = reverse $ snd $ foldl (\(c, acc) i -> let (newC, f) = findSequence2Cached keyPadMaze keyPadStart directionalMaze directionalStart c times i in (newC, f : acc)) (cache, []) input
    lengths = map length sequences
    numeric = map (\x -> read (init x) :: Int) input

day21part1 :: [Code] -> Int
day21part1 input = sum $ map (uncurry (*)) $ day20part1raw input

day21part2 :: Int -> [Code] -> Integer
day21part2 times input = sum $ map (\(a, b) -> toInteger a * toInteger b) $ day20part2rawCached times input

part1_example_robot1 = do
  part1 "<A^A>^^AvvvA" "2024/day21/example.txt" (day20part1sub 0)

part1_example_robot2 = do
  part1 "v<<A>>^A<A>AvA<^AA>A<vAAA>^A" "2024/day21/example.txt" (day20part1sub 1)

part1_example_user = do
  part1 "<vA<AA>>^AvAA<^A>Av<<A>>^AvA^A<vA>^Av<<A>^A>AAvA^Av<<A>A>^AAAvA<^A>A" "2024/day21/example.txt" (day20part1sub 2)

part1_example_raw = do
  part1 [(68, 29), (60, 980), (68, 179), (64, 456), (64, 379)] "2024/day21/example.txt" day20part1raw

part1_example = do
  part1 126384 "2024/day21/example.txt" day21part1

part1_input = do
  part1 248108 "2024/day21/input.txt" day21part1

part2_example_raw = do
  part2 [(68, 29), (60, 980), (68, 179), (64, 456), (64, 379)] "2024/day21/example.txt" (day20part2raw 1)

part2_example_raw_2 = do
  part2 [(68, 29), (60, 980), (68, 179), (64, 456), (64, 379)] "2024/day21/example.txt" (day20part2raw 2)

part2_example_raw_cached = do
  part2 [(68, 29), (60, 980), (68, 179), (64, 456), (64, 379)] "2024/day21/example.txt" (day20part2rawCached 1)

part2_example = do
  part2 126384 "2024/day21/example.txt" (day21part2 1)

part2_input_low = do
  part2 248108 "2024/day21/input.txt" (day21part2 1)

part2_input = do
  part2 248108 "2024/day21/input.txt" (day21part2 8)