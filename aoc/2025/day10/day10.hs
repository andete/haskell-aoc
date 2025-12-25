import Util.AStar (AStar(..), path)
import qualified Data.HashSet as HS
import Data.List (intersperse)

import System.IO
import Data.List (sort)
import Control.Exception (assert)
import Util.Aoc
import Debug.Trace (trace)
import Data.List.Split (splitOn)
import Data.List (elemIndex, nub)
import Data.Maybe (fromJust)

import Data.Hashable (Hashable, hashWithSalt)

part1_example = do
    part1 7 "2025/day10/example.txt" day10part1

part1_input = do
    part1 524 "2025/day10/input.txt" day10part1

part2_example = do
    part2 40 "2025/day10/example.txt" day10part2

part2_input = do
    part2 221371496188107 "2025/day10/input.txt" day10part2

data Indicators = Indicators [Bool] deriving (Eq, Ord)
data Button = Button [Int] deriving (Eq, Ord)

data Machine = Machine Indicators [Button] deriving (Eq, Ord)

instance Show Indicators where
    show (Indicators inds) = "[" ++ concatMap (\b -> if b then "#" else ".") inds ++ "]"

instance Show Button where
    show (Button nums) = "(" ++ concat (intersperse "," (map show nums)) ++ ")"

instance Show Machine where
    show (Machine inds buttons) =
        show inds ++ " " ++ show buttons

instance Hashable Indicators where
    hashWithSalt s (Indicators inds) = foldl (\acc b -> acc `hashWithSalt` b) s inds


showWithNewLines :: Show a => [a] -> String
showWithNewLines xn = concat $ intersperse "\n" (map show xn)

parseMachine :: String -> Machine
parseMachine str = Machine (Indicators indicators) (map Button buttons)
  where parts = splitOn " " str
        indicators = map (== '#') $ tail $ init $ head parts
        buttonStrings = tail (init parts)
        buttons = map (map read . splitOn "," . filter (`notElem` "()")) buttonStrings

initializeIndicators :: Indicators -> Indicators
initializeIndicators (Indicators inds) = Indicators (map (\_ -> False) inds)

togglePosition :: Indicators -> Int -> Indicators
togglePosition (Indicators inds) idx = Indicators newInds
  where
    newInds = take idx inds ++ [not (inds !! idx)] ++ drop (idx + 1) inds

applyButton :: Indicators -> Button -> Indicators
applyButton inds (Button btnNums) = foldl togglePosition inds btnNums

day10part1 :: [String] -> Integer
day10part1 xn = trace (showWithNewLines machines) sum presses
  where machines = map parseMachine xn
        presses = map minimalButtonPresses machines

day10part2 :: [String] -> Integer
day10part2 xn = 42

minimalButtonPresses :: Machine -> Integer
minimalButtonPresses (Machine targetInds buttons) =
    let initial = initializeIndicators targetInds
        neighbours inds = [applyButton inds btn | btn <- buttons]
        astar = AStar { getStart = initial
                     , getGoal = ((==) targetInds)
                     , getCost = \_ _ -> 1
                     , getNeighbours = neighbours
                     , getHeuristic = \_ -> 0 }
    in case path astar of
        Nothing -> -1
        Just p -> toInteger (length p - 1)