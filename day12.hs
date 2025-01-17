import Aoc
import Maze
import Data.List (nub, sort)
import Debug.Trace (trace)
import qualified Located
import qualified Data.HashSet as H
import Data.Hashable (Hashable)

type Crop = Char

part1_example1 = do
    part1 140 "day12/example1.txt" day12part1

part1_example1a = do
    part1 772 "day12/example1a.txt" day12part1

part1_example2 = do
    part1 1930 "day12/example2.txt" day12part1

part1_input = do
    part1 1473620 "day12/input.txt" day12part1

-- recognize areas of the same crop by a painting algorithm
paint :: Maze Char -> [[Located Char]]
paint maze = fst $ foldl (\(acc, v) plot ->
    if H.member plot v
        then (acc,v)
        else let xn = paintOne maze plot
                 v' = foldl (flip H.insert) v xn in
            (xn: acc, v')) ([], H.empty) (Maze.items maze)

-- why is the nub needed ?
paintOne :: Maze Char -> Located Char -> [Located Char]
paintOne maze plot = nub $ snd $ paintOne' (H.fromList [plot]) [plot] maze plot

paintOne' :: H.HashSet (Located Char) -> [Located Char] -> Maze Char -> Located Char -> (H.HashSet (Located Char), [Located Char])
paintOne' visited acc maze plot = 
    foldl (\(v, acc') n -> paintOne' (H.insert n v) (n:acc') maze n) (visited,acc) neighbours
    where crop = Located.value plot
          loc = Located.location plot
          neighbours = filter (\l -> Located.value l == crop && not (H.member l visited)) (Maze.neighbours maze loc)

area :: [Located a] -> Int
area = length

-- amount of different crop neighbours for each part of the plot
perimeter :: Maze Char -> [Located Char] -> Int
perimeter maze l = length n
    where crop = Located.value (head l)
          n = concatMap (filter (\(Located _ c) -> c /= Just crop) . Maze.neighbours' maze . Located.location) l

price :: Maze Char -> [Located Char] -> Int
price maze a = ar * p
    where ar = area a
          p = perimeter maze a
          crop = Located.value (head a)

day12part1 :: [String] -> Int
day12part1 field = sum $ map (price maze) painted
    where maze = Maze.parse id field
          painted = paint maze

main = do part1_input