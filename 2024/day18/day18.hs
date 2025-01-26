import Util.Location (Location (..))
import Data.List.Split (splitOn)
import Util.AStar
    ( AStar(AStar, getHeuristic, getStart, getGoal, getCost,
            getNeighbours),
      path )
import qualified Data.HashSet as H
import qualified Util.Direction4 as Direction
import Util.Aoc
import Debug.Trace (trace)
import Data.Maybe (isJust)

parse:: [String] -> [Location]
parse = map (((\yn -> Location (head yn) (yn !! 1)) . map read) . splitOn ",")

day18part1 :: Int -> Int -> Int -> [String] -> Int
day18part1 fallen endX endY input = length p - 1
    where locations = H.fromList $ take fallen $ parse input
          astar = AStar {
            getStart = Location 0 0,
            getGoal = \l -> l == Location endX endY,
            getCost = \_ _ -> 1,
            getNeighbours = \l -> filter (\(Location x y) -> x >=0 && y >= 0 && x <= endX && y <= endY) $ filter (`notElem` locations) $ map (l Direction.+|) Direction.all,
            getHeuristic = const 0
          }
          p = path astar

day18part2 :: Int -> Int -> Int -> [String] -> String
day18part2 start endX endY input = 
    let idxn = takeWhile (\i -> isJust $ path $ astar (H.fromList $ take i locations)) [start..] in
    let (Location x y) = locations !! (length idxn + start - 1) in
        show x ++ "," ++ show y
    where locations = parse input
          astar loc = AStar {
            getStart = Location 0 0,
            getGoal = \l -> l == Location endX endY,
            getCost = \_ _ -> 1,
            getNeighbours = \l -> filter (\(Location x y) -> x >=0 && y >= 0 && x <= endX && y <= endY) $ filter (`notElem` loc) $ map (l Direction.+|) Direction.all,
            getHeuristic = const 0
          }
        
part1_example = do
    part1 22 "2024/day18/example.txt" (day18part1 12 6 6)

part1_input = do
    part1 292 "2024/day18/input.txt" (day18part1 1024 70 70)

part2_example = do
    part2 "6,1" "2024/day18/example.txt" (day18part2 1 6 6)

-- this takes several minutes, perhaps a binary search would be faster
part2_input = do
    part2 "58,44" "2024/day18/input.txt" (day18part2 1024 70 70)

main = do part2_input