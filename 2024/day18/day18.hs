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

part1_example = do
    part1 22 "2024/day18/example.txt" (day18part1 12 6 6)

part1_input = do
    part1 292 "2024/day18/input.txt" (day18part1 1024 70 70)