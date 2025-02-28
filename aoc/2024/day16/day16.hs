import Util.Aoc
import qualified Util.Maze as M
import Debug.Trace (trace)
import qualified Data.HashMap.Strict as H
import qualified Util.Direction4 as Direction4
import Util.Direction4 (Direction4, (+|))
import qualified Util.Located as Located
import Util.Location (Location)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Maybe (fromJust, catMaybes)
import Util.AStar2
import qualified Util.AStarAllBest as AStarAllBest
import Data.List (nub)
import qualified Data.HashSet as HS

type Maze = M.Maze Char
type Located = M.Located Char
type Direction = Direction4.Direction4

data Reindeer = Reindeer Location Direction deriving (Eq, Show, Ord)

location :: Reindeer -> Location
location (Reindeer loc _) = loc
direction :: Reindeer -> Direction
direction (Reindeer _ dir) = dir

instance Hashable Reindeer where
    hashWithSalt s (Reindeer loc dir) = s `hashWithSalt` loc `hashWithSalt` dir



moves :: Maze -> Reindeer -> [Reindeer]
moves maze (Reindeer loc dir) =
    let mov = if movItem /= '#' then Just $ Reindeer (Located.location movLocated) dir else Nothing
        rot =  Just $ Reindeer loc rot90
        rotcc =  Just $ Reindeer loc rot90cc
    in catMaybes [mov, rot, rotcc]
    where movLocated = fromJust $ M.at maze $ loc +| dir
          movItem = Located.value movLocated
          rot90 = Direction4.rotate90 dir
          rot90cc = Direction4.rotate90cc dir

cost :: Reindeer -> Reindeer -> Int
cost a b = if location a == location b then 1000 else 1

bestPath :: Maze -> Reindeer -> Location -> [Reindeer]
bestPath maze start end = path aStar
    where aStar = AStar {
        getStart = start,
        getGoal = (== end) . location,
        getCost = cost,
        getNeighbours = moves maze,
        getHeuristic = const 0
    }

bestPaths :: Maze -> Reindeer -> Location -> [[Reindeer]]
bestPaths maze start end = AStarAllBest.path aStar
    where aStar = AStarAllBest.AStar {
        AStarAllBest.getStart = start,
        AStarAllBest.getGoal = (== end) . location,
        AStarAllBest.getCost = cost,
        AStarAllBest.getNeighbours = moves maze,
        AStarAllBest.getHeuristic = const 0
    }

score :: [Reindeer] -> Int
score path = sum $ zipWith cost (init path) (tail path)

score2 :: [[Reindeer]] -> Int
score2 path = length $ HS.fromList $ map location $ concat path

part1_example = do
    part1 7036 "aoc/2024/day16/example.txt" day16part1


part1_example2 = do
    part1 11048 "aoc/2024/day16/example2.txt" day16part1

part1_input = do
    part1 82460 "aoc/2024/day16/input.txt" day16part1

part2_example = do
    part1 45 "aoc/2024/day16/example.txt" day16part2

part2_example2 = do
    part1 64 "aoc/2024/day16/example2.txt" day16part2

part2_input = do
    part1 590 "aoc/2024/day16/input.txt" day16part2

day16part1 :: [String] -> Int
day16part1 field = trace (M.showMaze (:[]) maze locationPath) $ score path
    where maze = M.parse id field
          start = head $ M.findAll maze 'S'
          end = head $ M.findAll maze 'E'
          reindeer = Reindeer (Located.location start) Direction4.East
          path = bestPath maze reindeer (Located.location end)
          locationPath = HS.fromList $ map location path

day16part2 :: [String] -> Int
day16part2 field = trace (show scores) $ trace (M.showMaze (:[]) maze locationPath) $ score2 lowestPaths
    where maze = M.parse id field
          start = head $ M.findAll maze 'S'
          end = head $ M.findAll maze 'E'
          reindeer = Reindeer (Located.location start) Direction4.East
          paths = bestPaths maze reindeer (Located.location end)
          scores = nub $ map score paths
          bestScore = minimum scores
          lowestPaths = filter (\p -> score p == bestScore) paths
          locationPath = HS.fromList $ map location (concat lowestPaths)

main = runAoc [("1e", part1_example), ("1i", part1_input), ("1e2", part1_example2), ("2e", part2_example), ("2i", part2_input), ("2e2", part2_example2)]