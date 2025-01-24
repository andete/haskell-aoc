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
import Util.AStar
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



moves2 :: Maze -> Reindeer -> [Reindeer]
moves2 maze (Reindeer loc dir) =
    let mov = if movItem /= '#' then Just $ Reindeer (Located.location movLocated) dir else Nothing
        rot =  Just $ Reindeer loc rot90
        rotcc =  Just $ Reindeer loc rot90cc
    in catMaybes [mov, rot, rotcc]
    where movLocated = fromJust $ M.at maze $ loc +| dir
          movItem = Located.value movLocated
          rot90 = Direction4.rotate90 dir
          rot90cc = Direction4.rotate90cc dir

bests :: Maze -> H.HashMap Reindeer Int
bests maze = scores
    where start = Reindeer (Located.location $ head $ M.findAll maze 'S') Direction4.East
          scores = H.singleton start 0

cost :: Reindeer -> Reindeer -> Int
cost a b = if location a == location b then 1000 else 1

bestPath :: Maze -> Reindeer -> Location -> [Reindeer]
bestPath maze start end = path aStar
    where aStar = AStar {
        getStart = start,
        getGoal = \c visited -> location c == end,
        getCost = cost,
        getNeighbours = \c visited -> moves2 maze c,
        getHeuristic = const 0
    }

score :: [Reindeer] -> Int
score path = sum $ zipWith cost (init path) (tail path)

part1_example = do
    part1 7036 "2024/day16/example.txt" day16part1


part1_example2 = do
    part1 11048 "2024/day16/example2.txt" day16part1

part1_input = do
    part1 82460 "2024/day16/input.txt" day16part1

day16part1 :: [String] -> Int
day16part1 field = trace (M.showMaze (:[]) maze locationPath) $ score path
    where maze = M.parse id field
          start = head $ M.findAll maze 'S'
          end = head $ M.findAll maze 'E'
          reindeer = Reindeer (Located.location start) Direction4.East
          path = bestPath maze reindeer (Located.location end)
          locationPath = map location path

