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

type Maze = M.Maze Char
type Located = M.Located Char
type Direction = Direction4.Direction4

data Reindeer = Reindeer Location Direction deriving (Eq, Show, Ord)

instance Hashable Reindeer where
    hashWithSalt s (Reindeer loc dir) = s `hashWithSalt` loc `hashWithSalt` dir

part1_example = do
    part1 2028 "2024/day16/example.txt" day16part1

bests' :: Maze -> Reindeer -> H.HashMap Reindeer Int -> H.HashMap Reindeer Int
bests' maze reindeer scores = 
    foldl (\sc (newReindeer,newScore) -> case H.lookup newReindeer sc of
        Just oldScore -> if newScore < oldScore then H.insert newReindeer newScore sc else sc
        Nothing -> H.insert newReindeer newScore sc
        ) scores newMoves
    where score = fromJust $ H.lookup reindeer scores
          newMoves = moves maze reindeer score

moves :: Maze -> Reindeer -> Int -> [(Reindeer, Int)]
moves maze (Reindeer loc dir) score =
    let mov = if movItem /= '#' then Just (Reindeer (Located.location movLocated) dir, score + 1) else Nothing
        rot =  Just (Reindeer loc rot90, score + 1000)
        rotcc =  Just (Reindeer loc rot90cc, score + 1000)
    in catMaybes [mov, rot, rotcc]
    where movLocated = fromJust $ M.at maze $ loc +| dir
          movItem = Located.value movLocated
          rot90 = Direction4.rotate90 dir
          rot90cc = Direction4.rotate90cc dir

bests :: Maze -> H.HashMap Reindeer Int
bests maze = scores
    where start = Reindeer (Located.location $ head $ M.findAll maze 'S') Direction4.East
          scores = H.singleton start 0

day16part1 :: [String] -> Int
day16part1 field = trace (M.showMaze (:[]) maze []) $ 0
    where maze = M.parse id field
          start = head $ M.findAll maze 'S'
          end = head $ M.findAll maze 'E'
