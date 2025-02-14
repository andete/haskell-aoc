import Util.Aoc
import qualified Util.Maze as M
import Debug.Trace (trace)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as H
import Util.AStar
import Util.Located
import Util.Maze (Maze)
import Util.Direction4
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.Bifunctor (second)
import Data.List (elemIndex, sortBy, sort)
import Util.Location (hammingDistance, Location (Location))
import qualified GHC.Real as HS

-- exploration: just look at the path through the maze
-- looking at the path found without any cheats shows the path covers every open spot in the maze!!

day20part1pre :: [String] -> Int
day20part1pre input = trace (M.showMaze (: []) maze sp) $ length p
    where maze = M.parse id input
          start = head $ M.findAll maze 'S'
          end = head $ M.findAll maze 'E'
          neigh loc = filter (\v -> value v /= '#') $ M.neighbours maze (location loc)
          astar = AStar start (== end) (\_ _ -> 1) neigh (const 0)
          p = fromJust $ path astar
          sp = HS.fromList $ map location p

part1pre_example = do
    part1 85 "2024/day20/example.txt" day20part1pre

part1pre_input = do
    part1 9441 "2024/day20/input.txt" day20part1pre

-- part 1: for each position in the path, try doing cheats and see how much we can reduce the path length

cheat :: Int -> Maze Char -> [Located Char] -> H.HashMap Int Int -> Located Char -> H.HashMap Int Int
cheat saving maze path cr loc = cr'
    where neighFun loc = filter (\v -> value v /= '#') $ M.neighbours maze (location loc)
          -- look at neighbours and one step further
          neigh = M.neighboursDir maze (location loc)
          neigh2 = map (second fromJust) $ filter (\(_,l) -> isJust l) $ map (\(l, dir) -> (l, M.at maze (location l +| dir))) neigh
          -- make sure the neighbour is a wall and one step further isn't
          neigh3 = filter (\(l1, l2) -> value l1 == '#' && value l2 /= '#') neigh2
          -- make sure if we cheat we end up on a position further in the path
          neigh4 = filter (\(l1, l2) -> fromJust (elemIndex loc path) < fromJust (elemIndex l2 path)) neigh3
          -- insert the saved steps in the hashmap, but only if they save at least 'saving' steps
          cr' = foldl (\h (l1, l2) -> let d = fromJust (elemIndex l2 path) - fromJust (elemIndex loc path) - 2 in
                if d >= saving then H.insert d (H.lookupDefault 0 d h + 1) h else h) cr neigh4

day20part1 :: Int -> [String] -> Int
day20part1 saving input = trace (show cr) $ trace (M.showMaze (: []) maze sp) $ sum $ H.elems cr
    where maze = M.parse id input
          start = head $ M.findAll maze 'S'
          startLocation = location start
          end = head $ M.findAll maze 'E'
          neigh loc = filter (\v -> value v /= '#') $ M.neighbours maze (location loc)
          astar = AStar start (== end) (\_ _ -> 1) neigh (const 0)
          p = fromJust $ path astar
          sp = HS.fromList $ map location p
          cr = foldl (cheat saving maze p) H.empty p

part1_example = do
    part1 10 "2024/day20/example.txt" (day20part1 10) -- 10 cheats that save 10 picoseconds

part1_input = do
    part1 1399 "2024/day20/input.txt" (day20part1 100) -- 1399 cheats that save 100 picoseconds

-- part 2: instead of cheating 1, we can do upto 20 cheats in one go

-- -- test finding locations around a certain position in a fast way

day20testAround :: Int -> Int -> [String] -> Int
day20testAround x y input = trace (M.showMaze (: []) maze a') 0
    where maze = M.parse id input
          a = HS.fromList $ map location $ M.around maze 20 (Location x y)
          a' = HS.delete (Location x y) a

part2_input_testAround = do
    part1 0 "2024/day20/input.txt" (day20testAround 30 30)

type IndexMap = H.HashMap Location Int
lookUp :: IndexMap -> Location -> Int
lookUp indexMap loc = fromJust $ H.lookup loc indexMap

data Cheat = Cheat [Location] Int deriving (Show)

wallPath :: Maze Char -> Location -> Location -> Maybe [Location]
wallPath maze start end = path astar
    where astar = AStar start (== end) (\_ _ -> 1) n (const 0)
          n a = map location $ filter (\located -> value located == '#' || location located == end) $ M.neighbours maze a

positionsWithinReach :: Maze Char -> IndexMap -> Int -> Location -> [Cheat]
positionsWithinReach maze indexMap reach loc =
    mapMaybe (wpOk . wp) around
    where around = M.around maze reach loc
          locIndex = lookUp indexMap loc
          notWall = (/=) '#' . value
          gain' l s = lookUp indexMap l - locIndex - s - 1
          -- if not wall and further down the path, try to find a wall-path to the location
          wp l = if notWall l && gain' (location l) 1 >= 0 then wallPath maze loc (location l) else Nothing
          -- the wall path cannot be longer then the 'reach' + 2 as we can only cheat 'reach' steps
          wpOk ml = case ml of
              Just l -> if length l < reach + 2 then Just $ Cheat l (gain' (last l) (length l - 2)) else Nothing
              Nothing -> Nothing

day20part2 :: Int -> Int -> [String] -> Int
day20part2 reach gain input = trace (show cheatStats) $ trace (M.showMaze (:[]) maze (HS.fromList pLoc)) $ trace (show $ cheats !! 1) $ 
    length $ filter (\(Cheat _ g) -> g >= gain) cheats
    where maze = M.parse id input
          start = head $ M.findAll maze 'S'
          startLocation = location start
          end = head $ M.findAll maze 'E'
          neigh loc = filter (\v -> value v /= '#') $ M.neighbours maze (location loc)
          astar = AStar start (== end) (\_ _ -> 1) neigh (const 0)
          p = fromJust $ path astar
          pLoc = map location p
          indexMap = H.fromList $ zip pLoc [0..]
          cheats = concatMap (positionsWithinReach maze indexMap reach) pLoc
          cheatStats = sort $ filter (\(a,_) -> a > 0) $ H.toList $ foldl (\h (Cheat _ g) -> H.insert g (H.lookupDefault 0 g h + 1) h) H.empty cheats        

part2_example = do
    part1 273 "2024/day20/example.txt" (day20part2 20 50)