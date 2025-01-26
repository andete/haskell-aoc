import Util.Aoc
import qualified Util.Maze as M
import Debug.Trace (trace)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as H
import Util.AStar2
import Util.Located
import Util.Maze (Maze)
import Util.Direction4
import Data.Maybe (isJust, fromJust)
import Data.Bifunctor (second)
import Data.List (elemIndex)

-- exploration: just look at the path through the maze
-- looking at the path found without any cheats shows the path covers every open spot in the maze!!

day20part1pre :: [String] -> Int
day20part1pre input = trace (M.showMaze (: []) maze sp) $ length p
    where maze = M.parse id input
          start = head $ M.findAll maze 'S'
          end = head $ M.findAll maze 'E'
          neigh loc = filter (\v -> value v /= '#') $ M.neighbours maze (location loc)
          astar = AStar start (== end) (\_ _ -> 1) neigh (const 0)
          p = path astar
          sp = HS.fromList $ map location p

part1pre_example = do
    part1 85 "2024/day20/example.txt" day20part1pre

part1pre_input = do
    part1 9441 "2024/day20/input.txt" day20part1pre

-- part 1: for each position in the path, try doing cheats and see how much we can reduce the path length

cheat :: Maze Char -> [Located Char] -> H.HashMap Int Int -> Located Char -> H.HashMap Int Int
cheat maze path cr loc = cr'
    where neighFun loc = filter (\v -> value v /= '#') $ M.neighbours maze (location loc)
          -- look at neighbours and one step further
          neigh = M.neighboursDir maze (location loc)
          neigh2 = map (second fromJust) $ filter (\(_,l) -> isJust l) $ map (\(l, dir) -> (l, M.at maze (location l +| dir))) neigh
          -- make sure the neighbour is a wall and one step further isn't
          neigh3 = filter (\(l1, l2) -> value l1 == '#' && value l2 /= '#') neigh2
          -- make sure if we cheat we end up on a position further in the path
          neigh4 = filter (\(l1, l2) -> fromJust (elemIndex loc path) < fromJust (elemIndex l2 path)) neigh3
          -- insert the saved steps in the hashmap
          cr' = foldl (\h (l1, l2) -> let d = fromJust (elemIndex l2 path) - fromJust (elemIndex loc path) - 2 in
                H.insert d (H.lookupDefault 0 d h + 1) h) cr neigh4



day20part1 :: Int -> [String] -> Int
day20part1 saving input = trace (show cr) $ trace (M.showMaze (: []) maze sp) $ sum $ H.elems cr'
    where maze = M.parse id input
          start = head $ M.findAll maze 'S'
          end = head $ M.findAll maze 'E'
          neigh loc = filter (\v -> value v /= '#') $ M.neighbours maze (location loc)
          astar = AStar start (== end) (\_ _ -> 1) neigh (const 0)
          p = path astar
          sp = HS.fromList $ map location p
          cr = foldl (cheat maze p) H.empty p
          cr' = H.filterWithKey (\k _ -> k >= saving) cr

part1_example = do
    part1 10 "2024/day20/example.txt" (day20part1 10)

part1_input = do
    part1 1399 "2024/day20/input.txt" (day20part1 100)