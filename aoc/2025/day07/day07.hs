import System.IO
import Data.List (sort)
import Control.Exception (assert)
import Util.Aoc
import Debug.Trace (trace)
import Data.List.Split (splitOn)
import Data.List (elemIndex, nub)
import Data.Maybe (fromJust)
import qualified Util.Maze as Maze
import qualified Data.HashSet as HS
import qualified Util.Direction4 as Directional4
import Util.Direction4 ((+|))
import Util.Located (Located(..))
import Util.Location (Location(..))
import qualified Data.HashMap.Strict as H

part1_example = do
    part1 21 "2025/day07/example.txt" day07part1

part1_input = do
    part1 1537 "2025/day07/input.txt" day07part1

part2_example = do
    part2 40 "2025/day07/example.txt" day07part2

part2_input = do
    part2 221371496188107 "2025/day07/input.txt" day07part2

beam :: Maze.Maze Char -> [Location] -> [Location] -> ([Location], Maze.Maze Char)
beam maze splittersSeen [] = (splittersSeen, maze)
beam maze splittersSeen (loc:locs) = next
    where south = loc +| Directional4.South
          atSouth = Maze.at' maze south
          next = if south `elem` splittersSeen then beam maze splittersSeen locs else
            case atSouth of
              Located _ (Just '.') -> beam (Maze.set '|' south maze) splittersSeen (locs ++ [south])
              Located _ (Just '^') -> beam maze (splittersSeen ++ [south]) (locs ++ [loc +| Directional4.West, loc +| Directional4.East])
              Located _ (Just '|') -> beam maze splittersSeen (locs ++ [south])
              Located _ _ -> beam maze splittersSeen locs

day07part1 :: [String] -> Integer
day07part1 xn = trace (Maze.showMaze (:[]) endMaze (HS.fromList splittersSeen) ) (toInteger $ length splittersSeen)
  where maze = Maze.parse id xn
        (Located start _) = head $ Maze.findAll maze 'S'
        (splittersSeen, endMaze) = beam maze [] [start]

type Cache = H.HashMap Location Integer

countPathsCached :: Cache -> Maze.Maze Char -> Location -> (Cache, Integer)
countPathsCached cache maze loc = case H.lookup loc cache of
    Just v -> (cache, v)
    Nothing -> case Maze.at' maze loc of
        Located _ (Just '|') -> countPathsCached cache maze (loc +| Directional4.South)
        Located _ (Just '^') -> let locs = map (loc +|) [Directional4.West, Directional4.East]
                                    in foldl (\(c, v) l -> 
                                        let (c', v') = countPathsCached c maze l in (H.insert loc (v' + v) c', v' + v)) (cache, 0) locs
        _ -> (H.insert loc 1 cache, 1)

countPaths :: Maze.Maze Char -> Integer -> Location -> Integer
countPaths maze count loc = case Maze.at' maze loc of
    Located _ (Just '|') -> countPaths maze count (loc +| Directional4.South)
    Located _ (Just '^') -> let locs = map (loc +|) [Directional4.West, Directional4.East]
                            in sum $ map (countPaths maze count) locs
    _ -> 1

day07part2 :: [String] -> Integer
day07part2 xn =  trace (Maze.showMaze (:[]) mazeWithPaths HS.empty) (snd $ countPathsCached H.empty mazeWithPaths firstBeam)
  where maze = Maze.parse id xn
        (Located start _) = head $ Maze.findAll maze 'S'
        (splittersSeen, mazeWithPaths) = beam maze [] [start]
        firstBeam = start +| Directional4.South

main :: IO ()
main = part2_input
