import Util.Euler (runLines)
import qualified Util.Maze as Maze
import Data.List.Split (splitOn)
import Debug.Trace (trace)
import qualified Data.HashSet as HS
import Text.Printf (printf)
import Util.Location
import Util.Located
import Util.Direction8
import Data.Foldable (maximumBy)

range :: Maze.Maze a -> Location -> Int -> Direction8 -> [Located a]
range maze loc i dir = let r = range' loc dir in
    if length r < i then [] else take i r
    where range' loc dir = case Maze.at maze loc of
            Just a -> a : range' (loc +| dir) dir
            Nothing -> []

score :: [Located Int] -> Int
score = product . map value

options :: Maze.Maze Int -> Location -> [([Located Int], Int)]
options maze loc = map ((\l -> (l, score l)) . range maze loc 4) [East, SouthEast, South, SouthWest]

options' :: Maze.Maze Int -> [([Located Int], Int)]
options' maze = concatMap (\(Located loc c) -> options maze loc) $ Maze.items maze

nums :: Int -> [String] -> Int
nums i sn = let (r, m) = maximumBy (\x y -> compare (snd x) (snd y)) $ options' maze in
    let hs = HS.fromList $ map location r in
    trace (Maze.showMaze (\x -> " " ++ printf "%02d" x) maze hs) m
    where maze = Maze.parseItemList convert sn
          convert line = map (read :: String -> Int) $ splitOn " " line


example = runLines 70600674 "euler/input/0011.txt" (nums 4)