import Util.Euler (runLines)
import qualified Util.Maze as Maze
import Data.List.Split (splitOn)
import Debug.Trace (trace)
import qualified Data.HashSet as HS
import Text.Printf (printf)

nums :: Int -> [String] -> Int
nums i sn = trace (Maze.showMaze (\x -> " " ++ printf "%02d" x) maze HS.empty) 0
    where maze = Maze.parseItemList convert sn
          convert line = map (read :: String -> Int) $ splitOn " " line


example = runLines 70600674 "euler/input/0011.txt" (nums 4)