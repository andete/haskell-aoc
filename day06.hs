import Text.Printf (printf)
import Aoc
import Debug.Trace (trace)
import qualified CharMaze
import CharMaze (CharMaze, LocatedChar(..))
import Direction4 (Direction4 (North), (+|), rotate90)
import Location (Location(..))
import Data.Maybe (isNothing)
import Data.List (nub)
import GHC.Base (maxInt)

part1_example = do
    part1 41 "day06/example.txt" day06part1

part1_input = do
    part1 5153 "day06/input.txt" day06part1

moves :: CharMaze -> Maybe Location -> Direction4 -> [Location] -> [Location]
moves _ Nothing _ visited = nub visited
moves maze (Just loc) dir visited =
    let next = CharMaze.at maze (loc +| dir) in
        if isNothing next then nub visited else
            let Just (LocatedChar nextLoc nextChar) = next in
                if nextChar /= '#' 
                    then moves maze (Just nextLoc) dir (nextLoc:visited) 
                    else moves maze (Just loc) (rotate90 dir) visited 

day06part1 :: [String] -> Int
day06part1 field = -- trace (CharMaze.show maze []) $
    let start = CharMaze.find maze '^' in
        let ms = moves maze (Just start) Direction4.North [start] in
        --trace (show start) $ trace (CharMaze.show maze ms) 
        length ms
    where maze = CharMaze.parse field
