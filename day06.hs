import Text.Printf (printf)
import Aoc
import Debug.Trace (trace)
import qualified CharMaze
import CharMaze (CharMaze, LocatedChar(..))
import Direction4 (Direction4 (North), (+|), rotate90)
import Location (Location(..))
import Data.Maybe (isNothing, isJust)
import Data.List (nub)
import GHC.Base (maxInt)

part1_example = do
    part1 41 "day06/example.txt" day06part1

part1_input = do
    part1 5153 "day06/input.txt" day06part1

part2_example = do
    part2 6 "day06/example.txt" day06part2

part2_input = do
    part2 1711 "day06/input.txt" day06part2

moves :: CharMaze -> Maybe Location -> Direction4 -> [Location] -> [Location]
moves _ Nothing _ visited = nub visited
moves maze (Just loc) dir visited =
    let next = CharMaze.at maze (loc +| dir) in
        if isNothing next then nub visited else
            let Just (LocatedChar nextLoc nextChar) = next in
                if nextChar /= '#'
                    then moves maze (Just nextLoc) dir (nextLoc:visited)
                    else moves maze (Just loc) (rotate90 dir) visited

guardWillLoop :: CharMaze -> Bool
guardWillLoop maze = guardWillLoop' (start, Direction4.North) [] maze
    where start = CharMaze.find maze '^'

guardWillLoop' :: (Location, Direction4) -> [(Location, Direction4)] -> CharMaze -> Bool
guardWillLoop' (location, direction) visited maze
 | (location, direction) `elem` visited = True
 | otherwise =
    let next = CharMaze.at maze (location +| direction) in
        (isJust next && (let Just (LocatedChar nextLoc nextChar) = next in
                             if nextChar /= '#'
                                 then guardWillLoop' (nextLoc, direction) ((location, direction):visited) maze
                                 else guardWillLoop' (location, rotate90 direction) visited maze))

day06part1 :: [String] -> Int
day06part1 field = -- trace (CharMaze.show maze []) $
    let start = CharMaze.find maze '^' in
        let ms = moves maze (Just start) Direction4.North [start] in
        --trace (show start) $ trace (CharMaze.show maze ms) 
        length ms
    where maze = CharMaze.parse field

day06part2 :: [String] -> Int
day06part2 field = length $ filter (guardWillLoop . CharMaze.set maze '#') obstacleCandidates
    where maze = CharMaze.parse field
          start = CharMaze.find maze '^'
          obstacleCandidates = filter (/= start) $ moves maze (Just start) Direction4.North [start]

main = do part2_input