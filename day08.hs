import Debug.Trace (trace)

import qualified CharMaze
import Aoc
import Location
import Data.List (nub)
import Test.HUnit


part1_example = do
    part1 14 "day08/example.txt" day08part1

part1_input = do
    part1 351 "day08/input.txt" day08part1

antinodes :: CharMaze.CharMaze -> [Char] -> [Location]
antinodes maze chars = nub $ concatMap (antinodes1 maze) chars

combinationPairs :: [a] -> [(a,a)]
combinationPairs [] = []
combinationPairs (x:xs) = map (\y -> (x,y)) xs ++ combinationPairs xs

antinodes1 :: CharMaze.CharMaze -> Char -> [Location]
antinodes1 maze c = concatMap (uncurry (antinodes2 maze)) (combinationPairs locations)
    where locations = CharMaze.findAll maze c

antinodes2 :: CharMaze.CharMaze -> Location -> Location -> [Location]
antinodes2 maze loc1 loc2 = filter (CharMaze.validLocation maze) [antinode1, antinode2]
    where dx = Location.x loc2 - Location.x loc1
          dy = Location.y loc2 - Location.y loc1
          antinode1 = Location (Location.x loc1 - dx) (Location.y loc1 - dy)
          antinode2 = Location (Location.x loc2 + dx) (Location.y loc2 + dy)


day08part1 :: [String] -> Int
day08part1 field = trace (CharMaze.show maze []) $ trace (show chars) $
    length $ antinodes maze chars
    where maze = CharMaze.parse field
          chars = filter (/= '.') $ CharMaze.chars maze


testCombinationPairs123 :: Test
testCombinationPairs123 = TestCase $ assertEqual "combinationPairs" [(1,2),(1,3),(2,3)] (combinationPairs [1, 2, 3])

testCombinationPairsLocations :: Test
testCombinationPairsLocations = TestCase $ assertEqual "combinationPairs" [((1,2),(2,5)),((1,2),(7,3)),((2,5), (7,3))] (combinationPairs [(1,2), (2,5), (7,3)])

tests :: Test
tests = TestList [testCombinationPairs123, testCombinationPairsLocations]

testMain :: IO ()
testMain = runTestTT tests >>= print