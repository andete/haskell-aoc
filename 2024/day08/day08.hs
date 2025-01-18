import Debug.Trace (trace)

import qualified Util.CharMaze as CharMaze
import Util.Aoc
import qualified Util.Location as Location
import Data.List (nub)
import Test.HUnit

type Location = Location.Location

part1_example = do
    part1 14 "2024/day08/example.txt" day08part1

part1_input = do
    part1 351 "2024/day08/input.txt" day08part1

part2_example = do
    part2 34 "2024/day08/example.txt" day08part2

part2_input = do
    part2 1259 "2024/day08/input.txt" day08part2

antinodesPart1 :: CharMaze.CharMaze -> [Char] -> [Location]
antinodesPart1 maze chars = nub $ concatMap (antinodesPart1ForAntennaType maze) chars

antinodesPart2 :: CharMaze.CharMaze -> [Char] -> [Location]
antinodesPart2 maze chars = nub $ concatMap (antinodesPart2ForAntennaType maze) chars

antinodesPart1ForAntennaType :: CharMaze.CharMaze -> Char -> [Location]
antinodesPart1ForAntennaType maze c = concatMap (uncurry (antinodesPart1ForAntennaPair maze)) (combinationPairs locations)
    where locations = CharMaze.findAll maze c

antinodesPart2ForAntennaType :: CharMaze.CharMaze -> Char -> [Location]
antinodesPart2ForAntennaType maze c = concatMap (uncurry (antinodesPart2ForAntennaPair maze)) (combinationPairs locations)
    where locations = CharMaze.findAll maze c

antinodesPart1ForAntennaPair :: CharMaze.CharMaze -> Location -> Location -> [Location]
antinodesPart1ForAntennaPair maze loc1 loc2 = filter (CharMaze.validLocation maze) [antinode1, antinode2]
    where dx = Location.x loc2 - Location.x loc1
          dy = Location.y loc2 - Location.y loc1
          antinode1 = Location.Location (Location.x loc1 - dx) (Location.y loc1 - dy)
          antinode2 = Location.Location (Location.x loc2 + dx) (Location.y loc2 + dy)

antinodesPart2ForAntennaPair :: CharMaze.CharMaze -> Location -> Location -> [Location]
antinodesPart2ForAntennaPair maze loc1 loc2 = 
    takeWhile (CharMaze.validLocation maze) antinodes1 ++ 
    takeWhile (CharMaze.validLocation maze) antinodes2    
    
    where dx = Location.x loc2 - Location.x loc1
          dy = Location.y loc2 - Location.y loc1
          antinodes1 = map (\q -> Location.Location (Location.x loc1 - q * dx) (Location.y loc1 - q * dy)) [0..]
          antinodes2 = map (\q -> Location.Location (Location.x loc2 + q * dx) (Location.y loc2 + q * dy)) [0..]

day08part1 :: [String] -> Int
day08part1 field = trace (CharMaze.show maze []) $ trace (show chars) $
    length $ antinodesPart1 maze chars
    where maze = CharMaze.parse field
          chars = filter (/= '.') $ CharMaze.chars maze


day08part2 :: [String] -> Int
day08part2 field = trace (CharMaze.show maze []) $ trace (show chars) $
    length $ antinodesPart2 maze chars
    where maze = CharMaze.parse field
          chars = filter (/= '.') $ CharMaze.chars maze


combinationPairs :: [a] -> [(a,a)]
combinationPairs [] = []
combinationPairs (x:xs) = map (\y -> (x,y)) xs ++ combinationPairs xs

testCombinationPairs123 :: Test
testCombinationPairs123 = TestCase $ assertEqual "combinationPairs" [(1,2),(1,3),(2,3)] (combinationPairs [1, 2, 3])

testCombinationPairsLocations :: Test
testCombinationPairsLocations = TestCase $ assertEqual "combinationPairs" [((1,2),(2,5)),((1,2),(7,3)),((2,5), (7,3))] (combinationPairs [(1,2), (2,5), (7,3)])

tests :: Test
tests = TestList [testCombinationPairs123, testCombinationPairsLocations]

testMain :: IO ()
testMain = runTestTT tests >>= print