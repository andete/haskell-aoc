import System.IO
import Data.List (sort, sortBy)
import Control.Exception (assert)
import Util.Aoc
import Debug.Trace (trace)
import Data.List.Split (splitOn)
import Data.List (elemIndex, nub)
import Data.Maybe (fromJust)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as H
import qualified Util.Location3 as L

part1_example = do
    part1 40 "2025/day08/example.txt" (day08part1 10)

part1_input = do
    part1 1537 "2025/day08/input.txt" (day08part1 1000)

part2_example = do
    part2 40 "2025/day08/example.txt" day08part2

part2_input = do
    part2 221371496188107 "2025/day08/input.txt" day08part2

combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 1 xn = map (:[]) xn
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

data Pair = Pair L.Location3 L.Location3 Double  deriving (Eq,Show, Ord)

data Circuit = Circuit [L.Location3] deriving (Eq,Show, Ord)

makeCircuit :: ([Pair], [Circuit]) -> ([Pair], [Circuit])
makeCircuit (unseenCombinations, existingCircuits) = output
    where nextCombination = head unseenCombinations
          remainingCombinations = tail unseenCombinations
          Pair loc1 loc2 _ = nextCombination
          findCircuitsContaining loc = filter (\(Circuit locs) -> loc `elem` locs) existingCircuits
          matchingCircuits = findCircuitsContaining loc1 ++ findCircuitsContaining loc2
          output = case matchingCircuits of
            [] -> (remainingCombinations, (Circuit [loc1, loc2]) : existingCircuits)
            [h]  -> let (Circuit locs) = h in
                (remainingCombinations, (Circuit (nub (loc1:loc2:locs))) : (filter (\c -> c /= h) existingCircuits))
            [h1,h2] -> (remainingCombinations, (Circuit (nub (loc1:loc2:locs))) : (filter (\c -> c /= h1 && c /= h2) existingCircuits))
                where (Circuit locs) = let (Circuit l1) = h1; (Circuit l2) = h2 in Circuit (l1 ++ l2)
          


day08part1 :: Int -> [String] -> Integer
day08part1 count xn = trace (show sizes) result
    where parseLine x = L.fromList $ map read (splitOn "," x) :: L.Location3
          locations = map parseLine xn
          locationCombinations = map (\[a,b] -> Pair a b (L.distance a b)) $ combinations 2 locations
          sortedCombinations = sortBy (\(Pair _ _ d1) (Pair _ _ d2) -> compare d1 d2) locationCombinations
          (newS, circuits) = iterate makeCircuit (sortedCombinations, []) !! count
          sizes = reverse $ sort $ map (\(Circuit locs) -> length locs) circuits
          result = toInteger $ product $ take 3 sizes

day08part2 :: [String] -> Integer
day08part2 xn = 42