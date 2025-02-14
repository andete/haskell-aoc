import Data.List.Split (splitOn)
import Util.Aoc
import qualified Data.HashMap.Strict as H

type Design = String
type Pattern = String
type Cache = H.HashMap Design Integer

parse :: [String] -> ([Pattern], [Design])
parse (a:b:xn) = (patterns, designs)
    where patterns = splitOn ", " a
          designs = xn

possible :: [Pattern] -> Design -> Bool
possible patterns [] = True
possible patterns design = any (\p -> possible patterns (drop (length p) design)) $ filter (\p -> take (length p) design == p) patterns

ways :: [Pattern] -> Design -> Integer
ways patterns [] = 1
ways patterns design = sum $ map (\p -> ways patterns (drop (length p) design)) $ filter (\p -> take (length p) design == p) patterns

waysCached :: Cache -> [Pattern] -> Design -> (Cache, Integer)
waysCached cache patterns [] = (cache, 1)
waysCached cache patterns design = case H.lookup design cache of
    Just v -> (cache, v)
    Nothing -> 
        let candidatePatterns = filter (\p -> take (length p) design == p) patterns in
        let (c, v) = foldl (\(c, v) p -> let (c', v') = waysCached c patterns (drop (length p) design) in (c', v' + v)) (cache, 0) candidatePatterns in
        (H.insert design v c, v)

day19part1 :: [String] -> Int
day19part1 input = length p
    where (patterns, designs) = parse input
          p = filter (possible patterns) designs

day19part2 :: [String] -> Integer
day19part2 input = snd $ foldl (\(c,i) d -> let (c', v') = waysCached c patterns d in (c', v' +i)) (H.empty,0) designs
    where (patterns, designs) = parse input

part1_example = do
    part1 6 "2024/day19/example.txt" day19part1

part1_input = do
    part1 296 "2024/day19/input.txt" day19part1

part2_example = do
    part2 16 "2024/day19/example.txt" day19part2

part2_input = do
    part2 619970556776002 "2024/day19/input.txt" day19part2