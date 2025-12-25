import System.IO
import Data.List (sort, sortBy, tails)
import Control.Exception (assert)
import Util.Aoc
import Debug.Trace (trace)
import Data.List.Split (splitOn)
import Data.List (elemIndex, nub, tails, sortOn)
import Data.Maybe (fromJust)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as H
import qualified Util.Location as L
import qualified Util.Maze as Maze

combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 1 xn = map (:[]) xn
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

part1_example = do
    part1 50 "2025/day09/example.txt" day09part1

part1_input = do
    part1 4752484112 "2025/day09/input.txt" day09part1

part2_example = do
    part2 24 "2025/day09/example.txt" day09part2

part2_input = do
    part2 221371496188107 "2025/day09/input.txt" day09part2

day09part1 :: [String] -> Integer
day09part1 xn = trace (show biggest) $ result
  where locations = map (L.fromList . map read . splitOn ",") xn :: [L.Location]
        combinationLocs = combinations 2 locations
        sorted = sortBy (\[a,b] [c,d] -> compare (L.hammingDistance c d) (L.hammingDistance a b)) combinationLocs
        biggest = head sorted
        result = ((toInteger . abs) (L.x (biggest !! 0) - L.x (biggest !! 1)) + 1)
             * ((toInteger . abs) (L.y (biggest !! 0) - L.y (biggest !! 1)) + 1)

data Tile = Tile L.Location Char deriving (Eq,Show, Ord)

makeGreenTiles :: (L.Location, L.Location) -> [Tile]
makeGreenTiles (loc1, loc2) 
    | (L.x loc1 == L.x loc2) = let x = L.x loc1
                                   ys = [min (L.y loc1) (L.y loc2) + 1 .. max (L.y loc1) (L.y loc2) - 1] 
                               in map (\y -> Tile (L.Location x y) 'X') ys
    | (L.y loc1 == L.y loc2) = let y = L.y loc1
                                   xs = [min (L.x loc1) (L.x loc2) + 1 .. max (L.x loc1) (L.x loc2) - 1]
                                in map (\x -> Tile (L.Location x y) 'X') xs

fillGreenTiles :: [Tile] -> [Tile]
fillGreenTiles green = rowFillTiles ++ colFillTiles
  where
    minX = minimum $ map (\(Tile (L.Location x _) _) -> x) green
    maxX = maximum $ map (\(Tile (L.Location x _) _) -> x) green
    minY = minimum $ map (\(Tile (L.Location _ y) _) -> y) green
    maxY = maximum $ map (\(Tile (L.Location _ y) _) -> y) green
    greenLocs = map (\(Tile (L.Location x y) _) -> (x, y)) green
    -- Build maps from y to all x's in that row, and x to all y's in that column
    rowMap = H.fromListWith (++) [(y, [x]) | (x, y) <- greenLocs]
    colMap = H.fromListWith (++) [(x, [y]) | (x, y) <- greenLocs]
    -- For each row, fill between minX and maxX of green tiles in that row
    rowFillTiles = concatMap fillRow [minY..maxY]
    fillRow y = case H.lookup y rowMap of
      Just xs | length xs >= 2 ->
        let minx = minimum xs
            maxx = maximum xs
        in [Tile (L.Location x y) 'X' | x <- [minx+1 .. maxx-1], notElem (x, y) greenLocs]
      _ -> []
    -- For each column, fill between minY and maxY of green tiles in that column
    colFillTiles = concatMap fillCol [minX..maxX]
    fillCol x = case H.lookup x colMap of
      Just ys | length ys >= 2 ->
        let miny = minimum ys
            maxy = maximum ys
        in [Tile (L.Location x y) 'X' | y <- [miny+1 .. maxy-1], notElem (x, y) greenLocs]
      _ -> []

mazeFromTiles :: [Tile] -> Maze.Maze Char
mazeFromTiles tiles = foldl (\m (Tile loc c) -> Maze.set c loc m) emptyMaze tiles
  where maxX = maximum $ map (\(Tile (L.Location x _) _) -> x) tiles
        maxY = maximum $ map (\(Tile (L.Location _ y) _) -> y) tiles
        emptyMaze = Maze.emptyCharMaze '.' (maxX+1) (maxY+1)

allRedOrGreen :: H.HashMap L.Location Char -> (L.Location, L.Location) -> Bool
allRedOrGreen tileMap (loc1, loc2) = all isRedOrGreen between
  where isRedOrGreen loc = case H.lookup loc tileMap of
            Just '#' -> True
            Just 'X' -> True
            _ -> False
        between = if L.x loc1 == L.x loc2 then
                    [L.Location (L.x loc1) y | y <- [min (L.y loc1) (L.y loc2) .. max (L.y loc1) (L.y loc2)]]
                  else
                    [L.Location x (L.y loc1) | x <- [min (L.x loc1) (L.x loc2) .. max (L.x loc1) (L.x loc2)]]   

day09part2 :: [String] -> Integer
day09part2 xn = result
  where
    locations = map (L.fromList . map read . splitOn ",") xn :: [L.Location]
    redTiles = map (\loc -> Tile loc '#') locations
    sequentialPairs = zip locations (tail locations) ++ [(last locations, head locations)]
    greenBetween = concatMap makeGreenTiles sequentialPairs
    filledGreen = fillGreenTiles (greenBetween ++ redTiles)
    allTiles = sort $ redTiles ++ greenBetween ++ filledGreen
    tileSet = HS.fromList $ map (\(Tile loc _) -> loc) allTiles
    -- Only consider pairs of red tiles not on the same row or column
    redLocs = map (\(Tile loc _) -> loc) redTiles
    pairs = [(a, b) | (a:rest) <- tails redLocs, b <- rest, L.x a /= L.x b, L.y a /= L.y b]
    -- TODO: only take pairs where there are no other red tiles in between
    
    -- For each pair, check if the rectangle is fully filled with red or green
    -- Sort pairs by area descending
    sortedPairs = reverse $ sortOn (\(a, b) -> rectArea a b) pairs
    rectArea a b = toInteger (abs (L.x a - L.x b) + 1) * toInteger (abs (L.y a - L.y b) + 1)
    isFilledRect a b = all (\loc -> HS.member loc tileSet) (rectInterior a b)
    rectInterior a b = [L.Location x y |
      x <- [min (L.x a) (L.x b) .. max (L.x a) (L.x b)],
      y <- [min (L.y a) (L.y b) .. max (L.y a) (L.y b)]]
    result = trace (show sortedPairs)go sortedPairs
    go [] = 0
    go ((a, b):rest) = if isFilledRect a b then rectArea a b else go rest