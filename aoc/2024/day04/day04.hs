import Util.Aoc
import Debug.Trace
import Text.Printf (printf)
import Data.Bifunctor (bimap)

part1_example = do
    part1 18 "2024/day04/example.txt" day04part1

part1_input = do
    part1 2532 "2024/day04/input.txt" day04part1


part2_example = do
    part2 9 "2024/day04/example.txt" day04part2

part2_input = do
    part2 1941 "2024/day04/input.txt" day04part2

charAt :: [String] -> Int -> Int -> Char
charAt field x y
    | y < 0 || y >= length field = ' '
    | x < 0 || x >= length (head field) = ' '
    | otherwise = (field !! y) !! x


charIsAt :: [String] -> Int -> Int -> Char -> Bool
charIsAt field x y c = charAt field x y == c

charsAt :: [String] -> [(Char, Int, Int)] -> Bool
charsAt field positions = and [charIsAt field x y c | (c, x, y) <- positions]

xmasAt :: [String] -> [(Int, Int)] -> Bool
xmasAt field positions = charsAt field [(c, x, y) | ((x, y), c) <- zip positions "XMAS"]

xmas2At :: [String] -> Int -> Int -> Bool
xmas2At field x y 
    | not $ charIsAt field x y 'A' = False
    | otherwise = 
        let u = charAt field (x - 1) (y - 1)
            v = charAt field (x + 1) (y + 1)
            w = charAt field (x - 1) (y + 1)
            z = charAt field (x + 1) (y - 1)
            in
            (u == 'M' && v == 'S' || u == 'S' && v == 'M') && (w == 'M' && z == 'S' || w == 'S' && z == 'M')

testit = do print $ xmasAt ["XMAS"] [(0,0), (1,0), (2,0), (3,0)]

xmasOrientations = [
    [(0,0), (0,1), (0,2), (0, 3)],     -- S
    [(0,0), (1,1), (2,2), (3,3)],      -- SE
    [(0,0), (1,0), (2,0), (3,0)],      -- E
    [(0,0), (1,-1), (2,-2), (3,-3)],   -- NE
    [(0,0), (-1,0), (-2,0), (-3,0)],   -- W
    [(0,0), (-1,-1), (-2,-2), (-3,-3)],-- NW
    [(0,0), (0,-1), (0,-2), (0,-3)],   -- N
    [(0,0), (-1,1), (-2,2), (-3,3)]    -- SW
    ]

xSize :: [String] -> Int
xSize field = length (head field)

ySize :: [String] -> Int
ySize = length

-- day 04 part 1: search for XMAS in all orientation in the field
day04part1 :: [String] -> Int
day04part1 field = length
    $ concatMap (\x -> concatMap (\y -> filter (xmasAt field . map (bimap ((+) x) ((+) y))) xmasOrientations)
        [0..ySize field - 1]) [0..xSize field - 1]

-- day 04 part 2: search for an X made of two times the word MAS with the A shared
day04part2 :: [String] -> Int
day04part2 field = length
    $ concatMap (\x -> filter (xmas2At field x)
        [0..ySize field - 1]) [0..xSize field - 1]