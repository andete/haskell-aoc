import qualified Util.Location as Location
import Data.List.Split (splitOn)
import Util.Aoc
import Debug.Trace (trace)
import Data.Char (chr, ord)

type Location = Location.Location

data Robot = Robot Location Location

instance Show Robot where
    show (Robot (Location.Location x y) (Location.Location xv yv)) = "Robot@" ++ show x ++ "," ++ show y ++ "v" ++ show xv ++ "," ++ show yv

-- p=2,4 v=2,-3
parse :: String -> Robot
parse s = Robot location velocity
    where w = words s
          location = Location.fromList $ map read $ splitOn "," $ drop 2 $ head w
          velocity = Location.fromList $ map read $ splitOn "," $ drop 2 $ w !! 1

showRobotMaze :: Int -> Int -> [Robot] -> String
showRobotMaze wide tall robots = unlines $ map (\y -> map (\x ->
        let location = Location.fromList [x,y] in
        let count = length $ filter (== location) locations in
        if count > 0 then chr (ord '0' + count) else '.') [0..(wide-1)]) [0..(tall-1)]
    where locations = map (\(Robot l _) -> l) robots

move :: Int -> Int -> Robot -> Robot
move wide tall (Robot (Location.Location x y) (Location.Location xv yv)) =
    Robot (Location.Location ((x + xv) `mod` wide) ((y + yv) `mod` tall)) (Location.Location xv yv)

robotsInQuadrant :: [Robot] -> Int -> Int -> Int -> Int -> Integer
robotsInQuadrant robots xMin xMax yMin yMax = toInteger $ length $ filter (\(Robot (Location.Location x y) _) -> x >= xMin && x <= xMax && y >= yMin && y <= yMax) robots

part1_example = do
    part1 12 "2024/day14/example.txt" $ day14part1 11 7

part1_input = do
    part1 218295000 "2024/day14/input.txt" $ day14part1 101 103

part2_input = do
    part2 6870 "2024/day14/input.txt" $ day14part2 101 103

day14part1 :: Int -> Int -> [String] -> Integer
day14part1 wide tall s = q1 * q2 * q3 * q4
    where robots = map parse s
          robots100 = foldl (\r _ -> map (move wide tall) r) robots [0..99]
          q1 = robotsInQuadrant robots100 0 (wide `div` 2 - 1) 0 (tall `div` 2 - 1)
          q2 = robotsInQuadrant robots100 (wide `div` 2 + 1) (wide - 1) 0 (tall `div` 2 - 1)
          q3 = robotsInQuadrant robots100 0 (wide `div` 2 - 1) (tall `div` 2 + 1) (tall - 1)
          q4 = robotsInQuadrant robots100 (wide `div` 2 + 1) (wide - 1) (tall `div` 2 + 1) (tall - 1)

day14part2 :: Int -> Int -> [String] -> Integer
day14part2 wide tall s = trace (showRobotMaze wide tall robots6870) 6870
    where robots = map parse s
          robots6870 = iterate (map (move wide tall)) robots !! 6870
          