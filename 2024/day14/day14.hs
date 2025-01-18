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
showRobotMaze w h robots = unlines $ map (\y -> map (\x -> 
        let location = Location.fromList [x,y] in
        let count = length $ filter (== location) locations in
        if count > 0 then chr (ord '0' + count) else '.') [0..(w-1)]) [0..(h-1)]
    where locations = map (\(Robot l _) -> l) robots

part1_example = do
    part1 480 "2024/day14/example.txt" day14part1

day14part1 :: [String] -> Integer
day14part1 s = trace (showRobotMaze 11 7 robots) 0
    where robots = map parse s