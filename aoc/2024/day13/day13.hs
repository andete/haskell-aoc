import Debug.Trace (trace)
import Util.Aoc
import Data.Maybe (mapMaybe)
-- TODO

data Button = Button Integer Integer deriving (Show, Eq)
data ClawMachine = ClawMachine Button Button Integer Integer deriving (Show, Eq)

-- Button A: X+94, Y+34
parseButton :: String -> Button
parseButton s = Button x y
    where w = words s
          x :: Integer = let z = w !! 2 in read (drop 2 $ take (length z - 1) z)
          y :: Integer = read (drop 2 (w !! 3))

-- Prize: X=8400, Y=5400
parsePrize :: String -> (Integer, Integer)
parsePrize s = (x, y)
    where w = words s
          x :: Integer = let z = w !! 1 in read (drop 2 $ take (length z - 1) (w !! 1))
          y :: Integer = read (drop 2 (w !! 2))

parse :: Integer -> [String] -> [ClawMachine]
parse extra (a:b:c:d:d') = machine : parse extra d'
    where (e, f) = parsePrize c
          machine = ClawMachine (parseButton a) (parseButton b) (e + extra) (f + extra)
parse _ _ = []

--  equation solved via wolfram alpha: a * x1 + b * x2 = x3 and a * y1 + b * y2 = y3 solve for a and b
-- a = (x3 y2 - x2 y3)/(x1 y2 - x2 y1)
-- b = (x3 y1 - x1 y3)/(x2 y1 - x1 y2) and x2 y1!=x1 y2 and x2!=0
solve :: ClawMachine -> Maybe (Integer, Integer)
solve (ClawMachine (Button x1 y1) (Button x2 y2) x3 y3) = 
    if a1 `mod` a2 /= 0 || b1 `mod` b2 /= 0 then Nothing else Just (a, b)
    where a1 = x3 * y2 - x2 * y3
          a2 = x1 * y2 - x2 * y1
          a = a1 `div` a2
          b1 = x3 * y1 - x1 * y3 
          b2 = x2 * y1 - x1 * y2
          b = b1 `div` b2

part1_example = do
    part1 480 "2024/day13/example.txt" day13part1
part1_input = do
    part1 28059 "2024/day13/input.txt" day13part1

part2_example = do
    part2 875318608908 "2024/day13/example.txt" day13part2
part2_input = do
    part2 102255878088512 "2024/day13/input.txt" day13part2

day13part1 :: [String] -> Integer
day13part1 s = sum $ map (\(a,b) -> a * 3 + b) $ mapMaybe solve machines
    where machines = parse 0 s

day13part2 :: [String] -> Integer
day13part2 s = sum $ map (\(a,b) -> a * 3 + b) $ mapMaybe solve machines
    where machines = parse 10000000000000 s