import Text.Printf (printf)
import Aoc
import Debug.Trace (trace)
data Location = Location Int Int deriving (Eq,Show)

data Direction4 = North | East | South | West
    deriving (Enum, Show, Eq)

direction4location :: Direction4 -> Location
direction4location North = Location 0 (-1)
direction4location East  = Location 1 0
direction4location South = Location 0 1
direction4location West  = Location (-1) 0

instance Num Location where
    (Location x1 y1) + (Location x2 y2) = Location (x1 + x2) (y1 + y2)
    (Location x1 y1) - (Location x2 y2) = Location (x1 - x2) (y1 - y2)
    (Location x1 y1) * (Location x2 y2) = Location (x1 * x2) (y1 * y2)
    abs (Location x y) = Location (abs x) (abs y)
    signum (Location x y) = Location (signum x) (signum y)
    fromInteger x = Location (fromInteger x) (fromInteger x)

(+|) :: Location -> Direction4 -> Location
(+|) loc dir = loc + direction4location dir

data LocatedChar = LocatedChar Location Char deriving (Eq, Show)

newtype CharMaze = CharMaze [[LocatedChar]] deriving (Eq, Show)

parseCharMazeLine :: Int -> String -> [LocatedChar]
parseCharMazeLine y = zipWith (\ x c -> LocatedChar (Location x y) c) [0..]

parseCharMaze :: [String] -> CharMaze
parseCharMaze = CharMaze . zipWith parseCharMazeLine [0..]

showLocatedChar :: [Location] -> LocatedChar -> String
showLocatedChar visited (LocatedChar loc c) = if loc `elem` visited then "\ESC[31m" ++ show c ++ "\ESC[0m" else [c]

showCharMaze :: CharMaze -> [Location] -> String
showCharMaze (CharMaze xss) visited = joinToString "\n" $ map (joinToString "" . map (showLocatedChar visited)) xss

part1_example = do
    part1 143 "day06/example.txt" day06part1

day06part1 :: [String] -> Integer
day06part1 field = trace (showCharMaze maze []) 0
    where maze = parseCharMaze field