module CharMaze(CharMaze(..), LocatedChar(..), parse, show, find, at, set) where

import Location (Location (..))
import Prelude hiding (show)
import Aoc (joinToString)

data LocatedChar = LocatedChar Location Char deriving (Eq, Show)

newtype CharMaze = CharMaze [[LocatedChar]] deriving (Eq, Show)

parseCharMazeLine :: Int -> String -> [LocatedChar]
parseCharMazeLine y = zipWith (\ x c -> LocatedChar (Location x y) c) [0..]

parse :: [String] -> CharMaze
parse = CharMaze . zipWith parseCharMazeLine [0..]

showLocatedChar :: [Location] -> LocatedChar -> String
showLocatedChar visited (LocatedChar loc c) = if loc `elem` visited then "\ESC[31m" ++ [c] ++ "\ESC[0m" else [c]

update :: Char -> LocatedChar -> LocatedChar
update c (LocatedChar loc _) = LocatedChar loc c

show :: CharMaze -> [Location] -> String
show (CharMaze xss) visited = joinToString "\n" $ map (joinToString "" . map (showLocatedChar visited)) xss

find :: CharMaze -> Char -> Location
find (CharMaze xss) c = head [ loc | LocatedChar loc c' <- concat xss, c == c' ]

at :: CharMaze -> Location -> Maybe LocatedChar
at (CharMaze xss) (Location x y)
    | x < 0 || x >= lx = Nothing
    | y < 0 || y >= ly = Nothing
    | otherwise = Just $ xss !! y !! x
    where ly = length xss
          lx = if ly == 0 then 0 else length (head xss)

updateIndexed :: (a -> a) -> Int -> [a] -> [a]
updateIndexed f i = zipWith (\j x -> if i == j then f x else x) [0..]

set :: CharMaze -> Location -> Char -> CharMaze
set (CharMaze xss) (Location x y) newC = CharMaze $ updateIndexed (updateIndexed (update newC) x) y xss