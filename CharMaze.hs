module CharMaze(CharMaze(..), parse, show) where

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

show :: CharMaze -> [Location] -> String
show (CharMaze xss) visited = joinToString "\n" $ map (joinToString "" . map (showLocatedChar visited)) xss

