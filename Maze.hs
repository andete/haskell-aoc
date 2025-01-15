module Maze(Maze(..), Located(..), parse, showMaze) where

import qualified Data.Vector as V
import Location (Location (..))
import Aoc

data Located a = Located Location a deriving (Eq, Show)

newtype Maze a = Maze (V.Vector (V.Vector (Located a)))

parseMazeLine :: (Char -> a) -> Int -> String -> V.Vector (Located a)
parseMazeLine make y line = V.fromList $ zipWith (\ x c -> Located (Location x y) (make c)) [0..] line

parse :: (Char -> a) -> [String] -> Maze a
parse make input = Maze . V.fromList $ zipWith (parseMazeLine make) [0..] input

showLocated :: Show a => [Location] -> Located a -> String
showLocated visited (Located loc c) = if loc `elem` visited then "\ESC[31m" ++ Prelude.show c ++ "\ESC[0m" else Prelude.show c

showMaze :: Show a => Maze a -> [Location] -> String
showMaze maze visited = joinToString "\n" $ map (joinToString "" . map (showLocated visited)) (mazeToLists maze)

mazeToLists :: Maze a -> [[Located a]]
mazeToLists (Maze v) = V.toList $ fmap V.toList v