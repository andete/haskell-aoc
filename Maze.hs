module Maze(Maze(..), Located(..), parse, showMaze, findAll, at, neighbours) where

import qualified Data.Vector as V
import Location (Location (..))
import qualified Direction4

import Aoc
import Data.Maybe (mapMaybe)
import Located

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

findAll :: (Eq a) => Maze a -> a -> [Located a]
findAll maze c = concatMap (filter (\(Located _ c') -> c' == c)) (mazeToLists maze)

at :: Maze a -> Location -> Maybe (Located a)
at (Maze v) (Location x y)
    | x < 0 || x >= lx = Nothing
    | y < 0 || y >= ly = Nothing
    | otherwise = Just $ (v V.! y) V.! x
    where ly = V.length v
          lx = if ly == 0 then 0 else V.length (V.head v)

neighbours :: Maze a -> Location -> [Located a]
neighbours maze loc = mapMaybe (at maze . (loc Direction4.+|)) Direction4.all