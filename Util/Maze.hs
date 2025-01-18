module Util.Maze(Maze(..), Located(..), parse, showMaze, findAll, at, at', neighbours, neighboursDir, neighbours', neighbours'dir, items) where

import qualified Data.Vector as V
import Util.Location (Location (..))
import qualified Util.Direction4 as Direction4

import Util.Aoc
import Data.Maybe (mapMaybe, fromJust)
import Util.Located

newtype Maze a = Maze (V.Vector (V.Vector (Located a)))

parseMazeLine :: (Char -> a) -> Int -> String -> V.Vector (Located a)
parseMazeLine make y line = V.fromList $ zipWith (\ x c -> Located (Location x y) (make c)) [0..] line

parse :: (Char -> a) -> [String] -> Maze a
parse make input = Maze . V.fromList $ zipWith (parseMazeLine make) [0..] input

showLocated :: (a -> String) -> [Location] -> Located a -> String
showLocated showing visited (Located loc c) = if loc `elem` visited then "\ESC[31m" ++ showing c ++ "\ESC[0m" else showing c

showMaze :: (a -> String) -> Maze a -> [Location] -> String
showMaze showing maze visited = joinToString "\n" $ map (joinToString "" . map (showLocated showing visited)) (mazeToLists maze)

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

at' :: Maze a -> Location -> Located (Maybe a)
at' maze loc = case at maze loc of
    Just a -> Located loc (Just (value a))
    Nothing -> Located loc Nothing

neighbours :: Maze a -> Location -> [Located a]
neighbours maze loc = mapMaybe (at maze . (loc Direction4.+|)) Direction4.all

frit :: (Located (Maybe a), Direction4.Direction4) -> Maybe (Located a, Direction4.Direction4)
frit (Located loc a, dir) = case a of
    Just a -> Just (Located loc a, dir)
    Nothing -> Nothing

neighboursDir :: Maze a -> Location -> [(Located a, Direction4.Direction4)]
neighboursDir maze loc = mapMaybe (frit . (\dir -> (at' maze (loc Direction4.+| dir), dir))) Direction4.all

neighbours' :: Maze a -> Location -> [Located (Maybe a)]
neighbours' maze loc = map (at' maze . (loc Direction4.+|)) Direction4.all

neighbours'dir :: Maze a -> Location -> [(Located (Maybe a), Direction4.Direction4)]
neighbours'dir maze loc = map (\dir -> (at' maze (loc Direction4.+| dir), dir)) Direction4.all

xIndices :: Maze a -> [Int]
xIndices (Maze v) = [0..lx-1]
    where lx = if V.length v == 0 then 0 else V.length (V.head v)

yIndices :: Maze a -> [Int]
yIndices (Maze v) = [0..ly-1]
    where ly = V.length v

items :: Maze a -> [Located a]
items = concat . mazeToLists