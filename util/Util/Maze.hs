module Util.Maze(Maze(..), Located(..), parse, showMaze, findAll, at, at', neighbours, neighboursDir, neighbours', neighbours'dir, items, set, around, neighboursAltOrder, parseItemList, width, height, neighbours8) where

import qualified Data.Vector as V
import Util.Location (Location (..))
import qualified Util.Direction4 as Direction4
import qualified Util.Direction8 as Direction8
import qualified Data.HashSet as HS
import Util.Aoc
import Data.Maybe (mapMaybe, fromJust)
import Util.Located
import Data.Hashable (Hashable)
import qualified GHC.Show as M
import Debug.Trace (trace)

newtype Maze a = Maze (V.Vector (V.Vector (Located a)))

parseMazeLine :: (Char -> a) -> Int -> String -> V.Vector (Located a)
parseMazeLine make y line = V.fromList $ zipWith (\ x c -> Located (Location x y) (make c)) [0..] line

parseMazeItemLine :: Int -> [a] -> V.Vector (Located a)
parseMazeItemLine y line = V.fromList $ zipWith (\ x c -> Located (Location x y) c) [0..] line

parseItemList :: (String -> [a]) -> [String] -> Maze a
parseItemList makeItems input = Maze . V.fromList $ zipWith (\y c -> parseMazeItemLine y (makeItems c)) [0..] input

parse :: (Char -> a) -> [String] -> Maze a
parse make input = Maze . V.fromList $ zipWith (parseMazeLine make) [0..] input

showLocated :: (a -> String) ->  HS.HashSet Location -> Located a -> String
showLocated showing visited (Located loc c) = if loc `elem` visited then "\ESC[31m" ++ showing c ++ "\ESC[0m" else showing c

showMaze :: (a -> String) -> Maze a -> HS.HashSet Location -> String
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

neighbours8 :: Maze a -> Location -> [Located a]
neighbours8 maze loc = mapMaybe (at maze . (loc Direction8.+|)) Direction8.all

neighboursAltOrder :: Maze a -> Location -> [Located a]
neighboursAltOrder maze loc = mapMaybe (at maze . (loc Direction4.+|)) Direction4.all'

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

width :: Maze a -> Int
width (Maze v) = if V.length v == 0 then 0 else V.length (V.head v)

height:: Maze a -> Int
height (Maze v) = V.length v

xIndices :: Maze a -> [Int]
xIndices (Maze v) = [0..lx-1]
    where lx = if V.length v == 0 then 0 else V.length (V.head v)

yIndices :: Maze a -> [Int]
yIndices (Maze v) = [0..ly-1]
    where ly = V.length v

items :: Maze a -> [Located a]
items = concat . mazeToLists

set :: a -> Location -> Maze a -> Maze a
set newC (Location x y) (Maze v)  = Maze $ v V.// [(y, (v V.! y) V.// [(x, Located (Location x y) newC)])]

around :: Maze a -> Int -> Location -> [Located a]
around maze n loc = mapMaybe (at maze) xys
    where xys = map ((+ loc) . uncurry Location) $ filter filt $ [(x,y) | x <- [-n..n], y <- [-n..n]]
          filt (x,y) = let a = abs x + abs y in a > 0 && a <= n

