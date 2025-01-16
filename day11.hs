import Aoc
import Data.List (iterate', sort)
import qualified Data.HashMap.Internal.Strict as H
import Data.Maybe (fromMaybe)

type Stone = Integer
type Stones = H.HashMap Integer Integer


blinkOne' :: Stones -> Stones -> Stone -> Stones
blinkOne' existingStones newStones stone = foldl (\st b -> increase st b amount) newStones blinked
    where amount = fromMaybe 0 $ H.lookup stone existingStones
          blinked = blinkOne stone

increase :: Stones -> Stone -> Integer -> Stones
increase stones stone amount = H.insert stone (existing + amount) stones
    where existing = fromMaybe 0 $ H.lookup stone stones

blinkOne :: Stone -> [Stone]
blinkOne 0 = [1]
blinkOne i
    | even l = let (a,b) = splitAt (l `div` 2) s in [read a, read b]
    | otherwise = [i* 2024]
        where s = show i
              l = length s

blinkAll :: [Stone] -> [Stone]
blinkAll = concatMap blinkOne

blinkAll2 :: Stones -> Stones
blinkAll2 xn = foldl (blinkOne' xn) H.empty (H.keys xn)

part1_example = do
    part1 7 "day11/example.txt" (day11part1 1)


part1_example2 = do
    part1 22 "day11/example2.txt" (day11part1 6)

part1_input = do
    part1 183248 "day11/input.txt" (day11part1 25)

part2_example = do
    part2 7 "day11/example.txt" (day11part2 1)

part2_example2 = do
    part2 22 "day11/example2.txt" (day11part2 6)

part2_input25 = do
    part2 183248 "day11/input.txt" (day11part2 25)

part2_input75 = do
    part2 218811774248729 "day11/input.txt" (day11part2 75)

day11part1 :: Int -> [String] -> Int
day11part1 times field = length $ last $ take (times+1) $ iterate blinkAll rocks
    where rocks :: [Integer] = map read $ words $ head field

day11part2 :: Int -> [String] -> Integer
day11part2 times field = sum $ H.elems $ last $ take (times+1) $ iterate blinkAll2 rocks
    where rocks = H.fromList $ map ((, 1) . read) (words $ head field)

main = do part2_input75