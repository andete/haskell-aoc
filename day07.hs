import Aoc
import Debug.Trace (trace)

part1_example = do
    part1 3749 "day07/example.txt" day07part1


parse:: String -> (Integer, [Integer])
parse s =
    let result = read $ take (length first - 1) first
        values = map read (tail w)
        in (result, values)
    where w = words s
          first = head w

canMake :: Integer -> [Integer] -> Bool
canMake _ [] = False
canMake a [b] = a == b 
canMake a (b:_) | b >= a = False
canMake a (b:c:bs) = canMake a ((b * c) : bs) || canMake a ((b + c) : bs)

day07part1 :: [String] -> Integer
day07part1 field = sum $ map fst $ filter (uncurry canMake) parsed
    where parsed = map parse field