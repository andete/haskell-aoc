import Data.Maybe (isJust, isNothing)
import Debug.Trace (trace)
import Aoc

part1_example = do
    part1 1928 "day09/example.txt" day09part1

part1_input = do
    part1 6216544403458 "day09/input.txt" day09part1

part2_example = do
    part2 2858 "day09/example.txt" day09part2

part2_input = do
    part2 6237075041489 "day09/input.txt" day09part2

makeMemoryMap :: String -> [Maybe Int]
makeMemoryMap input =
    concatMap (\i -> let x = read [input !! i] in
        if even i then map (const (Just (i `div` 2))) [0 .. x - 1] else map (const Nothing) [0 .. x - 1]
        ) indices
    where indices = [0.. (length input - 1)]

-- to is always to the left of from
move :: [Maybe Int] -> Int -> Int -> [Maybe Int]
move xs from to =
    before ++ [f] ++ between ++ [g] ++ after
    where f = xs !! from
          g = xs !! to
          before = take to xs
          between = take (from - to - 1) (drop (to + 1) xs)
          after = drop (from + 1) xs


compactMemoryMap:: [Maybe Int] -> [Maybe Int]
compactMemoryMap xs =
    let clearAmountFromEnd = fst $ foldl (\(acc, a) x ->
            case x of
            Just y -> if a > 0 then (Nothing:acc, a - 1) else (Just y:acc, a)
            Nothing -> (Nothing:acc, a)) ([], amountMoved) (reverse xs) in
        -- fill empty spots with values from 'fillWith'
        reverse $ fst $ foldl (\(acc, fw) x ->
            case x of
                Just y -> (x:acc, fw)
                Nothing -> if null fw then (Nothing:acc, fw) else (head fw:acc, tail fw))
                ([], fillWith) clearAmountFromEnd
    where indexed = zip [0..] xs
          openSlots = filter (isNothing . snd) indexed
          openSlotIndices = map fst openSlots
          filledSlots = reverse $ filter (isJust . snd) indexed
          filledSlotIndices = map fst filledSlots
          moveSlots = filter (uncurry (>)) $ zip filledSlotIndices openSlotIndices
          amountMoved = length moveSlots
          fillWith = map snd $ take amountMoved filledSlots

checkSum :: [Maybe Int] -> Integer
checkSum xs =
    sum $ zipWith (\ index ch
  -> (case ch of
        Just x -> fromIntegral x * fromIntegral index
        Nothing -> 0)) [0..] xs

day09part1 :: [String] -> Integer
day09part1 field = checkSum $ compactMemoryMap $ makeMemoryMap $ head field

day09part2 :: [String] -> Integer
day09part2 field = 0

main = do part1_input