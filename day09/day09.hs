import Data.Maybe (isJust, isNothing)
import Debug.Trace (trace)
import Util.Aoc
import Data.List (nub, elemIndex, find)

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

data Segment = Segment { pointer :: Int, size :: Int, fileHandle :: Maybe Int } deriving (Eq)

instance Show Segment where
    show (Segment p s f) = "@" ++ show p ++ "+" ++ show s ++ case f of
        Just x -> "F" ++ show x
        Nothing -> "E"

isFile :: Segment -> Bool
isFile (Segment _ _ t) = isJust t

isEmpty :: Segment -> Bool
isEmpty = not . isFile

makeSegmentMap :: String -> [Segment]
makeSegmentMap input =
    concatMap (filter (\x -> size x > 0) . (\(p, i, n) ->
         if even i then [Segment p n (Just (i `div` 2))] else [Segment p n Nothing])) z 
    where indices = [0.. (length input - 1)]
          numbers = map (read . (:[])) input
          pointers = reverse $ fst $ foldl (\(acc, p) x -> (p:acc, p+x) ) ([],0) numbers
          z = zip3 pointers indices numbers


-- swap two segments, assuming they are equal in size; swap them, keep pointers though
swapSegment :: [Segment] -> Segment -> Segment -> [Segment]
swapSegment xs s1 s2 = simplifySegmentMap $ map (\x ->
    if x == s1 then s2 { pointer = pointer s1} else if x == s2 then s1 { pointer = pointer s2 } else x) xs

-- take any subsequent Empty segments and merge them into one
simplifySegmentMap :: [Segment] -> [Segment]
simplifySegmentMap [] = []
simplifySegmentMap ((Segment p s1 Nothing):(Segment _ s2 Nothing):xs) = simplifySegmentMap $ Segment p (s1 + s2) Nothing:xs
simplifySegmentMap (x:xs) = x:simplifySegmentMap xs

tr :: Show b => String -> b -> a -> a
tr prefix b = trace (prefix ++ ": " ++ show b)

-- swap two segments, assuming the Empty one (which is first) is bigger then the File one
-- replace the Empty one with the File + remaining Empty and create a new Empty to replace the File
swapSegment2 :: [Segment] -> Segment -> Segment -> [Segment]
swapSegment2 xs e s =
    simplifySegmentMap $ concatMap (\x ->
        if x == e then 
            [Segment (pointer e) (size s) (fileHandle s), Segment (pointer e + size s) (size e - size s) Nothing] else 
                if x == s then 
                    [Segment (pointer s) (size s) Nothing]
                else [x]) xs


-- try to move a segment the most to the left as possible
moveSegment :: [Segment] -> Segment -> [Segment]
moveSegment xs s = case maybeEmptyForSegment of
    Just e -> let es = size e in
        if es == ss then
            -- replace empty segment with file segment and file segment with empty segment
            swapSegment xs e s
        else if es > ss then
            -- replace empty segment with file segment + remaining empty segment and file segment with empty segment
            swapSegment2 xs e s
        else xs -- not possible 
    Nothing -> xs
    where ss = size s
          maybeEmptyForSegment = find (\x -> isEmpty x && size x >= ss && pointer x < pointer s) xs

compactSegmentMap:: [Segment] -> [Segment]
compactSegmentMap xs =
    foldl moveSegment xs files
    where files = reverse $ filter isFile xs

segmentMapToMemoryMap :: [Segment] -> [Maybe Int]
segmentMapToMemoryMap [] = []
segmentMapToMemoryMap (x:xs) = case x of
    Segment _ size (Just f) -> replicate size (Just f) ++ segmentMapToMemoryMap xs
    Segment _ size Nothing -> replicate size Nothing ++ segmentMapToMemoryMap xs

showMemoryMap :: [Maybe Int] -> String
showMemoryMap [] = ""
showMemoryMap (x:xs) = case x of
    Just y -> show y ++ showMemoryMap xs
    Nothing -> "." ++ showMemoryMap xs

day09part2 :: [String] -> Integer
day09part2 field = checkSum $ segmentMapToMemoryMap $ compactSegmentMap $ makeSegmentMap $ head field

main = do part2_input