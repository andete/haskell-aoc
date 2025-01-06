import System.IO
import Language.Haskell.TH (listT)
import Data.List (sort)
import Control.Exception (assert)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

readLines2 :: FilePath -> IO [String]
readLines2 fileName = do
    contents <- readFile fileName
    return $ lines contents

listToNums :: (Num a, Read a) => [String] -> [a]
listToNums = map read

part :: (Eq a, Show a) => Int -> a -> FilePath -> ([String] -> a) -> IO ()
part number expected filename op = do
    contents <- readLines filename
    let result = op contents in
        if result == expected then
            putStrLn $ "Part " ++ show number ++ "Pass " ++ show result
        else
            putStrLn $ "Part " ++ show number ++ "Fail, expected " ++ show expected ++ " but got " ++ show result


part1 :: (Eq a, Show a) => a -> FilePath -> ([String] -> a) -> IO ()
part1 = part 1

part2 :: (Eq a, Show a) => a -> FilePath -> ([String] -> a) -> IO ()
part2 = part 2

part1_example = do
    part1 11 "example.txt" day01part1

part1_example' :: IO ()
part1_example' = do
    part1 11 "example.txt" day01part1'

part1_input = do
    part1 2057374 "input.txt" day01part1'

part2_example = do
    part2 31 "example.txt" day01part2

part2_input = do
    part2 23177084 "input.txt" day01part2

day01part1 :: [String] -> Integer
day01part1 xs = sum $ zipWith (\x y -> abs (x - y)) (sort $ map (head . listToNums . words) xs) (sort $ map (last . listToNums . words) xs)


day01part1' :: [String] -> Integer
day01part1' xs = sum $ zipWith (\x y -> abs (x - y)) (convert head) (convert last)
    where convert taker = sort $ map (taker . listToNums . words) xs


day01part2 :: [String] -> Int
day01part2 xs = sum $ map (\x -> x * length (filter (== x) lasts)) firsts
    where convert taker = sort $ map (taker . listToNums . words) xs
          firsts = convert head
          lasts = convert last