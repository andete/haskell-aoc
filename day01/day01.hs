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

listToNumbers :: [String] -> [Integer]
listToNumbers = map read

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

example = do
    contents <- readLines "example.txt"
    let numbers = map (listToNumbers . words) contents
        leftNumbers = sort (map head numbers)
        rightNumbers = sort (map last numbers)
        result = sum (zipWith (\x y -> abs (x - y)) leftNumbers rightNumbers)
    putStr (show result)

example2 = do
    part1 11 "example.txt" day01part01

example2' = do
    part1 11 "example.txt" day01part01'

day01part01 :: [String] -> Integer
day01part01 xs = sum $ zipWith (\x y -> abs (x - y)) (sort $ map (head . listToNumbers . words) xs) (sort $ map (last . listToNumbers . words) xs)


day01part01' :: [String] -> Integer
day01part01' xs = sum $ zipWith (\x y -> abs (x - y)) (convert head) (convert last)
    where convert taker = sort $ map (taker . listToNumbers . words) xs