module Aoc(readLines, listToNums, part, part1, part2, joinToString) where

import qualified Data.Vector as V


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
            putStrLn $ "Part " ++ show number ++ " Pass " ++ show result
        else
            putStrLn $ "Part " ++ show number ++ " Fail, expected " ++ show expected ++ " but got " ++ show result


part1 :: (Eq a, Show a) => a -> FilePath -> ([String] -> a) -> IO ()
part1 = part 1

part2 :: (Eq a, Show a) => a -> FilePath -> ([String] -> a) -> IO ()
part2 = part 2

joinToString :: String -> [String] -> String
joinToString separator = foldr1 (\x acc -> x ++ separator ++ acc)