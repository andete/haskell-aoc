module Util.Aoc(readLines, listToNums, part, part1, part2, joinToString, repeatUntil, runAoc) where

import qualified Data.Vector as V
import System.Environment (getArgs)


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

repeatUntil :: (a -> Bool) -> (a -> a) -> a -> a
repeatUntil p f x
    | p x       = x
    | otherwise = repeatUntil p f (f x)


runAoc :: [(String, IO ())] -> IO ()
runAoc commands = getArgs >>= \args -> case args of
    [] -> usage
    (command:_) -> case lookup command commands of
        Just action -> action
        Nothing -> usage
  where usage = putStrLn $ "Usage: run " ++  joinToString " | " (map fst commands)
