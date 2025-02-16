module Util.Euler(readLines, runLines) where

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

runLines :: (Eq a, Show a) => a -> FilePath -> ([String] -> a) -> IO a
runLines expected filename op = do
    contents <- readLines filename
    let result = op contents in
        if result == expected then
            putStrLn ("Pass " ++ show result) >> return result
        else
            putStrLn ("Fail, expected " ++ show expected ++ " but got " ++ show result) >> return result
