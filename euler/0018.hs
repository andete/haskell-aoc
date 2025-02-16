import Util.Euler (runLines)

parse :: [String] -> [[Int]]
parse = map (map read . words)

maxPath :: [[Int]] -> Int
maxPath a = head $ foldr1 f a
    where f row res = zipWith3 (\row1 res1 tRes1 -> row1 + max res1 tRes1) row res (tail res)
    -- replace the row by the sum of the row and the maximum of the two adjacent numbers in the result from the lower rows
    -- this means we're building the result from the bottom up

example = runLines 23 "euler/input/0018ex.txt" $ maxPath . parse

result = runLines 1074 "euler/input/0018.txt" $ maxPath . parse
