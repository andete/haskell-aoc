import Util.Euler (runLines)


calculate :: [String] -> Integer
calculate input = read . take 10 . show . sum $ map read input

result = runLines 5537376230 "euler/input/0013.txt" calculate