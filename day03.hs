import Aoc
import Data.Char (isDigit)

part1_example = do
    part1 161 "day03/example.txt" day03part1

part1_input = do
    part1 160672468 "day03/input.txt" day03part1


data Token = Mul | Number Integer | Comma | Close | Other
    deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize ('m':'u':'l':'(':xn) = Mul : tokenize xn
tokenize (x:xn) | isDigit x = tokenizeNumber x xn
tokenize (',':xn) = Comma : tokenize xn
tokenize (')':xn) = Close : tokenize xn
tokenize (_:xn) = Other : tokenize xn
tokenize [] = []

tokenizeNumber :: Char -> String -> [Token]
tokenizeNumber c xn = Number (read (c : takeWhile isDigit xn)) : tokenize (dropWhile isDigit xn)

findValidMuls :: [Token] -> [(Integer, Integer)]
findValidMuls (Mul : Number x : Comma : Number y : Close : xn) = (x, y) : findValidMuls xn
findValidMuls (_:xs) = findValidMuls xs
findValidMuls [] = []

allValidMuls :: [String] -> [(Integer, Integer)]
allValidMuls xs = findValidMuls (tokenize (concat xs))

day03part1 :: [String] -> Integer
day03part1 xs = sum $ map (uncurry (*)) (allValidMuls xs)