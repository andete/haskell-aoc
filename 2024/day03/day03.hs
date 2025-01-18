import Util.Aoc
import Data.Char (isDigit)

part1_example = do
    part1 161 "2024/day03/example.txt" day03part1

part1_input = do
    part1 160672468 "2024/day03/input.txt" day03part1



part2_example = do
    part1 48 "2024/day03/example2.txt" day03part2

part2_input = do
    part1 84893551 "2024/day03/input.txt" day03part2

data Token = Mul | Number Integer | Comma | Close | Other | Do | Dont
    deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize ('m':'u':'l':'(':xn) = Mul : tokenize xn
tokenize ('d':'o':'(':')':xn) = Do : tokenize xn
tokenize ('d':'o':'n':'\'':'t':'(':')':xn) = Dont : tokenize xn
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

day03part1 :: [String] -> Integer
day03part1 xs = sum $ map (uncurry (*)) (findValidMuls (tokenize (concat xs)))

findValidMuls2 :: Bool -> [Token] -> [(Integer, Integer)]
findValidMuls2 True (Mul : Number x : Comma : Number y : Close : xn) = (x, y) : findValidMuls2 True xn
findValidMuls2 _ (Do : xn) = findValidMuls2 True xn
findValidMuls2 _ (Dont : xn) = findValidMuls2 False xn
findValidMuls2 a (_:xs) = findValidMuls2 a xs
findValidMuls2 _ [] = []

day03part2 :: [String] -> Integer
day03part2 xs = sum $ map (uncurry (*)) (findValidMuls2 True (tokenize (concat xs)))