import qualified Data.Vector as V
import Data.List.Split (splitOn)
import qualified Control.Lens.Internal.Deque as Vector
import Data.Bits (Bits(xor))
import qualified Util.Aoc as Util
import Util.Aoc
import Debug.Trace (trace)
import Control.Lens (op, re)
import GHC.Generics (C)
import Data.Maybe (fromJust)
import Data.List (nub)


data Instruction = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv deriving (Show, Eq)

instruction:: Int -> Instruction
instruction 0 = Adv
instruction 1 = Bxl
instruction 2 = Bst
instruction 3 = Jnz
instruction 4 = Bxc
instruction 5 = Out
instruction 6 = Bdv
instruction 7 = Cdv

data Operand = Ignore Int | Direct Int | Literal Int | A | B | C

instance Show Operand where
    show (Ignore a) = "I" ++ show a
    show (Direct a) = show a
    show (Literal a) = show a
    show A = "A"
    show B = "B"
    show C = "C"

direct :: Operand -> Int
direct (Direct a) = a

comboOperand :: Int -> Operand
comboOperand 0 = Literal 0
comboOperand 1 = Literal 1
comboOperand 2 = Literal 2
comboOperand 3 = Literal 3
comboOperand 4 = A
comboOperand 5 = B
comboOperand 6 = C

operand :: Instruction -> Int -> Operand
operand Adv = comboOperand
operand Bxl = Direct
operand Bst = comboOperand
operand Jnz = Direct
operand Bxc = Ignore
operand Out = comboOperand
operand Bdv = comboOperand
operand Cdv = comboOperand

data FullInstruction = FullInstruction Instruction Operand

instance Show FullInstruction where
    show (FullInstruction instr op) = show instr ++ ":" ++ show op

describe :: FullInstruction -> String
describe (FullInstruction Adv op) = "A = A / (2 ^ "  ++ show op ++ ")"
describe (FullInstruction Bxl op) = "B = B xor " ++ show op
describe (FullInstruction Bst op) = "B = " ++ show op ++ " % 8"
describe (FullInstruction Jnz (Direct a)) = "if A != 0 then pc = " ++ show a
describe (FullInstruction Bxc op) = "B = B xor C"
describe (FullInstruction Out op) = "output = " ++ show op ++ " % 8"
describe (FullInstruction Bdv op) = "B = A / (2 ^ " ++ show op ++ ")"
describe (FullInstruction Cdv op) = "C = A / (2 ^ " ++ show op ++ ")"
describe (FullInstruction instr op) = error ("non-described instruction" ++ show instr ++ " " ++ show op)

data Computer = Computer {
    memory :: V.Vector Int,
    output :: [Int],
    pc :: Int,
    a :: Integer,
    b :: Integer,
    c :: Integer,
    halted :: Bool
    } deriving (Show)

parse :: [String] -> Computer
parse xs = Computer memory [] 0 a b c False
    where a = read $ words (head xs) !! 2
          b = read $ words (xs !! 1) !! 2
          c = read $ words (xs !! 2) !! 2
          memory = V.fromList $ map read $ splitOn "," $ words (xs !! 4) !! 1

cycleOne :: Computer -> Computer
cycleOne computer = if pc computer >= V.length (memory computer) || halted computer
    then computer { halted = True } else cycle' computer

cycle' :: Computer -> Computer
cycle' computer = trace (show (pc computer) ++ " " ++ describe full) $ case instr of
    Adv -> computer { pc = pc', a = a computer `div` (2 ^ combo op computer) }
    Bxl -> computer { pc = pc', b = b computer `xor` toInteger (direct op) }
    Bst -> computer { pc = pc', b = combo op computer `mod` 8 }
    Jnz -> computer { pc = if a computer == 0 then pc' else direct op }
    Bxc -> computer { pc = pc', b = b computer `xor` c computer }
    Out -> computer { pc = pc', output = fromInteger (combo op computer `mod` 8) : output computer }
    Bdv -> computer { pc = pc', b = a computer `div` (2 ^ combo op computer) }
    Cdv -> computer { pc = pc', c = a computer `div` (2 ^ combo op computer) }
    where instr = instruction $ memory computer V.! pc computer
          op = operand instr $ memory computer V.! (pc computer + 1)
          full = FullInstruction instr op
          pc' = pc computer + 2

result :: Computer -> String
result computer = Util.joinToString "," $ map show (reverse $ output computer)

combo :: Operand -> Computer -> Integer
combo operand computer = case operand of
              Literal a -> toInteger a
              A -> a computer
              B -> b computer
              C -> c computer
              otherwise -> error ("Invalid operand" ++ show otherwise)


part1_example = do
    part1 "4,6,3,5,6,3,5,2,1,0" "2024/day17/example.txt" day17part1

part1_input = do
    part1 "7,0,7,3,4,1,3,0,1" "2024/day17/input.txt" day17part1

part1_inputTest = do
    part1 "2,4,1,4,7,5,4,1,1,4,5,5,0,3,3,0" "2024/day17/inputTest.txt" day17part1

part1_example2a = do
    part1 "0,3,5,4,3,0" "2024/day17/example2a.txt" day17part1

part1_example2af = do
    part1 "0,3,5,4,3,0" "2024/day17/example2af.txt" day17part1

-- div 8 is done at the start of the loop so needs to be part of the step
part2_example2a = do
    part2 117440 "2024/day17/example2a.txt" (day17part2 (\a -> Just $ (a `div` 8) `mod` 8))

part2_input = do
    part2 7 "2024/day17/input.txt" (day17part2 step)

run :: Computer -> Computer
run = repeatUntil halted cycleOne

day17part1 :: [String] -> String
day17part1 input = result $ repeatUntil halted cycleOne computer
    where computer = parse input

day17part2 :: (Integer -> Maybe Integer) -> [String] -> Integer
day17part2 step input = trace (show program) $ find2 step program
    where computer = parse input
          program = V.toList $ memory computer

-- A transformation based on analysis of the program
-- 0 B = A % 8
-- 2 B = B xor 4
-- 4 C = A / (2 ^ B)
-- 6 B = B xor C
-- 8 B = B xor 4
-- 10 output = B % 8
step :: Integer -> Maybe Integer
step 0 = Nothing
step a = Just out
    where b = (a `mod` 8) `xor` 4
          c = a `div` (2 ^ b)
          b' = (b `xor` c) `xor` 4
          out = b' `mod` 8


showOctal :: Integer -> String
showOctal a 
 | a > 7 = showOctal (a `div` 8) ++ show (a `mod` 8)
 | otherwise = show a

find2 :: (Integer -> Maybe Integer) -> [Int] -> Integer
find2 step program = trace (show $ map showOctal q) m
  where
      p = map toInteger $ reverse program
      q = foldl (\as expectedInstruction ->
        --trace (show (map showOctal as) ++ " " ++ show expectedInstruction) $
        concatMap (\a -> let sa = a * 8 in
                filter (\c -> step c == Just expectedInstruction) [sa..sa+7]
            ) as
        ) [0] p
      m = minimum q


-- what does the input program do ?
-- it outputs a number every time it loops
-- it takes the least significant 3 bits off A every loop: 
--     0  | B = A % 8
--     12 | A = A / (2 ^ 3)
-- a value is calculated based on those 3 bits
-- eventually A will be 0 (no bits left) and then the program will halt
-- as we require 16 outputs, we require A to be 16*3 = 48 bits long
