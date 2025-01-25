import qualified Data.Vector as V
import Data.List.Split (splitOn)
import qualified Control.Lens.Internal.Deque as Vector
import Data.Bits (Bits(xor))


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

data Computer = Computer {
    memory :: V.Vector Int,
    output :: [Int],
    pc :: Int,
    a :: Integer,
    b :: Integer,
    c :: Integer
    } deriving (Show)

parse :: [String] -> Computer
parse xs = Computer memory [] 0 a b c
    where a = read $ words (head xs) !! 2
          b = read $ words (xs !! 1) !! 2
          c = read $ words (xs !! 2) !! 2
          memory = V.fromList $ map read $ splitOn "," $ words (xs !! 4) !! 1

cycle :: Computer -> Maybe Computer
cycle computer = if pc computer >= V.length (memory computer) then Nothing else Just (cycle' computer)

cycle' :: Computer -> Computer
cycle' computer = case instr of
    Adv -> computer { pc = pc', a = a computer `div` (2 ^ combo operand computer) }
    Bxl -> computer { pc = pc', b = b computer `xor` toInteger operand }
    Bst -> computer { pc = pc', b = combo operand computer `mod` 8 }
    Jnz -> computer { pc = if a computer == 0 then pc' else operand }
    Bxc -> computer { pc = pc', b = b computer `xor` c computer }
    Out -> computer { pc = pc', output = fromInteger (combo operand computer `mod` 8) : output computer }
    Bdv -> computer { pc = pc', b = a computer `div` (2 ^ combo operand computer) }
    Cdv -> computer { pc = pc', c = a computer `div` (2 ^ combo operand computer) }
    where instr = instruction $ memory computer V.! pc computer
          operand = memory computer V.! (pc computer + 1)
          pc' = pc computer + 2

combo :: Int-> Computer -> Integer
combo operand computer = case operand of
              0 -> 0
              1 -> 1
              2 -> 2
              3 -> 3
              4 -> a computer
              5 -> b computer
              6 -> c computer
              7 -> error "reserved"