import Util.Math (properDivisors)
import Data.List (foldl')
import qualified Data.HashSet as H
import qualified Data.Vector as V

isAbundant :: Int -> Bool
isAbundant n = n < sum (properDivisors n)

abundants :: V.Vector Int
abundants = V.fromList $ filter isAbundant [1..28123]

abundantSums :: H.HashSet Int
abundantSums = foldl' (\s a -> 
    foldl' (\s' b -> if abundants V.! a + abundants V.! b  <= 28123 
        then H.insert (abundants V.! a + abundants V.! b) s' 
        else s') s [a..length abundants - 1]) H.empty [0..length abundants - 1]


nonAbundantSums = filter (`notElem` abundantSums) [1..28123]

result = sum nonAbundantSums

expected = result == 4179871