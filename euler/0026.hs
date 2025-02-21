
import Data.List (elemIndex)
import Data.Maybe (fromJust)

-- finite or infinite fraction of 1/n
fractions n = fractions' 1 n
    where fractions' i n
            | i == 0 = []
            | i >= n = i `div` n : fractions' ((i `mod` n)*10) n
            | i * 10 < n = 0 : fractions' (i * 10) n
            | otherwise = 0 : fractions' (i * 10) n

repFractions n = 1 + fractions' [] 1 n
    where fractions' seen i n
            | i == 0 = -1 -- if we reach 0, the fraction is finite
            | i < n = fractions' seen (i*10) n -- skip leading zeros
            | i `elem` seen = fromJust $ elemIndex i seen -- if we've seen the digit before, we've found the cycle
            | otherwise = fractions' (i:seen) (i `mod` n) n

result = snd $ maximum $ map (\x -> (repFractions x, x)) [1..999]

expected = result == 983

-- somehow I think a cooler approach is possible

