module Util.Math (
    properDivisors
) where

-- slow but works...
properDivisors :: Integral a => a -> [a]
properDivisors n = 1 : filter ((==0) . rem n) [2..n `div` 2]