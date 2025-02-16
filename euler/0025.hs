import Data.List (find)
fibonacci = 1 : 2 : zipWith (+) fibonacci (tail fibonacci)

example = find (\(i,j) -> length (show i) == 3) $ zip fibonacci [2..]

result = fmap snd $ find (\(i,j) -> length (show i) == 1000) $ zip fibonacci [2..]

expected = result == Just 4782