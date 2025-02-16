import Data.List (permutations, sort)


result :: Integer
result = read $ (sort . permutations) "0123456789" !! 999999

expected = result == 2783915460