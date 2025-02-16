import Util.Math

d n = sum $ properDivisors n

amicable a b = a /= b && a == d b && b == d a

amicablePair a = let b = d a in a /= b && a == d b

result = sum $ filter amicablePair [1..10000]

expected = result == 31626