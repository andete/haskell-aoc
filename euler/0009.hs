
result = head [a*b*c | a <- [1..1000], b <- [(a+1)..1000], let x = a^2 + b^2, let c = 1000 - a - b, x == c^2]

expected = result == 31875000

