
fact n = product [1..n]

result = sum $ map (\c -> read [c]) $ show $ fact 100

expected = result == 648