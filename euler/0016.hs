
example = sum $ map (\c -> read [c]) $ show $ 2 ^ 15


-- having arbitrary sized integers makes this trivial
result = sum $ map (\c -> read [c]) $ show $ 2 ^ 1000

expected = result == 1366