toWords:: Int -> String
toWords n
    | n == 1 = "one"
    | n == 2 = "two"
    | n == 3 = "three"
    | n == 4 = "four"
    | n == 5 = "five"
    | n == 6 = "six"
    | n == 7 = "seven"
    | n == 8 = "eight"
    | n == 9 = "nine"
    | n == 10 = "ten"
    | n == 11 = "eleven"
    | n == 12 = "twelve"
    | n == 13 = "thirteen"
    | n == 14 = "fourteen"
    | n == 15 = "fifteen"
    | n == 16 = "sixteen"
    | n == 17 = "seventeen"
    | n == 18 = "eighteen"
    | n == 19 = "nineteen"
    | n == 20 = "twenty"
    | n == 30 = "thirty"
    | n == 40 = "forty"
    | n == 50 = "fifty"
    | n == 60 = "sixty"
    | n == 70 = "seventy"
    | n == 80 = "eighty"
    | n == 90 = "ninety"
    | n == 1000 = "one thousand"
    | n < 100 = toWords (n - (n `mod` 10)) ++ " " ++ toWords (n `mod` 10)
    | n `mod` 100 == 0 = toWords (n `div` 100) ++ " hundred"
    | otherwise = toWords (n `div` 100) ++ " hundred and " ++ toWords (n `mod` 100)

letters :: String -> Int
letters = length . filter (\c -> c /= ' ' && c /= '-')

result = sum $ map (letters . toWords) [1..1000]

expected = result == 21124