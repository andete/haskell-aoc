
type Year = Int

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Show, Eq, Enum)

data Month = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Show, Eq, Enum)

type Date = (Year, Month, Int)

isLeapYear :: Year -> Bool
isLeapYear y = y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)

daysInMonth :: Year -> Month -> Int
daysInMonth y m = case m of
    January -> 31
    February -> if isLeapYear y then 29 else 28
    March -> 31
    April -> 30
    May -> 31
    June -> 30
    July -> 31
    August -> 31
    September -> 30
    October -> 31
    November -> 30
    December -> 31

daysInYear :: Year -> Int
daysInYear y = if isLeapYear y then 366 else 365

dayOf :: Date -> Day
dayOf (year, month, day) = toEnum (total `mod` 7)
    where months = if month == January then 0 else sum $ map (daysInMonth year) [January .. pred month]
          days = day - 1
          yearDays = sum $ map daysInYear [1900 .. pred year]
          total = yearDays + months + days

-- inefficient but still very fast...
result = length $ filter (== Sunday) $ [dayOf (year, month, 1) |
                                          year <- [1901 .. 2000], month <- [January .. December]]

expected = result == 171