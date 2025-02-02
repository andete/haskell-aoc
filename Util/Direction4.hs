module Util.Direction4(Direction4(..), (+|), toChar, rotate180, rotate90, rotate90cc, all, all', sideDirs, fromChar, directionFromLocations, directionFromLocation) where
import Util.Location (Location (..))
import Data.Hashable (Hashable (hashWithSalt))
import Prelude hiding (all)

data Direction4 = North | East | South | West
    deriving (Enum, Show, Eq, Ord)

instance Hashable Direction4 where
    hashWithSalt salt dir = hashWithSalt salt (fromEnum dir)

locationFromDirection :: Direction4 -> Location
locationFromDirection North = Location 0 (-1)
locationFromDirection East  = Location 1 0
locationFromDirection South = Location 0 1
locationFromDirection West  = Location (-1) 0

(+|) :: Location -> Direction4 -> Location
(+|) loc dir = loc + locationFromDirection dir

rotate90 :: Direction4 -> Direction4
rotate90 dir = toEnum $ (fromEnum dir + 1) `mod` 4

rotate90cc :: Direction4 -> Direction4
rotate90cc dir = toEnum $ (fromEnum dir + 3) `mod` 4

rotate180 :: Direction4 -> Direction4
rotate180 dir = toEnum $ (fromEnum dir + 2) `mod` 4

all :: [Direction4]
all = [North, East, South, West]

all' :: [Direction4]
all' = [East, West, North, South]

sideDirs :: Direction4 -> [Direction4]
sideDirs North = [East, West]
sideDirs East = [North, South]
sideDirs South = [East, West]
sideDirs West = [North, South]

fromChar :: Char -> Direction4
fromChar '^' = North
fromChar '>' = East
fromChar 'v' = South
fromChar '<' = West

toChar :: Direction4 -> Char
toChar North = '^'
toChar East = '>'
toChar South = 'v'
toChar West = '<'


directionFromLocation :: Location -> Direction4
directionFromLocation (Location 1 0) = East
directionFromLocation (Location 0 1) = South
directionFromLocation (Location (-1) 0) = West
directionFromLocation (Location 0 (-1)) = North

directionFromLocations :: Location -> Location -> Direction4
directionFromLocations a b = directionFromLocation (b - a)
