module Util.Direction8(Direction8(..), (+|), rotate180, rotate90, rotate90cc, all, directionFromLocations, directionFromLocation) where
import Util.Location (Location (..))
import Data.Hashable (Hashable (hashWithSalt))
import Prelude hiding (all)

data Direction8 = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
    deriving (Enum, Show, Eq, Ord)

instance Hashable Direction8 where
    hashWithSalt salt dir = hashWithSalt salt (fromEnum dir)

locationFromDirection :: Direction8 -> Location
locationFromDirection North = Location 0 (-1)
locationFromDirection East  = Location 1 0
locationFromDirection South = Location 0 1
locationFromDirection West  = Location (-1) 0
locationFromDirection NorthEast = Location 1 (-1)
locationFromDirection SouthEast = Location 1 1
locationFromDirection SouthWest = Location (-1) 1
locationFromDirection NorthWest = Location (-1) (-1)

(+|) :: Location -> Direction8 -> Location
(+|) loc dir = loc + locationFromDirection dir

rotate90 :: Direction8 -> Direction8
rotate90 dir = toEnum $ (fromEnum dir + 2) `mod` 8

rotate90cc :: Direction8 -> Direction8
rotate90cc dir = toEnum $ (fromEnum dir + 6) `mod` 8

rotate180 :: Direction8 -> Direction8
rotate180 dir = toEnum $ (fromEnum dir + 4) `mod` 8

all :: [Direction8]
all = [North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest]



directionFromLocation :: Location -> Direction8
directionFromLocation (Location 1 0) = East
directionFromLocation (Location 0 1) = South
directionFromLocation (Location (-1) 0) = West
directionFromLocation (Location 0 (-1)) = North
directionFromLocation (Location 1 1) = SouthEast
directionFromLocation (Location 1 (-1)) = NorthEast
directionFromLocation (Location (-1) 1) = SouthWest
directionFromLocation (Location (-1) (-1)) = NorthWest

directionFromLocations :: Location -> Location -> Direction8
directionFromLocations a b = directionFromLocation (b - a)
