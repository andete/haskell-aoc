module Direction4(Direction4(..), (+|)) where
import Location (Location (..))

data Direction4 = North | East | South | West
    deriving (Enum, Show, Eq)

direction4location :: Direction4 -> Location
direction4location North = Location 0 (-1)
direction4location East  = Location 1 0
direction4location South = Location 0 1
direction4location West  = Location (-1) 0

(+|) :: Location -> Direction4 -> Location
(+|) loc dir = loc + direction4location dir