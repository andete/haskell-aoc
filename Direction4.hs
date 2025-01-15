module Direction4(Direction4(..), (+|), rotate180, rotate90, rotate90cc, all) where
import Location (Location (..))
import Data.Hashable (Hashable (hashWithSalt))
import Prelude hiding (all)

data Direction4 = North | East | South | West
    deriving (Enum, Show, Eq)

instance Hashable Direction4 where
    hashWithSalt salt dir = hashWithSalt salt (fromEnum dir)

direction4location :: Direction4 -> Location
direction4location North = Location 0 (-1)
direction4location East  = Location 1 0
direction4location South = Location 0 1
direction4location West  = Location (-1) 0

(+|) :: Location -> Direction4 -> Location
(+|) loc dir = loc + direction4location dir

rotate90 :: Direction4 -> Direction4
rotate90 dir = toEnum $ (fromEnum dir + 1) `mod` 4

rotate90cc :: Direction4 -> Direction4
rotate90cc dir = toEnum $ (fromEnum dir + 3) `mod` 4

rotate180 :: Direction4 -> Direction4
rotate180 dir = toEnum $ (fromEnum dir + 2) `mod` 4

all :: [Direction4]
all = [North, East, South, West]