module Util.Location3(Location3(..), x ,y, fromList, distance) where

import Data.Hashable (Hashable, hashWithSalt)

data Location3 = Location3 Int Int Int deriving (Eq,Show, Ord)
instance Hashable Location3 where
    hashWithSalt salt (Location3 x y z) = hashWithSalt salt (x,y,z)

instance Num Location3 where
    (Location3 x1 y1 z1) + (Location3 x2 y2 z2) = Location3 (x1 + x2) (y1 + y2) (z1 + z2)
    (Location3 x1 y1 z1) - (Location3 x2 y2 z2) = Location3 (x1 - x2) (y1 - y2) (z1 - z2)
    (Location3 x1 y1 z1) * (Location3 x2 y2 z2) = Location3 (x1 * x2) (y1 * y2) (z1 * z2)
    abs (Location3 x y z) = Location3 (abs x) (abs y) (abs z)
    signum (Location3 x y z) = Location3 (signum x) (signum y) (signum z)
    fromInteger x = Location3 (fromInteger x) (fromInteger x) (fromInteger x)

x :: Location3 -> Int
x (Location3 x _ _) = x

y :: Location3 -> Int
y (Location3 _ y _) = y

z :: Location3 -> Int
z (Location3 _ _ z) = z

fromList :: [Int] -> Location3
fromList [x,y,z] = Location3 x y z

distance :: Location3 -> Location3 -> Double
distance (Location3 x1 y1 z1) (Location3 x2 y2 z2) = sqrt $ fromIntegral ((x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2)