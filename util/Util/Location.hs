module Util.Location(Location(..), x ,y, fromList, hammingDistance) where

import Data.Hashable (Hashable, hashWithSalt)

data Location = Location Int Int deriving (Eq,Show)

instance Ord Location where
    compare (Location x1 y1) (Location x2 y2) = case compare y1 y2 of
        EQ -> compare x1 x2
        other -> other

instance Hashable Location where
    hashWithSalt salt (Location x y) = hashWithSalt salt (x,y)

instance Num Location where
    (Location x1 y1) + (Location x2 y2) = Location (x1 + x2) (y1 + y2)
    (Location x1 y1) - (Location x2 y2) = Location (x1 - x2) (y1 - y2)
    (Location x1 y1) * (Location x2 y2) = Location (x1 * x2) (y1 * y2)
    abs (Location x y) = Location (abs x) (abs y)
    signum (Location x y) = Location (signum x) (signum y)
    fromInteger x = Location (fromInteger x) (fromInteger x)

x :: Location -> Int
x (Location x _) = x

y :: Location -> Int
y (Location _ y) = y

fromList :: [Int] -> Location
fromList [x,y] = Location x y

hammingDistance :: Location -> Location -> Int
hammingDistance (Location x1 y1) (Location x2 y2) = abs (x1 - x2) + abs (y1 - y2)