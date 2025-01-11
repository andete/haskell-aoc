module Location(Location(..)) where

data Location = Location Int Int deriving (Eq,Show)

instance Num Location where
    (Location x1 y1) + (Location x2 y2) = Location (x1 + x2) (y1 + y2)
    (Location x1 y1) - (Location x2 y2) = Location (x1 - x2) (y1 - y2)
    (Location x1 y1) * (Location x2 y2) = Location (x1 * x2) (y1 * y2)
    abs (Location x y) = Location (abs x) (abs y)
    signum (Location x y) = Location (signum x) (signum y)
    fromInteger x = Location (fromInteger x) (fromInteger x)