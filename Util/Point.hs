
module Util.Point(Point(..), cardinal) where
data Point t = Point t t deriving (Eq, Ord, Show)

instance (Num t) => Num (Point t) where
    (Point x1 y1) + (Point x2 y2) = Point (x1 + x2) (y1 + y2)
    (Point x1 y1) - (Point x2 y2) = Point (x1 - x2) (y1 - y2)
    (Point x1 y1) * (Point x2 y2) = Point (x1 * x2) (y1 * y2)
    abs (Point x y) = Point (abs x) (abs y)
    signum (Point x y) = Point (signum x) (signum y)
    fromInteger x = Point (fromInteger x) (fromInteger x)

cardinal :: (Num t) => Point t -> [Point t]
cardinal p = map (+ p) offsets 
    where offsets = [Point 0 (-1), Point 0 1, Point 1 0, Point (-1) 0]