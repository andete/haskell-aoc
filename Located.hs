module Located(Located(..), value, location) where

import Location

data Located a = Located Location a deriving (Eq, Show)

value :: Located a -> a
value (Located _ a) = a

location :: Located a -> Location
location (Located loc _) = loc

instance Functor Located where
    fmap f (Located loc a) = Located loc (f a)