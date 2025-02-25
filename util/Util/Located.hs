module Util.Located(Located(..), value, location) where

import Util.Location
import Data.Hashable (Hashable (hashWithSalt))

data Located a = Located Location a deriving (Eq, Show, Ord)

instance Hashable a => Hashable (Located a) where
    hashWithSalt s (Located loc a) = hashWithSalt s (loc, a)

value :: Located a -> a
value (Located _ a) = a

location :: Located a -> Location
location (Located loc _) = loc

instance Functor Located where
    fmap f (Located loc a) = Located loc (f a)