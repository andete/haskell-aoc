module Util.AStar(AStar(..), path) where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS

import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.PSQueue as Q
import Data.PSQueue (Binding ((:->)))
import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Debug.Trace (trace)

data Node a = Node a (Maybe (Node a))

value :: Node a -> a
value (Node a _) = a

-- explicit Eq and Hashable implementations to avoid recursive calls

instance Show a => Show (Node a) where
  show (Node a _) = "Node " ++ show a

instance Eq a => Eq (Node a) where
  (Node a Nothing) == (Node b (Just _)) = False
  (Node a (Just _)) == (Node b Nothing) = False
  (Node a Nothing) == (Node b Nothing)  = a == b
  (Node a (Just n1)) == (Node b (Just n2)) = a == b && value n1 == value n2

instance Hashable a => Hashable (Node a) where
    hashWithSalt s (Node a Nothing) = s `hashWithSalt` a
    hashWithSalt s (Node a (Just n)) = s `hashWithSalt` a `hashWithSalt` value n

instance Ord a => Ord (Node a) where
    compare (Node a Nothing) (Node b Nothing) = compare a b
    compare (Node a Nothing) (Node b (Just (Node x _))) = compare a b <> compare Nothing (Just x)
    compare (Node a (Just (Node x _))) (Node b Nothing) = compare a b <> compare (Just x) Nothing
    compare (Node a (Just (Node x _))) (Node b (Just (Node y _))) = compare a b <> compare x y

data AStar a = AStar {
    getStart :: a,
    getGoal :: a -> Bool,
    getCost :: a -> a -> Int,
    getNeighbours :: a -> [a],
    getHeuristic :: a -> Int
}

type Score a = H.HashMap (Node a) Int
type PriorityQueue a = Q.PSQ (Node a) Int
type ClosedSet a = HS.HashSet (Node a)

path :: (Eq a, Hashable a, Show a, Ord a) => AStar a -> [a]
path aStar =
    if immediatelyDone then [start] else path' aStar gScore fScore openSet closedSet
    where start = getStart aStar
          cost = getCost aStar
          heuristic = getHeuristic aStar start
          startNode = Node start Nothing
          immediatelyDone = getGoal aStar start
          gScore = H.singleton startNode 0
          fScore = H.singleton startNode (0 + heuristic)
          openSet = Q.singleton startNode (0 + heuristic)
          closedSet = HS.empty

path' :: (Eq a, Hashable a, Show a, Ord a) => AStar a -> Score a -> Score a -> PriorityQueue a -> ClosedSet a -> [a]
path' aStar gScore fScore openSet closedSet
  | Q.null openSet = []
  | goal (value current) = reverse (getPath current)
  | otherwise = path' aStar gScore'' fScore'' openSet'' closedSet'
  where
      goal = getGoal aStar
      cost = getCost aStar
      neighbours = getNeighbours aStar
      heuristic = getHeuristic aStar
      (current :-> _, openSet') = fromJust $ Q.minView openSet
      closedSet' = HS.insert current closedSet
      score = gScore H.! current
      gScore' = H.delete current gScore
      fScore' = H.delete current fScore
      neighboursNodes =  filter (\n -> not (HS.member n closedSet)) $ map (\v -> Node v (Just current))  (neighbours (value current))
      getPath (Node a Nothing) = [a]
      getPath (Node a (Just n)) = a : getPath n
      tentatives = map (\neighbour -> (neighbour, score + cost (value current) (value neighbour))) neighboursNodes
      (gScore'', fScore'', openSet'') = foldl' (\(gs, fs, os) (neighbour, tentative) ->
        if tentative < H.lookupDefault (maxBound :: Int) neighbour gs
        then let h = tentative + heuristic (value neighbour) in 
            (H.insert neighbour tentative gs, H.insert neighbour h fs, Q.insert neighbour h os)
        else (gs, fs, os)) (gScore', fScore', openSet') tentatives