module Util.AStarStep(AStar(..), AStarMethods(..), create, next, AStarResult, path) where

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

data AStarMethods a = AStarMethods {
    getStart :: a,
    getGoal :: a -> Bool,
    getCost :: a -> a -> Int,
    getNeighbours :: a -> [a],
    getHeuristic :: a -> Int
}

type Score a = H.HashMap (Node a) Int
type PriorityQueue a = Q.PSQ (Node a) Int
type ClosedSet a = HS.HashSet (Node a)

data AStarResult = NoSolution | Solution | InProgress

data AStar a = AStar {
    methods :: AStarMethods a,
    gScore :: Score a,
    openSet :: PriorityQueue a,
    closedSet :: ClosedSet a,
    current :: Node a,
    result:: AStarResult
}

create :: (Eq a, Hashable a, Show a, Ord a) => AStarMethods a -> AStar a
create m = AStar {
    methods = m,
    gScore = H.singleton startNode 0,
    openSet = Q.singleton startNode (0 + heuristic),
    closedSet = HS.empty,
    current = startNode,
    result = InProgress
    }
    where start = getStart m
          heuristic = getHeuristic m start
          startNode = Node start Nothing

next :: (Eq a, Hashable a, Show a, Ord a) => AStar a -> AStar a
next aStar
  | Q.null (openSet aStar) = aStar { result = NoSolution }
  | goal (value current') = aStar { current = current', result = Solution, openSet = openSet', closedSet = closedSet' }
  | otherwise = aStar { current = current', gScore = gScore'', openSet = openSet'', closedSet = closedSet' }
  where
      m = methods aStar
      goal = getGoal m
      cost = getCost m
      neighbours = getNeighbours m
      heuristic = getHeuristic m
      (current' :-> _, openSet') = fromJust $ Q.minView (openSet aStar)
      closedSet' = HS.insert current' $ closedSet aStar
      score = gScore aStar H.! current'
      neighboursNodes =  filter (\n -> not (HS.member n closedSet')) $ map (\v -> Node v (Just current'))  (neighbours (value current'))
      tentatives = map (\neighbour -> (neighbour, score + cost (value current') (value neighbour))) neighboursNodes
      (gScore'', openSet'') = foldl' (\(gs, os) (neighbour, tentative) ->
        if tentative < H.lookupDefault (maxBound :: Int) neighbour gs
        then let h = tentative + heuristic (value neighbour) in
            (H.insert neighbour tentative gs, Q.insert neighbour h os)
        else (gs, os)) (gScore aStar, openSet') tentatives

path :: (Eq a, Hashable a, Show a, Ord a) => AStar a -> [a]
path aStar = reverse $ getPath $ current aStar
    where getPath (Node a Nothing) = [a]
          getPath (Node a (Just n)) = a : getPath n
