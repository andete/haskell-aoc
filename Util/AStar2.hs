module Util.AStar2(AStar(..), path) where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS

import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.PSQueue as Q
import Data.PSQueue (Binding ((:->)))
import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Debug.Trace (trace)


data AStar a = AStar {
    getStart :: a,
    getGoal :: a -> Bool,
    getCost :: a -> a -> Int,
    getNeighbours :: a -> [a],
    getHeuristic :: a -> Int
}

type Score a = H.HashMap a Int
type PriorityQueue a = Q.PSQ a Int
type ClosedSet a = HS.HashSet a
type ParentMap a = H.HashMap a a

path :: (Eq a, Hashable a, Show a, Ord a) => AStar a -> [a]
path aStar =
    if immediatelyDone then [start] else path' aStar gScore fScore openSet closedSet parentMap
    where start = getStart aStar
          cost = getCost aStar
          heuristic = getHeuristic aStar start
          immediatelyDone = getGoal aStar start
          gScore = H.singleton start 0
          fScore = H.singleton start (0 + heuristic)
          openSet = Q.singleton start (0 + heuristic)
          closedSet = HS.empty
          parentMap = H.empty

parentMapPath :: (Hashable a) => a -> ParentMap a -> [a]
parentMapPath current parentMap = case H.lookup current parentMap of
  Just parent -> current : parentMapPath parent parentMap
  Nothing -> [current]

path' :: (Eq a, Hashable a, Show a, Ord a) => AStar a -> Score a -> Score a -> PriorityQueue a -> ClosedSet a -> ParentMap a -> [a]
path' aStar gScore fScore openSet closedSet parentMap
  | Q.null openSet = []
  | goal current = reverse (parentMapPath current parentMap)
  | otherwise = path' aStar gScore'' fScore'' openSet'' closedSet' parentMap'
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
      neighboursNodes =  filter (\n -> not (HS.member n closedSet)) $ neighbours current
      tentatives = map (\neighbour -> (neighbour, score + cost current neighbour)) neighboursNodes
      (gScore'', fScore'', openSet'', parentMap') = foldl' (\(gs, fs, os, pm) (neighbour, tentative) ->
        if tentative < H.lookupDefault (maxBound :: Int) neighbour gs
        then let h = tentative + heuristic neighbour in 
            (H.insert neighbour tentative gs, H.insert neighbour h fs, Q.insert neighbour h os, H.insert neighbour current pm)
        else (gs, fs, os, pm)) (gScore', fScore', openSet', parentMap) tentatives