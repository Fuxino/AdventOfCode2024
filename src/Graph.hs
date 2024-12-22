module Graph
  ( Graph (..),
    Distance (..),
    findShortestPath,
  )
where

import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import Data.Maybe (fromJust)
import qualified Data.PSQueue as PQ

data Distance a = Dist a | Infinity deriving (Eq)

instance (Ord a) => Ord (Distance a) where
  Infinity <= Infinity = True
  Infinity <= Dist _ = False
  Dist _ <= Infinity = True
  Dist x <= Dist y = x <= y

instance (Show a) => Show (Distance a) where
  show Infinity = "Infinity"
  show (Dist x) = show x

newtype Graph a b = Graph {edges :: M.HashMap a [(a, Distance b)]} deriving (Show)

addDistance :: (Num a) => Distance a -> Distance a -> Distance a
addDistance (Dist x) (Dist y) = Dist (x + y)
addDistance _ _ = Infinity

data DijkstraState a b = DijkstraState
  { unvisited :: PQ.PSQ a (Distance b),
    distances :: M.HashMap a (Distance b)
  }

updateDistances :: (Hashable a, Num b) => M.HashMap a (Distance b) -> [(a, Distance b)] -> Distance b -> M.HashMap a (Distance b)
updateDistances dists [] _ = dists
updateDistances dists (n : nodes) startD =
  let newD = addDistance startD (snd n)
   in updateDistances (M.adjust (const newD) (fst n) dists) nodes startD

visit :: (Ord a, Num b, Ord b) => PQ.PSQ a (Distance b) -> a -> [(a, Distance b)] -> Distance b -> PQ.PSQ a (Distance b)
visit us node [] _ = PQ.delete node us
visit us node (e : es) startD =
  let newD = addDistance startD (snd e)
   in visit (PQ.adjust (const newD) (fst e) us) node es startD

visitNode :: (Hashable a, Ord a, Num b, Ord b) => DijkstraState a b -> Graph a b -> a -> Distance b -> DijkstraState a b
visitNode state graph node d =
  let es = edges graph M.! node
      ds = updateDistances (distances state) es d
      us = visit (unvisited state) node es d
   in state {unvisited = us, distances = ds}

findShortestPath :: (Hashable a, Ord a, Ord b, Num b) => Graph a b -> a -> a -> Distance b
findShortestPath graph start end =
  let nodesDist = (start PQ.:-> Dist 0) : [k PQ.:-> Infinity | k <- M.keys $ edges graph, k /= start]
      dists = (start, Dist 0) : [(k, Infinity) | k <- M.keys $ edges graph, k /= start]
      initialState = DijkstraState {unvisited = PQ.fromList nodesDist, distances = M.fromList dists}
   in dijkstra initialState
  where
    dijkstra s =
      let nd = fromJust $ PQ.findMin (unvisited s)
          n = PQ.key nd
          d = PQ.prio nd
       in if n == end
            then d
            else
              if d == Infinity
                then Infinity
                else dijkstra $ visitNode s graph n d
