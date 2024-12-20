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

newtype Graph a = Graph {edges :: M.HashMap a [a]} deriving (Show)

data Distance a = Dist a | Infinity deriving (Eq)

instance (Ord a) => Ord (Distance a) where
  Infinity <= Infinity = True
  Infinity <= Dist _ = False
  Dist _ <= Infinity = True
  Dist x <= Dist y = x <= y

instance (Show a) => Show (Distance a) where
  show Infinity = "Infinity"
  show (Dist x) = show x

addDistance :: (Num a) => Distance a -> Distance a -> Distance a
addDistance (Dist x) (Dist y) = Dist (x + y)
addDistance _ _ = Infinity

data DijkstraState a b = DijkstraState
  { unvisited :: PQ.PSQ a (Distance b),
    distances :: M.HashMap a (Distance b)
  }

updateDistances :: (Hashable a) => M.HashMap a (Distance b) -> [a] -> Distance b -> M.HashMap a (Distance b)
updateDistances dists [] _ = dists
updateDistances dists (n : nodes) startD =
  updateDistances (M.adjust (const startD) n dists) nodes startD

visit :: (Ord a, Ord b) => PQ.PSQ a (Distance b) -> a -> [a] -> Distance b -> PQ.PSQ a (Distance b)
visit us node [] _ = PQ.delete node us
visit us node (e : es) dist = visit (PQ.adjust (const dist) e us) node es dist

visitNode :: (Hashable a, Ord a, Ord b) => DijkstraState a b -> Graph a -> a -> Distance b -> DijkstraState a b
visitNode state graph node d =
  let es = edges graph M.! node
      ds = updateDistances (distances state) es d
      us = visit (unvisited state) node es d
   in state {unvisited = us, distances = ds}

findShortestPath :: (Hashable a, Ord a, Ord b, Num b) => Graph a -> a -> a -> Distance b
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
                else dijkstra $ visitNode s graph n (addDistance d (Dist 1))
