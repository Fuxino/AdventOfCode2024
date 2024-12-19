{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day18.Puzzle2 (day18_2) where

import qualified Data.Array as A
import qualified Data.HashMap.Strict as M
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.PSQueue as PQ

type Coords = (Int, Int)

newtype Graph = Graph {edges :: M.HashMap Coords [Coords]} deriving (Show)

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

data DijkstraState = DijkstraState
  { unvisited :: PQ.PSQ Coords (Distance Int),
    distances :: M.HashMap Coords (Distance Int)
  }
  deriving (Show)

updateDistances :: M.HashMap Coords (Distance Int) -> [Coords] -> Distance Int -> M.HashMap Coords (Distance Int)
updateDistances dists [] _ = dists
updateDistances dists (n : nodes) startD =
  updateDistances (M.adjust (const startD) n dists) nodes startD

visit :: PQ.PSQ Coords (Distance Int) -> Coords -> [Coords] -> Distance Int -> PQ.PSQ Coords (Distance Int)
visit us node [] _ = PQ.delete node us
visit us node (e : es) dist = visit (PQ.adjust (const dist) e us) node es dist

visitNode :: DijkstraState -> Graph -> Coords -> Distance Int -> DijkstraState
visitNode state graph node d =
  let es = edges graph M.! node
      ds = updateDistances (distances state) es d
      us = visit (unvisited state) node es d
   in state {unvisited = us, distances = ds}

findShortestPath :: Graph -> Coords -> Coords -> Distance Int
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

adjacent :: A.Array Coords Char -> Coords -> Coords -> [Coords]
adjacent array (i, j) (maxI, maxJ) = [(a, b) | (a, b) <- [(i, j + 1), (i, j - 1), (i + 1, j), (i - 1, j)], a >= 0, b >= 0, a <= maxI, b <= maxJ, array A.! (a, b) /= '#']

corruptMemory :: A.Array Coords Char -> [Coords] -> A.Array Coords Char
corruptMemory = foldl (\a b -> a A.// [(b, '#')])

findFirstBlocker :: A.Array Coords Char -> [Coords] -> Coords -> Coords -> Coords
findFirstBlocker memory (c : cs) start end =
  let memory' = corruptMemory memory [c]
      memoryGraph = Graph {edges = M.fromList [(k, adjacent memory' k (70, 70)) | k <- A.indices memory']}
   in if findShortestPath memoryGraph start end == Infinity
        then c
        else findFirstBlocker memory' cs start end

day18_2 :: IO ()
day18_2 = do
  contents <- map (splitOn ",") . lines <$> readFile "input/day18.txt"
  let memory = A.listArray ((0, 0), (70, 70)) $ replicate 5041 '.'
      coords = take 1024 [(read x, read y) | (x : y : _) <- contents]
      coords' = drop 1024 [(read x, read y) | (x : y : _) <- contents]
      memory' = corruptMemory memory coords
  putStrLn $ "Day 18, Puzzle 2 solution: " ++ show (findFirstBlocker memory' coords' (0, 0) (70, 70))
