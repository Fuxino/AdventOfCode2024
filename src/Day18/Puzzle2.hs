module Day18.Puzzle2 (day18_2) where

import qualified Data.Array as A
import Data.Function (on)
import Data.List (minimumBy)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Coords = (Int, Int)

newtype Graph = Graph {edges :: M.Map Coords [Coords]} deriving (Show)

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
  { unvisited :: S.Set Coords,
    distances :: M.Map Coords (Distance Int)
  }
  deriving (Show)

initDijkstraState :: Coords -> Graph -> DijkstraState
initDijkstraState start g =
  let s =
        DijkstraState
          { unvisited = S.fromList . M.keys $ edges g,
            distances = M.fromList $ zip (M.keys $ edges g) (repeat Infinity)
          }
   in s {distances = M.adjust (const (Dist 0)) start (distances s)}

updateDistances :: Graph -> M.Map Coords (Distance Int) -> [Coords] -> Distance Int -> M.Map Coords (Distance Int)
updateDistances _ dists [] _ = dists
updateDistances graph dists (n : nodes) startD =
  updateDistances graph (M.adjust (const (addDistance startD (Dist 1))) n dists) nodes startD

findShortestPath :: Graph -> DijkstraState -> Coords -> Coords -> Distance Int
findShortestPath graph state start end
  | start == end = distances state M.! end
  | distances state M.! start == Infinity = Infinity
  | otherwise =
      let newState =
            state
              { unvisited = S.delete start (unvisited state),
                distances = updateDistances graph (distances state) (edges graph M.! start) (distances state M.! start)
              }
          nodes = S.toList $ unvisited newState
          next = fst $ minimumBy (compare `on` snd) [(n, distances newState M.! n) | n <- nodes]
       in findShortestPath graph newState next end

adjacent :: A.Array Coords Char -> Coords -> Coords -> [Coords]
adjacent array (i, j) (maxI, maxJ) = [(a, b) | (a, b) <- [(i, j + 1), (i, j - 1), (i + 1, j), (i - 1, j)], a >= 0, b >= 0, a <= maxI, b <= maxJ, array A.! (a, b) /= '#']

corruptMemory :: A.Array Coords Char -> [Coords] -> A.Array Coords Char
corruptMemory = foldl (\a b -> a A.// [(b, '#')])

findFirstBlocker :: A.Array Coords Char -> [Coords] -> Coords -> Coords -> Coords
findFirstBlocker memory (c:cs) start end =
    let memory' = corruptMemory memory [c]
        memoryGraph = Graph {edges = M.fromList [(k, adjacent memory' k (70, 70)) | k <- A.indices memory']}
        state = initDijkstraState (0, 0) memoryGraph
     in  if findShortestPath memoryGraph state start end == Infinity
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
