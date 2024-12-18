module Day18.Puzzle1 (day18_1) where

import Data.Array (Array, indices, listArray, (!), (//))
import Data.Function (on)
import Data.List (minimumBy)
import Data.List.Split (splitOn)
import qualified Data.Map as M
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

data DijkstraState = DijkstraState
  { unvisited :: S.Set Coords,
    distances :: M.Map Coords (Distance Int)
  }
  deriving (Show)

addDistance :: (Num a) => Distance a -> Distance a -> Distance a
addDistance (Dist x) (Dist y) = Dist (x + y)
addDistance _ _ = Infinity

corruptMemory :: Array Coords Char -> [Coords] -> Array Coords Char
corruptMemory = foldl (\a b -> a // [(b, '#')])

adjacent :: Array Coords Char -> Coords -> Coords -> [Coords]
adjacent array (i, j) (maxI, maxJ) = [(a, b) | (a, b) <- [(i, j + 1), (i, j - 1), (i + 1, j), (i - 1, j)], a >= 0, b >= 0, a <= maxI, b <= maxJ, array ! (a, b) /= '#']

initDijkstraState :: Coords -> Graph -> DijkstraState
initDijkstraState start g =
  let s =
        DijkstraState
          { unvisited = S.fromList . M.keys $ edges g,
            distances = M.fromList $ zip (M.keys $ edges g) (repeat Infinity)
          }
   in s {distances = M.adjust (const (Dist 0)) start (distances s)}

updateDistances :: Graph -> M.Map Coords (Distance Int) -> Coords -> M.Map Coords (Distance Int)
updateDistances graph dists coords =
  let nodes = edges graph M.! coords
      dist = dists M.! coords
   in M.mapWithKey (\k d -> if k `elem` nodes && d > dist then addDistance dist (Dist 1) else d) dists

findShortestPath :: Graph -> DijkstraState -> Coords -> Coords -> Distance Int
findShortestPath graph state start end
  | start == end = distances state M.! end
  | S.null (unvisited state) = distances state M.! end
  | distances state M.! start == Infinity = Infinity
  | otherwise =
      let newState =
            state
              { unvisited = S.delete start (unvisited state),
                distances = updateDistances graph (distances state) start
              }
          nodes = S.toList $ unvisited newState
          next = fst $ minimumBy (compare `on` snd) [(n, distances newState M.! n) | n <- nodes]
       in findShortestPath graph newState next end

day18_1 :: IO ()
day18_1 = do
  contents <- map (splitOn ",") . lines <$> readFile "input/day18.txt"
  let memory = listArray ((0, 0), (70, 70)) $ replicate 5041 '.'
      coords = take 1024 [(read x, read y) | (x : y : _) <- contents]
      memory' = corruptMemory memory coords
      memoryMap = Graph {edges = M.fromList [(k, adjacent memory' k (70, 70)) | k <- indices memory']}
      state = initDijkstraState (0, 0) memoryMap
  putStrLn $ "Day 18, Puzzle 1 solution: " ++ show (findShortestPath memoryMap state (0, 0) (70, 70))
