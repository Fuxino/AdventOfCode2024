{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-type-defaults #-}

module Day16 (day16_1) where

import qualified Data.Array as A
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.Ix
import GHC.Generics (Generic)
import Graph

data Direction = N | S | E | W deriving (Eq, Ord, Generic, Ix)

instance Hashable Direction

type Coords = (Int, Int, Direction)

getCost :: Direction -> Direction -> Int
getCost a b
  | a == b = 1
  | otherwise = 1000

adjacent :: A.Array Coords Char -> Coords -> (Int, Int) -> [(Coords, Distance Int)]
adjacent array (i, j, N) (maxI, maxJ) =
  [((a, b, d), Dist (getCost N d)) | i >= 0, j >= 0, i <= maxI, j <= maxJ, (a, b, d) <- [(i - 1, j, N), (i, j, W), (i, j, E)], array A.! (i, j, d) /= '#']
adjacent array (i, j, S) (maxI, maxJ) =
  [((a, b, d), Dist (getCost S d)) | i >= 0, j >= 0, i <= maxI, j <= maxJ, (a, b, d) <- [(i + 1, j, S), (i, j, W), (i, j, E)], array A.! (i, j, d) /= '#']
adjacent array (i, j, E) (maxI, maxJ) =
  [((a, b, d), Dist (getCost E d)) | i >= 0, j >= 0, i <= maxI, j <= maxJ, (a, b, d) <- [(i, j + 1, E), (i, j, N), (i, j, S)], array A.! (i, j, d) /= '#']
adjacent array (i, j, W) (maxI, maxJ) =
  [((a, b, d), Dist (getCost W d)) | i >= 0, j >= 0, i <= maxI, j <= maxJ, (a, b, d) <- [(i, j - 1, W), (i, j, N), (i, j, S)], array A.! (i, j, d) /= '#']

getMazeGraph :: A.Array Coords Char -> Int -> Int -> (Graph Coords Int, Coords, [Coords])
getMazeGraph mazeMap nRow nCol =
  let mazeGraph = Graph {edges = M.fromList [(k, adjacent mazeMap k (nRow, nCol)) | k <- A.indices mazeMap]}
      start = fst . last . filter (\((_, _, d), c) -> d == E && c == 'S') $ A.assocs mazeMap
      end = map fst $ filter (\(_, c) -> c == 'E') $ A.assocs mazeMap
   in (mazeGraph, start, end)

parseInput :: IO (A.Array (Int, Int, Direction) Char, Int, Int)
parseInput = do
  contents <- lines <$> readFile "input/day16.txt"
  let nRow = length contents - 1
      nCol = length (last contents) - 1
      mazeMap = A.listArray ((0, 0, N), (nRow, nCol, W)) (concatMap (replicate 4) $ concat contents)
  return (mazeMap, nRow, nCol)

day16_1 :: IO ()
day16_1 = do
  (mazeMap, nRow, nCol) <- parseInput
  let (mazeGraph, start, end) = getMazeGraph mazeMap nRow nCol
      shortestPaths = [findShortestPath mazeGraph start e | e <- end]
  --  putStrLn $ "Day 16, Puzzle 1 solution: " ++ show (findShortestPath mazeGraph start end)
  putStrLn $ "Day 16, Puzzle 1 solution: " ++ show (minimum shortestPaths)
