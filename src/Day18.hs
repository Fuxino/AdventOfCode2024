{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-type-defaults #-}

module Day18
  ( day18_1,
    day18_2,
  )
where

import qualified Data.Array as A
import qualified Data.HashMap.Strict as M
import Data.List.Split (splitOn)
import Graph

type Coords = (Int, Int)

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

getCorruptedMemoryMap :: [[String]] -> A.Array Coords Char
getCorruptedMemoryMap fallingBytes =
  let memory = A.listArray ((0, 0), (70, 70)) $ replicate 5041 '.'
      bytesCoords = take 1024 [(read x, read y) | (x : y : _) <- fallingBytes]
      corruptedMemory = corruptMemory memory bytesCoords
  in corruptedMemory

day18_1 :: IO ()
day18_1 = do
  contents <- map (splitOn ",") . lines <$> readFile "input/day18.txt"
  let corruptedMemory = getCorruptedMemoryMap contents
      memoryGraph = Graph {edges = M.fromList [(k, adjacent corruptedMemory k (70, 70)) | k <- A.indices corruptedMemory]}
  putStrLn $ "Day 18, Puzzle 1 solution: " ++ show (findShortestPath memoryGraph (0, 0) (70, 70))

day18_2 :: IO ()
day18_2 = do
  contents <- map (splitOn ",") . lines <$> readFile "input/day18.txt"
  let corruptedMemory = getCorruptedMemoryMap contents
      fallingBytesCoords = drop 1024 [(read x, read y) | (x : y : _) <- contents]
  putStrLn $ "Day 18, Puzzle 2 solution: " ++ show (findFirstBlocker corruptedMemory fallingBytesCoords (0, 0) (70, 70))
