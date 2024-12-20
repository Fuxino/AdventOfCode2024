module Day10.Puzzle2 (day10_2) where

import qualified Data.Array as A
import Data.Char (digitToInt)
import qualified Data.HashMap.Strict as M

type Coords = (Int, Int)

newtype Graph a = Graph {edges :: M.HashMap a [a]} deriving (Show)

adjacent :: A.Array Coords Int -> Coords -> Coords -> [Coords]
adjacent array (i, j) (maxI, maxJ) = [(a, b) | (a, b) <- [(i, j + 1), (i, j - 1), (i + 1, j), (i - 1, j)], a >= 0, b >= 0, a <= maxI, b <= maxJ, array A.! (a, b) - array A.! (i, j) == 1]

findAllPaths :: Graph Coords -> Coords -> Coords -> [Coords] -> [[Coords]]
findAllPaths graph start end path = do
  node <- edges graph M.! start
  let path' = path ++ [node]
  if node == end
    then return path'
    else findAllPaths graph node end path'

day10_2 :: IO ()
day10_2 = do
  contents <- lines <$> readFile "input/day10.txt"
  let trailMap = A.listArray ((0, 0), (52, 52)) $ map digitToInt $ concat contents
      trailGraph = Graph {edges = M.fromList [(k, adjacent trailMap k (52, 52)) | k <- A.indices trailMap]}
      startList = map fst . filter (\(_, y) -> y == 0) $ A.assocs trailMap
      endList = map fst . filter (\(_, y) -> y == 9) $ A.assocs trailMap
      paths = concat $ filter (not . null) [findAllPaths trailGraph x y [x] | x <- startList, y <- endList]
  putStrLn $ "Day 10, Puzzle 2 solution: " ++ show (length paths)
