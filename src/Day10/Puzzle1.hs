module Day10.Puzzle1 (day10_1) where

import Data.Char (digitToInt)
import Data.Graph (graphFromEdges, path, vertices)
import Data.List (uncons)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

type Coords = (Int, Int)

type V = (String, Int)

getValue :: [[V]] -> Coords -> V
getValue grid (i, j) = grid !! i !! j

getEdges :: [[V]] -> Coords -> [Int]
getEdges grid (i, j) =
  let value = fst $ grid !! i !! j
      adjI = filter (\x -> fst x >= 0 && fst x < length grid && snd x >= 0 && snd x < length (fst . fromJust $ uncons grid)) [(i, j + 1), (i + 1, j), (i, j - 1), (i - 1, j)]
   in [snd x | x <- map (getValue grid) adjI, digitToInt (fst . fromJust $ uncons value) == digitToInt (fst . fromJust $ uncons (fst x)) - 1]

listVertices :: [String] -> [[V]]
listVertices grid =
  let l = length $ fst . fromJust $ uncons grid
   in chunksOf l $ zip (map (: []) (concat grid)) [0 ..]

day10_1 :: IO ()
day10_1 = do
  contents <- lines <$> readFile "input/day10.txt"
  let grid = listVertices contents
      edgeCoords = [(x, y) | x <- [0 .. length grid - 1], y <- [0 .. length (fst . fromJust $ uncons grid) - 1]]
      edgeList = [(x, y, z) | ((x, y), z) <- zip (concat grid) (map (getEdges grid) edgeCoords)]
      (graph, nodeFromVertex, _) = graphFromEdges edgeList
      startList = [x | (_, x, _) <- filter (\(x, _, _) -> x == "0") $ map nodeFromVertex $ vertices graph]
      endList = [x | (_, x, _) <- filter (\(x, _, _) -> x == "9") $ map nodeFromVertex $ vertices graph]
      paths = filter id $ [path graph x y | x <- startList, y <- endList]
  putStrLn $ "Day 10, Puzzle 1 solution: " ++ show (length paths)
