{-# OPTIONS_GHC -Wno-x-partial #-}

module Day12 (day12_1) where

import Data.Foldable (toList)
import Data.Graph (Tree, Vertex, graphFromEdges, scc)
import Data.List.Split (chunksOf)

type Coords = (Int, Int)

type V = (String, Int)

getValue :: [[V]] -> Coords -> V
getValue grid (i, j) = grid !! i !! j

getEdges :: [[V]] -> Coords -> [Int]
getEdges grid (i, j) =
  let value = fst $ grid !! i !! j
      adjI = filter (\x -> fst x >= 0 && fst x < length grid && snd x >= 0 && snd x < length (head grid)) [(i, j + 1), (i + 1, j), (i, j - 1), (i - 1, j)]
   in [snd x | x <- map (getValue grid) adjI, head value == head (fst x)]

listVertices :: [String] -> [[V]]
listVertices grid =
  let l = length $ head grid
   in chunksOf l $ zip (map (: []) (concat grid)) [0 ..]

calculatePerimeter :: (Vertex -> (String, Vertex, [Vertex])) -> Tree Vertex -> Int
calculatePerimeter nodeFromVertex p =
  let edges = concat [x | (_, _, x) <- toList $ fmap nodeFromVertex p]
      area = 4 * length p
   in area - length edges

day12_1 :: IO ()
day12_1 = do
  contents <- lines <$> readFile "input/day12.txt"
  let grid = listVertices contents
      edgeCoords = [(x, y) | x <- [0 .. length grid - 1], y <- [0 .. length (head grid) - 1]]
      edgeList = [(x, y, z) | ((x, y), z) <- zip (concat grid) (map (getEdges grid) edgeCoords)]
      (graph, nodeFromVertex, _) = graphFromEdges edgeList
      plots = scc graph
      areas = map length plots
      perimeters = map (calculatePerimeter nodeFromVertex) plots
  putStrLn $
    "Day 12, Puzzle 1 solution: "
      ++ show (sum $ zipWith (*) areas perimeters)
