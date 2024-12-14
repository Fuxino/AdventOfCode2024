import Data.List (uncons)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Data.Graph (Tree, Vertex, graphFromEdges, scc)
import Data.Foldable (toList)

type Coords = (Int, Int)
type V = (String, Int)

getValue :: [[V]] -> Coords -> V
getValue grid (i, j) = grid !! i !! j

getEdges :: [[V]] -> Coords -> [Int]
getEdges grid (i, j) = let value = fst $ grid !! i !! j
                           adjI  = filter (\x -> fst x >= 0 && fst x < length grid && snd x >= 0 && snd x < length (fst . fromJust $ uncons grid)) [ (i, j + 1), (i + 1, j), (i, j - 1), (i - 1, j) ]
                        in  [ snd x | x <- map (getValue grid) adjI, (fst . fromJust $ uncons value) == (fst . fromJust $ uncons (fst x)) ]

listVertices :: [String] -> [[V]]
listVertices grid = let l = length $ fst . fromJust $ uncons grid
                    in  chunksOf l $ zip (map (:[]) (concat grid)) [0..]

calculatePerimeter :: (Vertex -> (String, Vertex, [Vertex])) -> Tree Vertex -> Int
calculatePerimeter nodeFromVertex p = let edges = concat [ x | (_, _, x) <- toList $ fmap nodeFromVertex p ]
                                          area = 4 * length p
                                      in  area - length edges

main = do
    contents <- lines <$> readFile "day12.txt"
    let grid = listVertices contents
        edgeCoords = [ (x, y) | x <- [0..length grid -1], y <- [0..length (fst . fromJust $ uncons grid) - 1] ]
        edgeList = [ (x, y, z) | ((x, y), z) <- zip (concat grid) (map (getEdges grid) edgeCoords) ]
        (graph, nodeFromVertex, _) = graphFromEdges edgeList
        plots = scc graph
        areas = map length plots
        perimeters = map (calculatePerimeter nodeFromVertex) plots
    print . sum $ zipWith (*) areas perimeters
