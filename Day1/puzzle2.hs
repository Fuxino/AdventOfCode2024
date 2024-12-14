import Data.List (transpose, sort, group, uncons)
import Data.Maybe (fromJust)

similarityScore :: [Int] -> [Int] -> Int
similarityScore xs ys = let elemsY = [ (fst . fromJust $ uncons y, length y) | y <- (group . sort) ys ]
                        in  sum [ x * snd y | x <- xs, y <- elemsY, x == fst y ]

main = do
    contents <- lines <$> readFile "day1.txt"
    let [x, y] = transpose $ map read . words <$> contents
    print $ similarityScore x y
