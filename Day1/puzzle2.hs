import Data.List (transpose, sort, group)
import System.IO

similarityScore :: [Int] -> [Int] -> Int
similarityScore xs ys = let elemsY = [ (head y, length y) | y <- (group . sort) ys ]
                            xy = [ (x, snd y) | x <- xs, y <- elemsY, x == fst y ]
                        in sum [ uncurry (*) x | x <- xy ]

main = do
    contents <- lines <$> readFile "day1.txt"
    let [x, y] = transpose $ map read . words <$> contents
        score = similarityScore x y
    print score
