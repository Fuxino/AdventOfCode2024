import Data.List (transpose, sort)
import System.IO

listDistance :: [Int] -> [Int] -> Int
listDistance xs ys = sum $ map abs $ zipWith (-) (sort xs) (sort ys)

main = do
    contents <- lines <$> readFile "day1.txt"
    let [x, y] = transpose $ map read . words <$> contents
        distance = listDistance x y
    print distance
