import Data.List (sort)
import System.IO

isSafe :: [Int] -> Bool
isSafe xs = (isAscending xs || isDescending xs) && maximum distances <= 3 && minimum distances >= 1
            where isAscending xs = xs == sort xs
                  isDescending xs = xs == reverse (sort xs)
                  distances = map abs $ zipWith (-) xs (tail xs)

main = do
    contents <- lines <$> readFile "day2.txt"
    let reports = map read . words <$> contents
    print . length $ filter isSafe reports
