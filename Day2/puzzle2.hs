import Data.List (sort, sortBy, inits, tails)
import Data.Ord

isSafe :: [Int] -> Bool
isSafe xs = (isAscending xs || isDescending xs) && maximum distances <= 3 && minimum distances >= 1
            where isAscending xs  = xs == sort xs
                  isDescending xs = xs == sortBy (comparing Down) xs
                  distances       = map abs $ zipWith (-) xs (drop 1 xs)

removeLevel :: [Int] -> [[Int]]
removeLevel xs = zipWith (++) ys zs
            where ys = map init $ drop 1 (inits xs)
                  zs = map (drop 1) $ init (tails xs)

main = do
    contents <- lines <$> readFile "day2.txt"
    let reports = map read . words <$> contents
    print . length . filter (any isSafe) $ map removeLevel reports
