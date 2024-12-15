module Day02.Puzzle2 (day02_2) where

import Data.List (inits, sort, sortBy, tails)
import Data.Ord

isSafe :: [Int] -> Bool
isSafe xs = (isAscending xs || isDescending xs) && maximum distances <= 3 && minimum distances >= 1
  where
    isAscending x = x == sort x
    isDescending x = x == sortBy (comparing Down) x
    distances = map abs $ zipWith (-) xs (drop 1 xs)

removeLevel :: [Int] -> [[Int]]
removeLevel xs = zipWith (++) ys zs
  where
    ys = map init $ drop 1 (inits xs)
    zs = map (drop 1) $ init (tails xs)

day02_2 :: IO ()
day02_2 = do
  contents <- lines <$> readFile "input/day2.txt"
  let reports = map read . words <$> contents
  putStrLn $
    "Day 2, Puzzle 2 solution: "
      ++ show (length . filter (any isSafe) $ map removeLevel reports)
