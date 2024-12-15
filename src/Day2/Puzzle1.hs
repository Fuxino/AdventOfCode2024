module Day2.Puzzle1 (day2_1) where

import Data.List (sort, sortBy)
import Data.Ord

isSafe :: [Int] -> Bool
isSafe xs = (isAscending xs || isDescending xs) && maximum distances <= 3 && minimum distances >= 1
  where
    isAscending x = x == sort x
    isDescending x = x == sortBy (comparing Down) x
    distances = map abs $ zipWith (-) xs (drop 1 xs)

day2_1 :: IO ()
day2_1 = do
  contents <- lines <$> readFile "input/day2.txt"
  let reports = map read . words <$> contents
  putStrLn $
    "Day 2, Puzzle 1 solution: "
      ++ show (length $ filter isSafe reports)
