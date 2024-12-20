module Day02
  ( day02_1,
    day02_2,
  )
where

import Control.Monad
import Data.List (sort, sortBy)
import Data.Ord

isSafe :: [Int] -> Bool
isSafe xs = (isAscending xs || isDescending xs) && maximum distances <= 3 && minimum distances >= 1
  where
    isAscending x = x == sort x
    isDescending x = x == sortBy (comparing Down) x
    distances = map abs $ zipWith (-) xs (drop 1 xs)

removeLevel :: [Int] -> [[Int]]
removeLevel xs = filter (\x -> length x == l) $ filterM (const [True, False]) xs
  where
    l = length xs - 1

day02_1 :: IO ()
day02_1 = do
  contents <- lines <$> readFile "input/day2.txt"
  let reports = map read . words <$> contents
  putStrLn $
    "Day 2, Puzzle 1 solution: "
      ++ show (length $ filter isSafe reports)

day02_2 :: IO ()
day02_2 = do
  contents <- lines <$> readFile "input/day2.txt"
  let reports = map read . words <$> contents
  putStrLn $
    "Day 2, Puzzle 2 solution: "
      ++ show (length . filter (any isSafe) $ map removeLevel reports)
