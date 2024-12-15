{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day01.Puzzle1 (day01_1) where

import Data.List (sort, transpose)

listDistance :: [Int] -> [Int] -> Int
listDistance xs ys = sum $ map abs $ zipWith (-) (sort xs) (sort ys)

day01_1 :: IO ()
day01_1 = do
  contents <- lines <$> readFile "input/day1.txt"
  let [x, y] = transpose $ map read . words <$> contents
  putStrLn $
    "Day 1, Puzzle 1 solution: "
      ++ show (listDistance x y)
