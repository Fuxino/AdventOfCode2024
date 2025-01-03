{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-x-partial #-}

module Day01
  ( day01_1,
    day01_2,
  )
where

import Data.List (group, sort, transpose)

listDistance :: [Int] -> [Int] -> Int
listDistance xs ys = sum $ map abs $ zipWith (-) (sort xs) (sort ys)

similarityScore :: [Int] -> [Int] -> Int
similarityScore xs ys =
  let elemsY = [(head y, length y) | y <- (group . sort) ys]
   in sum [x * snd y | x <- xs, y <- elemsY, x == fst y]

parseInput :: IO [[Int]]
parseInput = do
  contents <- lines <$> readFile "input/day1.txt"
  let [x, y] = transpose $ map read . words <$> contents
  return [x, y]

day01_1 :: IO ()
day01_1 = do
  [x, y] <- parseInput
  putStrLn $
    "Day 1, Puzzle 1 solution: "
      ++ show (listDistance x y)

day01_2 :: IO ()
day01_2 = do
  [x, y] <- parseInput
  putStrLn $
    "Day 1, Puzzle 2 solution: "
      ++ show (similarityScore x y)
