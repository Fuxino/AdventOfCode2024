{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day1.Puzzle2 (day1_2) where

import Data.List (transpose, sort, group, uncons)
import Data.Maybe (fromJust)

similarityScore :: [Int] -> [Int] -> Int
similarityScore xs ys = let elemsY = [ (fst . fromJust $ uncons y, length y) | y <- (group . sort) ys ]
                        in  sum [ x * snd y | x <- xs, y <- elemsY, x == fst y ]

day1_2 :: IO ()
day1_2 = do
    contents <- lines <$> readFile "input/day1.txt"
    let [x, y] = transpose $ map read . words <$> contents
    putStrLn $ "Day 1, Puzzle 2 solution: "
        ++ show (similarityScore x y)
