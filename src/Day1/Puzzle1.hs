module Day1.Puzzle1 (day1_1) where

import Data.List (transpose, sort)

listDistance :: [Int] -> [Int] -> Int
listDistance xs ys = sum $ map abs $ zipWith (-) (sort xs) (sort ys)

day1_1 :: IO ()
day1_1 = do
    contents <- lines <$> readFile "input/day1.txt"
    let [x, y] = transpose $ map read . words <$> contents
    putStrLn $ "Day 1, Puzzle 1 solution: "
        ++ show (listDistance x y)
