module Day3.Puzzle1 (day3_1) where

import Data.List.Split (splitOn)
import Text.Regex.TDFA (getAllTextMatches, (=~))

sumMul :: [String] -> Int
sumMul xs =
  let vals = map (splitOn "," . filter (`elem` "0123456789,")) xs
   in sum $ map (product . map read) vals

day3_1 :: IO ()
day3_1 = do
  contents <- readFile "input/day3.txt"
  let mults = getAllTextMatches (contents =~ "mul\\([0-9]+,[0-9]+\\)") :: [String]
  putStrLn $
    "Day 3, Puzzle 1 solution: "
      ++ show (sumMul mults)
