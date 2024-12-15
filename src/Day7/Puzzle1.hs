module Day7.Puzzle1 (day7_1) where

import Data.List (transpose)
import Data.List.Split (splitOn)

type Equation = (Int, [Int])

isSolvable :: Int -> Equation -> Bool
isSolvable cur (result, []) = cur == result
isSolvable cur (result, [x]) = cur + x == result || cur * x == result
isSolvable cur (result, x : y : ys) =
  isSolvable (cur + x + y) (result, ys)
    || isSolvable ((cur + x) * y) (result, ys)
    || isSolvable (cur * x + y) (result, ys)
    || isSolvable (cur * x * y) (result, ys)

day7_1 :: IO ()
day7_1 = do
  [x, y] <- transpose . map (splitOn ":") . lines <$> readFile "input/day7.txt"
  let results = map read x
      values = map read <$> map words y
      equations = zip results values
  putStrLn $
    "Day 7, Puzzle 1 solution: "
      ++ show (sum . map fst $ filter (isSolvable 0) equations)
