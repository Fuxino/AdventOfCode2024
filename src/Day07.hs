module Day07
  ( day07_1,
    day07_2,
  )
where

import Data.List (transpose)
import Data.List.Split (splitOn)

type Equation = (Int, [Int])

concatInt :: Int -> Int -> Int
concatInt x y = read $ show x ++ show y

isSolvable :: Int -> Equation -> Bool
isSolvable cur (result, []) = cur == result
isSolvable cur (result, [x]) = cur + x == result || cur * x == result
isSolvable cur (result, x : y : ys) =
  isSolvable (cur + x + y) (result, ys)
    || isSolvable ((cur + x) * y) (result, ys)
    || isSolvable (cur * x + y) (result, ys)
    || isSolvable (cur * x * y) (result, ys)

isSolvable' :: Int -> Equation -> Bool
isSolvable' cur (result, []) = cur == result
isSolvable' cur (result, [x]) = cur + x == result || cur * x == result || cur `concatInt` x == result
isSolvable' cur (result, x : y : ys) =
  isSolvable' (cur + x + y) (result, ys)
    || isSolvable' ((cur + x) * y) (result, ys)
    || isSolvable' ((cur + x) `concatInt` y) (result, ys)
    || isSolvable' (cur * x + y) (result, ys)
    || isSolvable' (cur * x * y) (result, ys)
    || isSolvable' ((cur * x) `concatInt` y) (result, ys)
    || isSolvable' ((cur `concatInt` x) + y) (result, ys)
    || isSolvable' ((cur `concatInt` x) * y) (result, ys)
    || isSolvable' ((cur `concatInt` x) `concatInt` y) (result, ys)

day07_1 :: IO ()
day07_1 = do
  [x, y] <- transpose . map (splitOn ":") . lines <$> readFile "input/day7.txt"
  let results = map read x
      values = map read <$> map words y
      equations = zip results values
  putStrLn $
    "Day 7, Puzzle 1 solution: "
      ++ show (sum . map fst $ filter (isSolvable 0) equations)

day07_2 :: IO ()
day07_2 = do
  [x, y] <- transpose . map (splitOn ":") . lines <$> readFile "input/day7.txt"
  let results = map read x
      values = map read <$> map words y
      equations = zip results values
  putStrLn $
    "Day 7, Puzzle 2 solution: "
      ++ show (sum . map fst $ filter (isSolvable' 0) equations)
