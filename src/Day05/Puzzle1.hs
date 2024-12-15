{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day05.Puzzle1 (day05_1) where

import Data.List.Split (splitOn)

isSorted :: [(String, String)] -> [String] -> Bool
isSorted _ [_] = True
isSorted rules (x : xs) =
  let after = [p | (p, n) <- rules, n == x]
   in not (any (`elem` after) xs) && isSorted rules xs

getMiddle :: [String] -> String
getMiddle xs = xs !! (length xs `div` 2)

day05_1 :: IO ()
day05_1 = do
  contents <- map (splitOn "|") . lines <$> readFile "input/day5.txt"
  let rules = [(x, y) | [x, y] <- takeWhile (/= [""]) contents]
      updates = concatMap (map (splitOn ",")) . drop 1 $ dropWhile (/= [""]) contents
      sorted = filter (isSorted rules) updates
  putStrLn $
    "Day 5, Puzzle 1 solution: "
      ++ (show :: Int -> String) (sum $ map (read . getMiddle) sorted)
