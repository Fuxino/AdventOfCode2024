{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day05
  ( day05_1,
    day05_2,
  )
where

import Data.List ((\\))
import Data.List.Split (splitOn)

isSortedS :: [(String, String)] -> [String] -> Bool
isSortedS _ [_] = True
isSortedS rules (x : xs) =
  let after = [p | (p, n) <- rules, n == x]
   in not (any (`elem` after) xs) && isSortedS rules xs

isSortedI :: [(Int, Int)] -> [Int] -> Bool
isSortedI _ [_] = True
isSortedI rules (x : xs) =
  let after = [p | (p, n) <- rules, n == x]
   in not (any (`elem` after) xs) && isSortedI rules xs

getMiddleS :: [String] -> String
getMiddleS xs = xs !! (length xs `div` 2)

getMiddleI :: [Int] -> Int
getMiddleI xs = xs !! (length xs `div` 2)

sortOnRules :: [(Int, Int)] -> [Int] -> [Int]
sortOnRules _ [] = []
sortOnRules rules (x : xs) = sortOnRules rules beforeArray ++ [x] ++ sortOnRules rules afterArray
  where
    afterArray = xs \\ before
    beforeArray = xs \\ afterArray
    before = [p | (p, n) <- rules, n == x]

day05_1 :: IO ()
day05_1 = do
  contents <- map (splitOn "|") . lines <$> readFile "input/day5.txt"
  let rules = [(x, y) | [x, y] <- takeWhile (/= [""]) contents]
      updates = concatMap (map (splitOn ",")) . drop 1 $ dropWhile (/= [""]) contents
      sorted = filter (isSortedS rules) updates
  putStrLn $
    "Day 5, Puzzle 1 solution: "
      ++ (show :: Int -> String) (sum $ map (read . getMiddleS) sorted)

day05_2 :: IO ()
day05_2 = do
  contents <- map (splitOn "|") . lines <$> readFile "input/day5.txt"
  let rules = [(read x, read y) | [x, y] <- takeWhile (/= [""]) contents]
      unsorted = filter (not . isSortedI rules) . map (map read) $ concatMap (map (splitOn ",")) . drop 1 $ dropWhile (/= [""]) contents
      fixUnsorted = map (sortOnRules rules) unsorted
  putStrLn $
    "Day 5, Puzzle 2 solution: "
      ++ show (sum $ map getMiddleI fixUnsorted)
