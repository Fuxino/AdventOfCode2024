module Day03
  ( day03_1,
    day03_2,
  )
where

import Data.List (isPrefixOf)
import Data.List.Split (split, splitOn, startsWith)
import Text.Regex.TDFA (getAllTextMatches, (=~))

sumMul :: [String] -> Int
sumMul xs =
  let vals = map (splitOn "," . filter (`elem` "0123456789,")) xs
   in sum $ map (product . map read) vals

filterDonts :: [String] -> String
filterDonts = concat . concatMap (filter (not . isPrefixOf "don't()") . split (startsWith "do()"))

day03_1 :: IO ()
day03_1 = do
  contents <- readFile "input/day3.txt"
  let mults = getAllTextMatches (contents =~ "mul\\([0-9]+,[0-9]+\\)") :: [String]
  putStrLn $
    "Day 3, Puzzle 1 solution: "
      ++ show (sumMul mults)

day03_2 :: IO ()
day03_2 = do
  contents <- split (startsWith "don't()") <$> readFile "input/day3.txt"
  let mults = getAllTextMatches (filterDonts contents =~ "mul\\([0-9]+,[0-9]+\\)") :: [String]
  putStrLn $
    "Day 3, Puzzle 2 solution: "
      ++ show (sumMul mults)
