module Day11.Puzzle1 (day11_1) where

import qualified Data.Map.Strict as M

blinkStone :: (Int, Int) -> [(Int, Int)]
blinkStone (0, n) = [(1, n)]
blinkStone (s, n) =
  let ss = show s
      nDigit = length ss
   in if even nDigit
        then zip (map read [take (nDigit `div` 2) ss, drop (nDigit `div` 2) ss]) [n, n]
        else [(s * 2024, n)]

blink :: Int -> M.Map Int Int -> M.Map Int Int
blink 0 m = m
blink n m = blink (n - 1) $ M.fromListWith (+) $ concatMap blinkStone $ M.toList m

day11_1 :: IO ()
day11_1 = do
  contents <- M.fromListWith (+) . flip zip (repeat 1) . map read . words <$> readFile "input/day11.txt"
  putStrLn $
    "Day 11, Puzzle 1 solution: "
      ++ show (M.foldl (+) 0 $ blink 25 contents)
