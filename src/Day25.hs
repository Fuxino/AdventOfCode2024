{-# OPTIONS_GHC -Wno-x-partial #-}

module Day25 (day25_1) where

import Control.Monad (guard)
import Data.List (transpose)
import Data.List.Split (splitOn)

parseSchematics :: [String] -> [Int]
parseSchematics s =
  let s' = transpose . init $ tail s
   in map (length . filter (== '#')) s'

keyLockCombinations :: [[Int]] -> [[Int]] -> [[Int]]
keyLockCombinations keys locks = do
  key <- keys
  lock <- locks

  guard $ all (< 6) $ zipWith (+) key lock

  return $ zipWith (+) key lock

day25_1 :: IO ()
day25_1 = do
  contents <- lines <$> readFile "input/day25.txt"
  let schematics = splitOn [""] contents
      locks = map parseSchematics $ filter (\x -> head x == "#####" && last x == ".....") schematics
      keys = map parseSchematics $ filter (\x -> head x == "....." && last x == "#####") schematics
  putStrLn $
    "Day 25, Puzzle 1 solution: "
      ++ show (length $ keyLockCombinations keys locks)
  print . length $ keyLockCombinations keys locks
