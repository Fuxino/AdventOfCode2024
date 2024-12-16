{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day14.Puzzle2 (day14_2) where

import Data.Char (isDigit)
import Data.List (nub)
import Data.List.Split (splitOn)

type Position = (Int, Int)

type Velocity = (Int, Int)

type Robot = (Position, Velocity)

readRobot :: String -> Robot
readRobot s =
  let [ps, vs] = splitOn " " s
      [px, py] = map read . splitOn "," $ filter (\x -> isDigit x || x == ',' || x == '-') ps
      [vx, vy] = map read . splitOn "," $ filter (\x -> isDigit x || x == ',' || x == '-') vs
   in ((px, py), (vx, vy))

moveRobot :: Robot -> Robot
moveRobot r =
  let (px, py) = fst r
      (vx, vy) = snd r
   in (((px + vx) `mod` 101, (py + vy) `mod` 103), (vx, vy))

findChristmasTree :: Int -> [Robot] -> Int
findChristmasTree n rs =
  let rs' = map moveRobot rs
      positions = map fst rs'
   in if positions == nub positions
        then n
        else findChristmasTree (n + 1) rs'

day14_2 :: IO ()
day14_2 = do
  contents <- lines <$> readFile "input/day14.txt"
  let robots = map readRobot contents
  putStrLn $
    "Day 14, Puzzle 2 solution: "
      ++ show (findChristmasTree 1 robots)
