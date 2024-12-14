{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day14.Puzzle1 (day14_1) where

import Data.Char (isDigit)
import Data.List.Split (splitOn)

type Position = (Int, Int)
type Velocity = (Int, Int)
type Robot = (Position, Velocity)

readRobot :: String -> Robot
readRobot s = let [ps, vs] = splitOn " " s
                  [px, py] = map read . splitOn "," $ filter (\x -> isDigit x || x == ',' || x == '-') ps
                  [vx, vy] = map read . splitOn "," $ filter (\x -> isDigit x || x == ',' || x == '-') vs
              in  ((px, py), (vx, vy))

moveRobot :: Int -> Robot -> Robot
moveRobot 0 r = r
moveRobot n r = let (px, py) = fst r
                    (vx, vy) = snd r
                in  moveRobot (n - 1) (((px + vx) `mod` 101, (py + vy) `mod` 103), (vx, vy))

quadrant :: Robot -> Int
quadrant r
    | fst p `elem` [0..49] &&
        snd p `elem` [0..50] = 0
    | fst p `elem` [51..100] &&
        snd p `elem` [0..50] = 1
    | fst p `elem` [0..49] &&
        snd p `elem` [52..102] = 2
    | fst p `elem` [51..100] &&
        snd p `elem` [52..102] = 3
    | otherwise = -1
    where p = fst r

day14_1 :: IO ()
day14_1= do
    contents <- lines <$> readFile "input/day14.txt"
    let robots =  map readRobot contents
        robots' = map (moveRobot 100) robots
        firstQ = length $ filter (\r -> quadrant r == 0) robots'
        secondQ = length $ filter (\r -> quadrant r == 1) robots'
        thirdQ = length $ filter (\r -> quadrant r == 2) robots'
        fourthQ = length $ filter (\r -> quadrant r == 3) robots'
    putStrLn $ "Day 14, Puzzle 1 solution: "
        ++ show (firstQ * secondQ * thirdQ * fourthQ)
