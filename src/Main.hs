module Main (main) where

import Day01 (day01_1, day01_2)
import Day02 (day02_1, day02_2)
import Day03 (day03_1, day03_2)
import Day04 (day04_1, day04_2)
import Day05 (day05_1, day05_2)
import Day06 (day06_1, day06_2)
import Day07 (day07_1, day07_2)
import Day08 (day08_1, day08_2)
import Day09 (day09_1, day09_2)
import Day10 (day10_1, day10_2)
import Day11 (day11_1, day11_2)
import Day12 (day12_1)
import Day13 (day13_1, day13_2)
import Day14 (day14_1, day14_2)
import Day15 (day15_1)
import Day17 (day17_1, day17_2)
import Day18 (day18_1, day18_2)
import Day19 (day19_1)
import Day22 (day22_1)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "1" : "1" : _ -> day01_1
    "1" : "2" : _ -> day01_2
    "1" : _ -> do
      day01_1
      day01_2
    "2" : "1" : _ -> day02_1
    "2" : "2" : _ -> day02_2
    "2" : _ -> do
      day02_1
      day02_2
    "3" : "1" : _ -> day03_1
    "3" : "2" : _ -> day03_2
    "3" : _ -> do
      day03_1
      day03_2
    "4" : "1" : _ -> day04_1
    "4" : "2" : _ -> day04_2
    "4" : _ -> do
      day04_1
      day04_2
    "5" : "1" : _ -> day05_1
    "5" : "2" : _ -> day05_2
    "5" : _ -> do
      day05_1
      day05_2
    "6" : "1" : _ -> day06_1
    "6" : "2" : _ -> day06_2
    "6" : _ -> do
      day06_1
      day06_2
    "7" : "1" : _ -> day07_1
    "7" : "2" : _ -> day07_2
    "7" : _ -> do
      day07_1
      day07_2
    "8" : "1" : _ -> day08_1
    "8" : "2" : _ -> day08_2
    "8" : _ -> do
      day08_1
      day08_2
    "9" : "1" : _ -> day09_1
    "9" : "2" : _ -> day09_2
    "9" : _ -> do
      day09_1
      day09_2
    "10" : "1" : _ -> day10_1
    "10" : "2" : _ -> day10_2
    "10" : _ -> do
      day10_1
      day10_2
    "11" : "1" : _ -> day11_1
    "11" : "2" : _ -> day11_2
    "11" : _ -> do
      day11_1
      day11_2
    "12" : "1" : _ -> day12_1
    "13" : "1" : _ -> day13_1
    "13" : "2" : _ -> day13_2
    "13" : _ -> do
      day13_1
      day13_2
    "14" : "1" : _ -> day14_1
    "14" : "2" : _ -> day14_2
    "14" : _ -> do
      day14_1
      day14_2
    "15" : "1" : _ -> day15_1
    "17" : "1" : _ -> day17_1
    "17" : "2" : _ -> day17_2
    "17" : _ -> do
      day17_1
      day17_2
    "18" : "1" : _ -> day18_1
    "18" : "2" : _ -> day18_2
    "18" : _ -> do
      day18_1
      day18_2
    "19" : "1" : _ -> day19_1
    "22" : "1" : _ -> day22_1
    _ -> error "Not implemented"
