module Main (main) where

import Day01.Puzzle1
import Day01.Puzzle2
import Day02.Puzzle1
import Day02.Puzzle2
import Day03.Puzzle1
import Day03.Puzzle2
import Day04.Puzzle1
import Day04.Puzzle2
import Day05.Puzzle1
import Day05.Puzzle2
import Day06.Puzzle1
import Day06.Puzzle2
import Day07.Puzzle1
import Day07.Puzzle2
import Day08.Puzzle1
import Day08.Puzzle2
import Day09.Puzzle1
import Day09.Puzzle2
import Day10.Puzzle1
import Day11.Puzzle1
import Day11.Puzzle2
import Day12.Puzzle1
import Day13.Puzzle1
import Day13.Puzzle2
import Day14.Puzzle1
import Day14.Puzzle2
import Day15.Puzzle1
import Day17.Puzzle1
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "1" : "1" : _ -> day01_1
    "1" : "2" : _ -> day01_2
    "2" : "1" : _ -> day02_1
    "2" : "2" : _ -> day02_2
    "3" : "1" : _ -> day03_1
    "3" : "2" : _ -> day03_2
    "4" : "1" : _ -> day04_1
    "4" : "2" : _ -> day04_2
    "5" : "1" : _ -> day05_1
    "5" : "2" : _ -> day05_2
    "6" : "1" : _ -> day06_1
    "6" : "2" : _ -> day06_2
    "7" : "1" : _ -> day07_1
    "7" : "2" : _ -> day07_2
    "8" : "1" : _ -> day08_1
    "8" : "2" : _ -> day08_2
    "9" : "1" : _ -> day09_1
    "9" : "2" : _ -> day09_2
    "10" : "1" : _ -> day10_1
    "11" : "1" : _ -> day11_1
    "11" : "2" : _ -> day11_2
    "12" : "1" : _ -> day12_1
    "13" : "1" : _ -> day13_1
    "13" : "2" : _ -> day13_2
    "14" : "1" : _ -> day14_1
    "14" : "2" : _ -> day14_2
    "15" : "1" : _ -> day15_1
    "17" : "1" : _ -> day17_1
    _ -> error "Not implemented"
