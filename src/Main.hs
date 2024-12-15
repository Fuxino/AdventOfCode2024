module Main (main) where

import Day1.Puzzle1
import Day1.Puzzle2
import Day2.Puzzle1
import Day2.Puzzle2
import Day3.Puzzle1
import Day3.Puzzle2
import Day4.Puzzle1
import Day4.Puzzle2
import Day5.Puzzle1
import Day5.Puzzle2
import Day6.Puzzle1
import Day6.Puzzle2
import Day7.Puzzle1
import Day7.Puzzle2
import Day8.Puzzle1
import Day8.Puzzle2
import Day9.Puzzle1
import Day9.Puzzle2
import Day10.Puzzle1
import Day11.Puzzle1
import Day11.Puzzle2
import Day12.Puzzle1
import Day13.Puzzle1
import Day13.Puzzle2
import Day14.Puzzle1
import Day15.Puzzle1
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
         "1":"1":_  -> day1_1
         "1":"2":_  -> day1_2
         "2":"1":_  -> day2_1
         "2":"2":_  -> day2_2
         "3":"1":_  -> day3_1
         "3":"2":_  -> day3_2
         "4":"1":_  -> day4_1
         "4":"2":_  -> day4_2
         "5":"1":_  -> day5_1
         "5":"2":_  -> day5_2
         "6":"1":_  -> day6_1
         "6":"2":_  -> day6_2
         "7":"1":_  -> day7_1
         "7":"2":_  -> day7_2
         "8":"1":_  -> day8_1
         "8":"2":_  -> day8_2
         "9":"1":_  -> day9_1
         "9":"2":_  -> day9_2
         "10":"1":_ -> day10_1
         "11":"1":_ -> day11_1
         "11":"2":_ -> day11_2
         "12":"1":_ -> day12_1
         "13":"1":_ -> day13_1
         "13":"2":_ -> day13_2
         "14":"1":_ -> day14_1
         "15":"1":_ -> day15_1
         _          -> error "Not implemented"
