{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day19 (day19_1) where

import Control.Monad (guard)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)

getDesignCombinations :: String -> [String] -> String -> [String]
getDesignCombinations design towels cur = do
  towel <- towels
  guard $ towel `isPrefixOf` design

  let design' = drop (length towel) design
  if null design'
    then return $ drop 1 cur ++ "," ++ towel
    else getDesignCombinations design' towels (cur ++ "," ++ towel)

isDesignPossible :: String -> [String] -> Bool
isDesignPossible design towels =
  let combinations = getDesignCombinations design towels ""
   in (not . null) combinations

day19_1 :: IO ()
day19_1 = do
  contents <- lines <$> readFile "input/day19.txt"
  let [[ts], designs] = splitOn [""] contents
      towels = splitOn ", " ts
  putStrLn $
    "Day 19, Puzzle 1 solution: "
      ++ show (length . filter id $ map (`isDesignPossible` towels) designs)
