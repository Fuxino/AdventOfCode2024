{-# OPTIONS_GHC -Wno-type-defaults #-}

module Day13.Puzzle1 (day13_1) where

import Data.Char (isDigit)
import Data.Either (fromRight)
import Data.List (uncons)
import Data.List.Split (chunksOf, splitOn)
import Data.Matrix (Matrix, fromLists, rref, toList, zero)
import Data.Maybe (fromJust, mapMaybe)

isAlmostInt :: (RealFrac a) => a -> Bool
isAlmostInt x =
  let diff = x - fromInteger (round x)
   in abs diff < 0.001

getMatrix :: (Read a) => String -> Matrix a
getMatrix s =
  let nValues = map (map read . splitOn ",") . splitOn ":" . drop 1 $ filter (\x -> isDigit x || x == ',' || x == ':') s
      eq1 = map (fst . fromJust . uncons) nValues
      eq2 = map last nValues
   in fromLists [eq1, eq2]

solve :: (RealFrac a) => Matrix a -> Maybe [a]
solve eqSystem =
  let rowEchelonList = toList . fromRight (zero 1 1) $ rref eqSystem
      solutions = [rowEchelonList !! 2, rowEchelonList !! 5]
   in if all isAlmostInt solutions
        then Just solutions
        else Nothing

cost :: [Int] -> Int
cost [x, y] = 3 * x + y
cost _ = 0

day13_1 :: IO ()
day13_1 = do
  contents <- map concat . chunksOf 4 . lines <$> readFile "input/day13.txt"
  let eqSystems = map getMatrix contents
      solutions = (map . map) round $ mapMaybe solve eqSystems
  putStrLn $
    "Day 13, Puzzle 1 solution: "
      ++ show (sum $ map cost solutions)
