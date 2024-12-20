module Day04
  ( day04_1,
    day04_2,
  )
where

import Data.List (isPrefixOf, tails, transpose)

diagonals :: [String] -> [String]
diagonals xs = diagonals' xs ++ diagonals' ((transpose . reverse) xs)
  where
    diagonals' x =
      transpose (zipWith drop [0 ..] x)
        ++ transpose (zipWith drop [1 ..] (transpose x))

countOccurrences :: String -> [String] -> Int
countOccurrences word text =
  sum (map (countOccurrences' word) text)
    + sum (map (countOccurrences' word . reverse) text)
    + sum (map (countOccurrences' word) cols)
    + sum (map (countOccurrences' word . reverse) cols)
    + sum (map (countOccurrences' word) diags)
    + sum (map (countOccurrences' word . reverse) diags)
  where
    cols = transpose text
    diags = diagonals text
    countOccurrences' _ [] = 0
    countOccurrences' w txt@(_ : rest) =
      if w `isPrefixOf` txt
        then 1 + countOccurrences' word rest
        else countOccurrences' w rest

countOccurrencesX :: String -> [String] -> Int
countOccurrencesX word text =
  sum (map (countOccurrencesX' word) diags)
    + sum (map (countOccurrencesX' word . reverse) diags)
  where
    diags = diagonals text
    countOccurrencesX' _ [] = 0
    countOccurrencesX' w txt@(_ : rest) =
      if w `isPrefixOf` txt
        then 1 + countOccurrencesX' w rest
        else countOccurrencesX' w rest

submatricesVert :: Int -> [String] -> [[String]]
submatricesVert _ [] = []
submatricesVert _ [_] = []
submatricesVert _ [_, _] = []
submatricesVert n matrix@(_ : xxs) = submatrix matrix ++ submatricesVert n xxs
  where
    submatrix m = [take n $ map (take n) m]

day04_1 :: IO ()
day04_1 = do
  contents <- lines <$> readFile "input/day4.txt"
  putStrLn $
    "Day 4, Puzzle 1 solution: "
      ++ show (countOccurrences "XMAS" contents)

day04_2 :: IO ()
day04_2 = do
  contents <- lines <$> readFile "input/day4.txt"
  let xmas = length . concatMap (filter (\x -> countOccurrencesX "MAS" x == 2) . submatricesVert 3) . transpose $ map tails contents
  putStrLn $ "Day 4, Puzzle 2 solution: " ++ show xmas
