module Day4.Puzzle2 (day4_2) where

import Data.List (transpose, isPrefixOf, tails)

diagonals :: [String] -> [String]
diagonals xs = diagonals' xs ++ diagonals' ((transpose . reverse) xs)
                   where diagonals' x = transpose (zipWith drop [0..] x)
                                        ++ transpose (zipWith drop [1..] (transpose x))

countOccurrences :: String -> [String] -> Int
countOccurrences word text = sum (map (countOccurrences' word) diags) + sum (map (countOccurrences' word . reverse) diags)
                                 where diags = diagonals text
                                       countOccurrences' _ [] = 0
                                       countOccurrences' w txt@(_:rest) = if w `isPrefixOf` txt
                                                                              then 1 + countOccurrences' w rest
                                                                          else countOccurrences' w rest

submatricesVert :: Int -> [String] -> [[String]]
submatricesVert _ [] = []
submatricesVert _ [_] = []
submatricesVert _ [_, _] = []
submatricesVert n matrix@(_:xxs) = submatrix matrix ++ submatricesVert n xxs
                                    where submatrix m = [take n $ map (take n) m]

day4_2 :: IO ()
day4_2 = do
    contents <- lines <$> readFile "input/day4.txt"
    let  xmas = length . filter (\x -> countOccurrences "MAS" x == 2) . concatMap (submatricesVert 3) . transpose $ map tails contents
    putStrLn $ "Day 4, Puzzle 2 solution: " ++ show xmas
