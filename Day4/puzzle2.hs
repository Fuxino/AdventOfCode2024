import Data.List (transpose, isPrefixOf, tails)

diagonals :: [String] -> [String]
diagonals xs = diagonals' xs ++ diagonals' ((transpose . reverse) xs)
                   where diagonals' xs = transpose (zipWith drop [0..] xs)
                                         ++ transpose (zipWith drop [1..] (transpose xs))

countOccurrences :: String -> [String] -> Int
countOccurrences word text = sum (map (countOccurrences' word) diags) + sum (map (countOccurrences' word . reverse) diags)
                                 where diags = diagonals text
                                       countOccurrences' _ [] = 0
                                       countOccurrences' word text@(_:rest) = if word `isPrefixOf` text
                                                                                  then 1 + countOccurrences' word rest
                                                                              else countOccurrences' word rest

submatricesVert :: Int -> [String] -> [[String]]
submatricesVert _ [] = []
submatricesVert _ [xs] = []
submatricesVert _ [xs, ys] = []
submatricesVert n matrix@(xs:xxs) = submatrix matrix ++ submatricesVert n xxs
                                    where submatrix matrix = [take n $ map (take n) matrix]

main = do
    contents <- lines <$> readFile "day4.txt"
    let  xmas = length . filter (\x -> countOccurrences "MAS" x == 2) . concatMap (submatricesVert 3) . transpose $ map tails contents
    print xmas
