import Data.List (transpose, isPrefixOf)

diagonals :: [String] -> [String]
diagonals xs = diagonals' xs ++ diagonals' ((transpose . reverse) xs)
               where diagonals' xs = transpose (zipWith drop [0..] xs)
                                     ++ transpose (zipWith drop [1..] (transpose xs))

countOccurrences :: String -> [String] -> Int
countOccurrences word text = sum (map (countOccurrences' word) text) + sum (map (countOccurrences' word . reverse) text)
                            + sum (map (countOccurrences' word) cols) + sum (map (countOccurrences' word . reverse) cols)
                            + sum (map (countOccurrences' word) diags) + sum (map (countOccurrences' word . reverse) diags)
                            where cols  = transpose text
                                  diags = diagonals text
                                  countOccurrences' _ [] = 0
                                  countOccurrences' word text@(_:rest) = if word `isPrefixOf` text
                                                                             then 1 + countOccurrences' word rest
                                                                         else countOccurrences' word rest
    
main = do
    contents <- lines <$> readFile "day4.txt"
    print $ countOccurrences "XMAS" contents
