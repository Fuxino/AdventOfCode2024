import Data.List (transpose, isPrefixOf)

diagonals :: [String] -> [String]
diagonals xs = diagonals' xs ++ diagonals' ((transpose . reverse) xs)
               where diagonals' xs = transpose (zipWith drop [0..] xs)
                                     ++ transpose (zipWith drop [1..] (transpose xs))

countSubstrings :: String -> [String] -> Int
countSubstrings word text = sum (map (countSubstrings' word) text) + sum (map (countSubstrings' word . reverse) text)
                            + sum (map (countSubstrings' word) cols) + sum (map (countSubstrings' word . reverse) cols)
                            + sum (map (countSubstrings' word) diags) + sum (map (countSubstrings' word . reverse) diags)
                            where cols  = transpose text
                                  diags = diagonals text
                                  countSubstrings' _ [] = 0
                                  countSubstrings' word text@(_:rest) = if word `isPrefixOf` text
                                                                           then 1 + countSubstrings' word rest
                                                                           else countSubstrings' word rest
    
main = do
    contents <- lines <$> readFile "day4.txt"
    print $ countSubstrings "XMAS" contents
