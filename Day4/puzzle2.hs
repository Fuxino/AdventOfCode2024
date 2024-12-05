import Data.List (transpose, isPrefixOf, tails)

diagonals :: [String] -> [String]
diagonals xs = diagonals' xs ++ diagonals' ((transpose . reverse) xs)
               where diagonals' xs = transpose (zipWith drop [0..] xs)
                                     ++ transpose (zipWith drop [1..] (transpose xs))

countSubstrings :: String -> [String] -> Int
countSubstrings word text = sum (map (countSubstrings' word) diags) + sum (map (countSubstrings' word . reverse) diags)
                            where diags = diagonals text
                                  countSubstrings' _ [] = 0
                                  countSubstrings' word text@(_:rest) = if word `isPrefixOf` text
                                                                           then 1 + countSubstrings' word rest
                                                                           else countSubstrings' word rest

submatricesVert :: Int -> [String] -> [[String]]
submatricesVert _ [] = []
submatricesVert _ [xs] = []
submatricesVert _ [xs, ys] = []
submatricesVert n matrix@(xs:xxs) = submatrix matrix ++ submatricesVert n xxs
                                    where submatrix matrix = [take n $ map (take n) matrix]

main = do
    contents <- lines <$> readFile "day4.txt"
    let  xmas = length . filter (\x -> countSubstrings "MAS" x == 2) . concatMap (submatricesVert 3) . transpose $ map tails contents
    print xmas
