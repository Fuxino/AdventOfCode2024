import Data.List.Split (splitOn)

isSorted :: [(String, String)] -> [String] -> Bool
isSorted rules [x] = True
isSorted rules (x:xs) = let after = [ p | (p, n) <- rules, n == x ]
                        in  not (any (`elem` after) xs) && isSorted rules xs

getMiddle :: [String] -> String
getMiddle xs = xs !! (length xs `div` 2)

main = do
    contents <- map (splitOn "|") . lines <$> readFile "day5.txt"
    let rules = [ (x, y) | [x, y] <- takeWhile (/= [""]) contents ]
        updates = concatMap (map (splitOn ",")) . tail $ dropWhile (/= [""]) contents
        sorted = filter (isSorted rules) updates
    print . sum $ map (read . getMiddle) sorted
