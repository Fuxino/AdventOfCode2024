import Data.List ((\\))
import Data.List.Split (splitOn)

isSorted :: [(Int, Int)] -> [Int] -> Bool
isSorted rules [x] = True
isSorted rules (x:xs) = let after = [ p | (p, n) <- rules, n == x ]
                        in  not (any (`elem` after) xs) && isSorted rules xs

getMiddle :: [Int] -> Int 
getMiddle xs = xs !! (length xs `div` 2)

sortOnRules :: [(Int, Int)] -> [Int] -> [Int]
sortOnRules _ [] = []
sortOnRules rules (x:xs) = sortOnRules rules beforeArray ++ [x] ++ sortOnRules rules afterArray
                               where afterArray  = xs \\ before
                                     beforeArray = xs \\ afterArray
                                     before      = [ p | (p, n) <- rules, n  == x ]

main = do
    contents <- map (splitOn "|") . lines <$> readFile "day5.txt"
    let rules = [ (read x, read y) | [x, y] <- takeWhile (/= [""]) contents ]
        unsorted = filter (not . isSorted rules) . map (map read) $ concatMap (map (splitOn ",")) . drop 1 $ dropWhile (/= [""]) contents
        fixUnsorted = map (sortOnRules rules) unsorted
    print . sum $ map getMiddle fixUnsorted
