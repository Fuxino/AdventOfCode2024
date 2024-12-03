import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Text.Regex.TDFA (getAllTextMatches, (=~))

sumMul :: [String] -> Int
sumMul xs = let vals = map (splitOn "," . filter (`elem` "0123456789,")) xs
            in  sum $ map (product . map read) vals

main = do
    contents <- readFile "day3.txt"
    let mults = getAllTextMatches (contents =~ "mul\\([0-9]+,[0-9]+\\)") :: [String]
    print $ sumMul mults
