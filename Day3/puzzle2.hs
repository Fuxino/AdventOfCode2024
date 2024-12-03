import Data.List (isPrefixOf)
import Data.List.Split (split, splitOn, startsWith)
import Data.Char (isDigit)
import Text.Regex.TDFA (getAllTextMatches, (=~))

sumMul :: [String] -> Int
sumMul xs = let vals = map (splitOn "," . filter (`elem` "0123456789,")) xs
            in  sum $ map (product . map read) vals

filterDonts :: [String] -> String
filterDonts = concat . filter (not . isPrefixOf "don't()") . concatMap (split (startsWith "do()"))

main = do
    contents <- split (startsWith "don't()") <$> readFile "day3.txt"
    let mults = getAllTextMatches (filterDonts contents =~ "mul\\([0-9]+,[0-9]+\\)") :: [String]
    print $ sumMul mults
