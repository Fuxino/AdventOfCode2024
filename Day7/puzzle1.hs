import Data.List (transpose)
import Data.List.Split (splitOn)

type Equation = (Int, [Int])

isSolvable :: Int -> Equation -> Bool
isSolvable cur (result, []) = cur == result
isSolvable cur (result, [x]) = cur + x == result || cur * x == result
isSolvable cur (result, x:y:ys) = isSolvable (cur + x + y) (result, ys)
                                  || isSolvable ((cur + x) * y) (result, ys)
                                  || isSolvable (cur * x + y) (result, ys)
                                  || isSolvable (cur * x * y) (result, ys)

main = do
    [x, y] <- transpose . map (splitOn ":") . lines <$> readFile "day7.txt"
    let results = map read x
        values = map read <$> map words y
        equations = zip results values
    print . sum . map fst $ filter (isSolvable 0) equations
