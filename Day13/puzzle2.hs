import Data.Char (isDigit)
import Data.List.Split (splitOn, chunksOf)
import Data.Matrix (Matrix, fromLists, toList, rref, zero)
import Data.Either (fromRight)
import Data.Maybe (mapMaybe)

isAlmostInt :: (RealFrac a) => a -> Bool
isAlmostInt x = let diff = x - fromInteger (round x)
                in  abs diff < 0.001

multRes :: (Num a) => [a] -> [a]
multRes [x, y, z] = [x, y, z + 10000000000000]

getMatrix :: (Num a, Read a) => String -> Matrix a
getMatrix s = let nValues = map (map read . splitOn ",") . splitOn ":" . drop 1 $ filter (\x -> isDigit x || x == ',' || x == ':') s
                  eq1 = multRes $ map head nValues
                  eq2 = multRes $ map last nValues
              in  fromLists [eq1, eq2]

solve :: (RealFrac a) => Matrix a -> Maybe [a]
solve eqSystem = let rowEchelonList = toList . fromRight (zero 1 1) $ rref eqSystem
                     solutions = [ rowEchelonList !! 2, rowEchelonList !! 5 ]
                 in  if all isAlmostInt solutions
                        then Just solutions
                     else Nothing

cost :: [Int] -> Int
cost [x, y] = 3 * x + y

main = do
    contents <- map concat . chunksOf 4 . lines <$> readFile "day13.txt"
    let eqSystems = map getMatrix contents
        solutions = (map . map) round $ mapMaybe solve eqSystems
    print . sum $ map cost solutions
