module Day22 (day22_1) where

import Data.Bits

getSecretNumber :: Integer -> Int -> Integer
getSecretNumber x 0 = x
getSecretNumber x n =
  let x' = ((x `shiftL` 6) `xor` x) `mod` 16777216
      x'' = ((x' `shiftR` 5) `xor` x') `mod` 16777216
   in getSecretNumber (((x'' `shiftL` 11) `xor` x'') `mod` 16777216) (n - 1)

day22_1 :: IO ()
day22_1 = do
  contents <- map read . lines <$> readFile "input/day22.txt"
  putStrLn $
    "Day 22, Puzzle 1 solution: "
      ++ show (sum $ map (`getSecretNumber` 2000) contents)
