blinkStone :: Int -> [Int]
blinkStone 0 = [1]
blinkStone s = let ss = show s
                   nDigit = length ss
               in  if even nDigit
                      then map read [take (nDigit `div` 2) ss, drop (nDigit `div` 2) ss]
                   else [s * 2024]

blink :: Int -> [Int] -> [Int]
blink 0 xs = xs
blink n xs = blink (n - 1) $ concatMap blinkStone xs

main = do
    contents <- words <$> readFile "day11.txt"
    print . length . blink 25 $ map read contents 
