{-# OPTIONS_GHC -Wno-x-partial #-}

module Day08
  ( day08_1,
    day08_2,
  )
where

import Control.Applicative
import Data.Bifunctor (bimap)
import Data.Set (fromList)

type Freq = Char

type Coords = (Int, Int)

data Antenna = Antenna
  { frequency :: Freq,
    coordinates :: Coords
  }
  deriving (Show, Eq)

readAntenna :: Freq -> Coords -> Antenna
readAntenna freq coords = Antenna {frequency = freq, coordinates = coords}

getAntennas :: [String] -> [Antenna]
getAntennas grid = concat . getZipList $ getAntennasRow <$> ZipList [0 ..] <*> ZipList grid
  where
    getAntennasRow n row = [readAntenna x (n, y) | (x, y) <- zip row [0 ..], x /= '.']

isInside :: Coords -> Int -> Int -> Bool
isInside c x y = fst c >= 0 && fst c < x && snd c >= 0 && snd c < y

generateCoords :: Coords -> Coords -> [Coords]
generateCoords c offset = scanl shiftCoords c (repeat offset)
  where
    shiftCoords x = bimap (fst x +) (snd x +)

getAntinodes :: Antenna -> Antenna -> Int -> Int -> [Coords]
getAntinodes a b maxX maxY =
  let xa = fst $ coordinates a
      ya = snd $ coordinates a
      xb = fst $ coordinates b
      yb = snd $ coordinates b
   in if frequency a /= frequency b || coordinates a == coordinates b
        then []
        else filter (\c -> isInside c maxX maxY) [(2 * xa - xb, 2 * ya - yb), (2 * xb - xa, 2 * yb - ya)]

getAntinodes' :: Antenna -> Antenna -> Int -> Int -> [Coords]
getAntinodes' a b maxX maxY =
  let xa = fst $ coordinates a
      ya = snd $ coordinates a
      xb = fst $ coordinates b
      yb = snd $ coordinates b
      distX = xa - xb
      distY = ya - yb
   in if frequency a /= frequency b || coordinates a == coordinates b
        then []
        else
          filter (\c -> isInside c maxX maxY) [(2 * xa - xb, 2 * ya - yb), (2 * xb - xa, 2 * yb - ya)]
            ++ takeWhile (\c -> isInside c maxX maxY) (generateCoords (coordinates a) (distX, distY))
            ++ takeWhile (\c -> isInside c maxX maxY) (generateCoords (coordinates b) (-distX, -distY))

parseInput :: IO (Int, Int, [Antenna])
parseInput = do
  contents <- lines <$> readFile "input/day8.txt"
  let antennas = getAntennas contents
      x = length contents
      y = length $ head contents
  return (x, y, antennas)

day08_1 :: IO ()
day08_1 = do
  (x, y, antennas) <- parseInput
  let antinodes = fromList $ concat [getAntinodes a b x y | a <- antennas, b <- antennas, a /= b, frequency a == frequency b]
  putStrLn $ "Day 8, Puzzle 1 solution: " ++ show (length antinodes)

day08_2 :: IO ()
day08_2 = do
  (x, y, antennas) <- parseInput
  let antinodes = fromList $ concat [getAntinodes' a b x y | a <- antennas, b <- antennas, a /= b, frequency a == frequency b]
  putStrLn $ "Day 8, Puzzle 2 solution: " ++ show (length antinodes)
