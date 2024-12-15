module Day6.Puzzle1 (day6_1) where

import Data.List (elemIndex, uncons)
import Data.Maybe (fromJust, fromMaybe, isJust)

type Grid = [String]

type Position = (Int, Int)

data Direction = U | R | D | L deriving (Eq)

getDirection :: Char -> Maybe Direction
getDirection '^' = Just U
getDirection '>' = Just R
getDirection 'v' = Just D
getDirection '<' = Just L
getDirection _ = Nothing

getStartPosition :: Char -> Grid -> Position
getStartPosition c grid = (x, y)
  where
    x = fromMaybe (-1) . elemIndex True . map (isJust . elemIndex c) $ grid
    y = if x == -1 then -1 else fromMaybe (-1) . elemIndex c $ grid !! x

getGridVal :: Position -> Grid -> Char
getGridVal (x, y) grid = (grid !! x) !! y

isInside :: Position -> Grid -> Bool
isInside (x, y) grid = x >= 0 && y >= 0 && x < length grid && y < length (fst . fromJust $ uncons grid)

getNextPosition :: Position -> Direction -> Grid -> (Position, Direction)
getNextPosition (x, y) U grid =
  let newPos = (x - 1, y)
      gridVal = getGridVal newPos grid
   in if newPos `isInside` grid && gridVal == '#'
        then getNextPosition (x, y) R grid
        else (newPos, U)
getNextPosition (x, y) R grid =
  let newPos = (x, y + 1)
      gridVal = getGridVal newPos grid
   in if newPos `isInside` grid && gridVal == '#'
        then getNextPosition (x, y) D grid
        else (newPos, R)
getNextPosition (x, y) D grid =
  let newPos = (x + 1, y)
      gridVal = getGridVal newPos grid
   in if newPos `isInside` grid && gridVal == '#'
        then getNextPosition (x, y) L grid
        else (newPos, D)
getNextPosition (x, y) L grid =
  let newPos = (x, y - 1)
      gridVal = getGridVal newPos grid
   in if newPos `isInside` grid && gridVal == '#'
        then getNextPosition (x, y) U grid
        else (newPos, L)

markVisited :: Position -> Char -> Grid -> Grid
markVisited (x, y) c grid =
  let row = grid !! x
      newRow = take y row ++ [c] ++ drop (y + 1) row
   in take x grid ++ [newRow] ++ drop (x + 1) grid

visitGrid :: Position -> Direction -> Grid -> Grid
visitGrid (x, y) direction grid =
  let newGrid = markVisited (x, y) 'X' grid
      (nextPosition, newDirection) = getNextPosition (x, y) direction grid
   in if nextPosition `isInside` newGrid
        then visitGrid nextPosition newDirection newGrid
        else newGrid

day6_1 :: IO ()
day6_1 = do
  contents <- lines <$> readFile "input/day6.txt"
  let (x, y) =
        (\a b c d -> fst . fromJust $ uncons $ filter ((>= 0) . fst) [a, b, c, d])
          <$> getStartPosition 'v'
          <*> getStartPosition '^'
          <*> getStartPosition '<'
          <*> getStartPosition '>'
          $ contents
      direction = fromJust . getDirection $ (contents !! x) !! y
  putStrLn $
    "Day 6, Puzzle 1 solution: "
      ++ show (length . concatMap (filter (== 'X')) $ visitGrid (x, y) direction contents)
