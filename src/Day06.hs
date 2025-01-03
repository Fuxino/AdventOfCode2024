{-# OPTIONS_GHC -Wno-x-partial #-}

module Day06
  ( day06_1,
    day06_2,
  )
where

import Data.List (elemIndex)
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

printDirection :: Direction -> Char
printDirection U = '^'
printDirection R = '>'
printDirection D = 'v'
printDirection L = '<'

getStartPosition :: Char -> Grid -> Position
getStartPosition c grid = (x, y)
  where
    x = fromMaybe (-1) . elemIndex True . map (isJust . elemIndex c) $ grid
    y = if x == -1 then -1 else fromMaybe (-1) . elemIndex c $ grid !! x

getGridVal :: Position -> Grid -> Char
getGridVal (x, y) grid = (grid !! x) !! y

isInside :: Position -> Grid -> Bool
isInside (x, y) grid = x >= 0 && y >= 0 && x < length grid && y < length (head grid)

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
  let gridVal = getGridVal (x, y) grid
   in if gridVal == '#' || gridVal == '^' || gridVal == '>' || gridVal == 'v' || gridVal == '<'
        then grid
        else
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

checkGridLoop :: Position -> Direction -> Grid -> Bool
checkGridLoop startPosition direction grid =
  let (nextPosition, newDirection) = getNextPosition startPosition direction grid
      newDirectionChar = printDirection newDirection
      newGrid = markVisited nextPosition newDirectionChar grid
   in (nextPosition `isInside` grid)
        && ( (getGridVal nextPosition grid == newDirectionChar)
               || checkGridLoop nextPosition newDirection newGrid
           )

setGridObstacles :: Position -> Grid -> [Grid]
setGridObstacles startPosition grid =
  let positions = [(x, y) | x <- [0 .. (length grid - 1)], y <- [0 .. (length (head grid) - 1)], (x, y) /= startPosition, getGridVal (x, y) grid == 'X']
   in zipWith (`markVisited` '#') positions (replicate (length positions) grid)

parseInput :: IO (Int, Int, Direction, [String])
parseInput = do
  contents <- lines <$> readFile "input/day6.txt"
  let (x, y) =
        (\a b c d -> head $ filter ((>= 0) . fst) [a, b, c, d])
          <$> getStartPosition 'v'
          <*> getStartPosition '^'
          <*> getStartPosition '<'
          <*> getStartPosition '>'
          $ contents
      direction = fromJust . getDirection $ (contents !! x) !! y
  return (x, y, direction, contents)

day06_1 :: IO ()
day06_1 = do
  (x, y, direction, grid) <- parseInput
  putStrLn $
    "Day 6, Puzzle 1 solution: "
      ++ show (1 + (length . concatMap (filter (== 'X')) $ visitGrid (x, y) direction grid))

day06_2 :: IO ()
day06_2 = do
  (x, y, direction, initialGrid) <- parseInput
  let grid = visitGrid (x, y) direction initialGrid
      gridObstacles = setGridObstacles (x, y) grid
      loops = filter (checkGridLoop (x, y) direction) gridObstacles
  putStrLn $ "Day 6, Puzzle 2 solution: " ++ show (length loops)
