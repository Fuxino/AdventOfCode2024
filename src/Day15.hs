{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day15 (day15_1) where

import Data.List (elemIndex, elemIndices, transpose, uncons)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe, isJust)

type Grid = [String]

type Position = (Int, Int)

data Direction = U | R | D | L deriving (Eq, Show)

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

moveUp :: Position -> Position
moveUp (x, y) = (x - 1, y)

moveRight :: Position -> Position
moveRight (x, y) = (x, y + 1)

moveDown :: Position -> Position
moveDown (x, y) = (x + 1, y)

moveLeft :: Position -> Position
moveLeft (x, y) = (x, y - 1)

markPosition :: Position -> Char -> Grid -> Grid
markPosition (x, y) c grid =
  let row = grid !! x
      newRow = take y row ++ [c] ++ drop (y + 1) row
   in take x grid ++ [newRow] ++ drop (x + 1) grid

getValsUp :: Position -> Grid -> String
getValsUp (x, y) grid = takeWhile (/= '#') . reverse . take x $ transpose grid !! y

getValsDown :: Position -> Grid -> String
getValsDown (x, y) grid = takeWhile (/= '#') . drop (x + 1) $ transpose grid !! y

getValsLeft :: Position -> Grid -> String
getValsLeft (x, y) grid = takeWhile (/= '#') . reverse . take y $ grid !! x

getValsRight :: Position -> Grid -> String
getValsRight (x, y) grid = takeWhile (/= '#') . drop (y + 1) $ grid !! x

shiftUp :: Position -> Grid -> Grid
shiftUp p1 grid =
  let p2 = moveUp p1
      p3 = moveUp p2
      v = getGridVal p3 grid
   in if v == '#'
        then grid
        else
          if v == '.'
            then markPosition p1 '.' (markPosition p2 '@' (markPosition p3 'O' grid))
            else
              let column = reverse $ transpose grid !! snd p1
                  nextDot = fst . fromMaybe (-1, []) $ uncons $ dropWhile (< length column - fst p1) $ elemIndices '.' column
                  nextHash = fst . fromMaybe (-1, []) $ uncons $ dropWhile (< length column - fst p1) $ elemIndices '#' column
               in if nextDot == -1 || nextDot > nextHash
                    then grid
                    else markPosition p1 '.' (markPosition p2 '@' (markPosition (length column - 1 - nextDot, snd p1) 'O' grid))

shiftRight :: Position -> Grid -> Grid
shiftRight p1 grid =
  let p2 = moveRight p1
      p3 = moveRight p2
      v = getGridVal p3 grid
   in if v == '#'
        then grid
        else
          if v == '.'
            then markPosition p1 '.' (markPosition p2 '@' (markPosition p3 'O' grid))
            else
              let row = grid !! fst p1
                  nextDot = fst . fromMaybe (-1, []) $ uncons $ dropWhile (< snd p1) $ elemIndices '.' row
                  nextHash = fst . fromMaybe (-1, []) $ uncons $ dropWhile (< snd p1) $ elemIndices '#' row
               in if nextDot == -1 || nextDot > nextHash
                    then grid
                    else markPosition p1 '.' (markPosition p2 '@' (markPosition (fst p1, nextDot) 'O' grid))

shiftDown :: Position -> Grid -> Grid
shiftDown p1 grid =
  let p2 = moveDown p1
      p3 = moveDown p2
      v = getGridVal p3 grid
   in if v == '#'
        then grid
        else
          if v == '.'
            then markPosition p1 '.' (markPosition p2 '@' (markPosition p3 'O' grid))
            else
              let column = transpose grid !! snd p1
                  nextDot = fst . fromMaybe (-1, []) $ uncons $ dropWhile (< fst p1) $ elemIndices '.' column
                  nextHash = fst . fromMaybe (-1, []) $ uncons $ dropWhile (< fst p1) $ elemIndices '#' column
               in if nextDot == -1 || nextDot > nextHash
                    then grid
                    else markPosition p1 '.' (markPosition p2 '@' (markPosition (nextDot, snd p1) 'O' grid))

shiftLeft :: Position -> Grid -> Grid
shiftLeft p1 grid =
  let p2 = moveLeft p1
      p3 = moveLeft p2
      v = getGridVal p3 grid
   in if v == '#'
        then grid
        else
          if v == '.'
            then markPosition p1 '.' (markPosition p2 '@' (markPosition p3 'O' grid))
            else
              let row = reverse $ grid !! fst p1
                  nextDot = fst . fromMaybe (-1, []) $ uncons $ dropWhile (< length row - snd p1) $ elemIndices '.' row
                  nextHash = fst . fromMaybe (-1, []) $ uncons $ dropWhile (< length row - snd p1) $ elemIndices '#' row
               in if nextDot == -1 || nextDot > nextHash
                    then grid
                    else markPosition p1 '.' (markPosition p2 '@' (markPosition (fst p1, length row - 1 - nextDot) 'O' grid))

move :: Position -> Direction -> Grid -> (Position, Grid)
move p U grid =
  let newP = moveUp p
      newV = getGridVal newP grid
   in if newV == '#'
        then (p, grid)
        else
          if newV == '.'
            then (newP, markPosition p '.' (markPosition newP '@' grid))
            else
              let valsUp = getValsUp p grid
               in if '.' `notElem` valsUp
                    then (p, grid)
                    else (newP, shiftUp p grid)
move p R grid =
  let newP = moveRight p
      newV = getGridVal newP grid
   in if newV == '#'
        then (p, grid)
        else
          if newV == '.'
            then (newP, markPosition p '.' (markPosition newP '@' grid))
            else
              let valsRight = getValsRight p grid
               in if '.' `notElem` valsRight
                    then (p, grid)
                    else (newP, shiftRight p grid)
move p D grid =
  let newP = moveDown p
      newV = getGridVal newP grid
   in if newV == '#'
        then (p, grid)
        else
          if newV == '.'
            then (newP, markPosition p '.' (markPosition newP '@' grid))
            else
              let valsDown = getValsDown p grid
               in if '.' `notElem` valsDown
                    then (p, grid)
                    else (newP, shiftDown p grid)
move p L grid =
  let newP = moveLeft p
      newV = getGridVal newP grid
   in if newV == '#'
        then (p, grid)
        else
          if newV == '.'
            then (newP, markPosition p '.' (markPosition newP '@' grid))
            else
              let valsLeft = getValsLeft p grid
               in if '.' `notElem` valsLeft
                    then (p, grid)
                    else (newP, shiftLeft p grid)

visitGrid :: Position -> [Direction] -> Grid -> Grid
visitGrid _ [] grid = grid
visitGrid p (d : ds) grid =
  let (newP, grid') = move p d grid
   in visitGrid newP ds grid'

gpsCoords :: Position -> Int
gpsCoords (x, y) = 100 * x + y

boxCoords :: Grid -> [Int]
boxCoords grid =
  let coords = [(x, y) | x <- [0 .. length grid - 1], y <- [0 .. length (fst . fromJust $ uncons grid) - 1], getGridVal (x, y) grid == 'O']
   in map gpsCoords coords

day15_1 :: IO ()
day15_1 = do
  contents <- lines <$> readFile "input/day15.txt"
  let [grid, d] = splitOn [""] contents
      directions = map (fromJust . getDirection) $ concat d
      startPos = getStartPosition '@' grid
      finalGrid = visitGrid startPos directions grid
  putStrLn $ "Day 15, Puzzle 1 solution: " ++ show (sum $ boxCoords finalGrid)
