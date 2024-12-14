import Data.List (elemIndex, uncons)
import Data.Maybe (isJust, fromMaybe, fromJust)

type Grid = [String]
type Position = (Int, Int)
data Direction = U | R | D | L deriving Eq

getDirection :: Char -> Direction
getDirection '^' = U
getDirection '>' = R
getDirection 'v' = D
getDirection '<' = L

getStartPosition:: Char -> Grid -> Position
getStartPosition c grid = (x, y)
                              where x = fromMaybe (-1) . elemIndex True . map (isJust . elemIndex c) $ grid
                                    y = if x == -1 then -1 else fromMaybe (-1) . elemIndex c $ grid !! x

getGridVal :: Position -> Grid -> Char
getGridVal (x, y) grid = (grid !! x) !! y

isInside :: Position -> Grid -> Bool
isInside (x, y) grid = x >= 0 && y >= 0 && x < length grid && y < length (fst . fromJust $ uncons grid)

getNextPosition :: Position -> Direction -> Grid -> (Position, Direction)
getNextPosition (x, y) U grid = let newPos = (x - 1, y)
                                    gridVal = getGridVal newPos grid
                                in  if newPos `isInside` grid && gridVal == '#'
                                        then getNextPosition (x, y) R grid
                                    else (newPos, U)
getNextPosition (x, y) R grid = let newPos = (x, y + 1)
                                    gridVal = getGridVal newPos grid
                                in  if newPos `isInside` grid && gridVal == '#'
                                        then getNextPosition (x, y) D grid
                                    else (newPos, R)
getNextPosition (x, y) D grid = let newPos = (x + 1, y)
                                    gridVal = getGridVal newPos grid
                                in  if newPos `isInside` grid && gridVal == '#'
                                        then getNextPosition (x, y) L grid
                                    else (newPos, D)
getNextPosition (x, y) L grid = let newPos = (x, y - 1)
                                    gridVal = getGridVal newPos grid
                                in  if newPos `isInside` grid && gridVal == '#'
                                        then getNextPosition (x, y) U grid
                                    else (newPos, L)

markVisited :: Position -> Char -> Grid -> Grid
markVisited (x, y) c grid = let row = grid !! x
                                newRow = take y row ++ [c] ++ drop (y + 1) row
                            in  take x grid ++ [newRow] ++ drop (x + 1) grid

visitGrid :: Position -> Direction -> Grid -> Grid
visitGrid (x, y) direction grid = let newGrid = markVisited (x, y) 'X' grid
                                      (nextPosition, newDirection) = getNextPosition (x, y) direction grid
                                  in  if nextPosition `isInside` newGrid
                                          then visitGrid nextPosition newDirection newGrid
                                      else newGrid

main = do
    contents <- lines <$> readFile "day6.txt"
    let (x, y) = (\w x y z -> fst . fromJust $ uncons $ filter ((>= 0) . fst) [w, x, y, z]) <$> getStartPosition 'v' <*> getStartPosition '^'
                                                                         <*> getStartPosition '<' <*> getStartPosition '>' $ contents
        direction = getDirection $ (contents !! x) !! y
    print . length . filter (== 'X') . concat $ visitGrid (x, y) direction contents
