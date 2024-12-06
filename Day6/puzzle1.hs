import Data.List (elemIndex)
import Data.Maybe (isJust, fromMaybe)

type Grid = [String]
type Position = (Int, Int)
data Direction = U | R | D | L deriving Eq

getDirection :: Char -> Direction
getDirection '^' = U
getDirection '>' = R
getDirection 'v' = D
getDirection '<' = L

getStartPositionition :: Char -> Grid -> Position
getStartPositionition c grid = (x, y)
                            where x = fromMaybe (-1) . elemIndex True . map (isJust . elemIndex c) $ grid
                                  y = if x == -1
                                           then -1
                                           else fromMaybe (-1) . elemIndex c $ grid !! x

getGridVal :: Position -> Grid -> Char
getGridVal (x, y) grid = (grid !! x) !! y

moveUp :: Position -> Position
moveUp (x, y) = (x - 1, y)

moveDown :: Position -> Position
moveDown (x, y) = (x + 1, y)

moveLeft :: Position -> Position
moveLeft (x, y) = (x, y - 1)

moveRight :: Position -> Position
moveRight (x, y) = (x, y + 1)

isInside :: Position -> Grid -> Bool
isInside (x, y) grid = x >= 0 && y >= 0 && x < length grid && y < length (head grid)

getNextPosition :: Position -> Direction -> Grid -> (Position, Direction)
getNextPosition (x, y) U grid = let newPos = moveUp (x, y)
                                in  if newPos `isInside` grid && getGridVal newPos grid == '#'
                                       then (moveRight (x, y), R)
                                    else (newPos, U)
getNextPosition (x, y) R grid = let newPos = moveRight (x, y)
                                in  if newPos `isInside` grid && getGridVal newPos grid == '#'
                                       then (moveDown (x, y), D)
                                    else (newPos, R)
getNextPosition (x, y) D grid = let newPos = moveDown (x, y)
                                in  if newPos `isInside` grid && getGridVal newPos grid == '#'
                                       then (moveLeft (x, y), L)
                                    else (newPos, D)
getNextPosition (x, y) L grid = let newPos = moveLeft (x, y)
                                in  if newPos `isInside` grid && getGridVal newPos grid == '#'
                                       then (moveUp (x, y), U)
                                    else (newPos, L)

markVisited :: Position -> Grid -> Grid
markVisited (x, y) grid = let row = grid !! x
                              newRow = take y row ++ ['X'] ++ drop (y + 1) row
                          in  take x grid ++ [newRow] ++ drop (x + 1) grid

visitGrid :: Position -> Direction -> Grid -> Grid
visitGrid (x, y) direction grid = let newGrid = markVisited (x, y) grid
                                      (newPosition, newDirection) = getNextPosition (x, y) direction grid
                                  in  if newPosition `isInside` newGrid
                                         then visitGrid newPosition newDirection newGrid
                                         else newGrid

main = do
    contents <- lines <$> readFile "day6.txt"
    let (x, y) = (\w x y z -> head $ filter ((>= 0) . fst) [w, x, y, z]) <$> getStartPositionition 'v' <*> getStartPositionition '^'
                                                                             <*> getStartPositionition '<' <*> getStartPositionition '>' $ contents
        direction = getDirection $ (contents !! x) !! y
    print . length . filter (== 'X') . concat $ visitGrid (x, y) direction contents
