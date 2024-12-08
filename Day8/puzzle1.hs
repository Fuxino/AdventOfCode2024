import Control.Applicative
import qualified Data.Set as Set

type Freq = Char
type Coords = (Int, Int)
data Antenna = Antenna { frequency :: Freq
                       , coordinates :: Coords
                       } deriving (Show, Eq)

readAntenna :: Freq -> Coords -> Antenna
readAntenna frequency coordinates = Antenna {frequency=frequency, coordinates=coordinates}

getAntennas :: [String] -> [Antenna]
getAntennas grid = concat . getZipList $ getAntennasRow <$> ZipList [0..] <*> ZipList grid
                     where getAntennasRow n row = [ readAntenna x (n, y) | (x, y) <- zip row [0..], x /= '.' ]

isInside :: Coords -> Int -> Int -> Bool
isInside c x y = fst c >= 0 && fst c < x && snd c >= 0 && snd c < y
                
getAntinodes :: Antenna -> Antenna -> Int -> Int -> [Coords]
getAntinodes a b maxX maxY= let xa = fst $ coordinates a
                                ya = snd $ coordinates a
                                xb = fst $ coordinates b
                                yb = snd $ coordinates b
                                distX = abs $ xa - xb
                                distY = abs $ ya - yb
                            in  if frequency a /= frequency b || coordinates a == coordinates b
                                    then []
                                else if xa > xb && ya > yb
                                    then filter (\c -> isInside c maxX maxY) [(xb - distX, yb - distY), (xa + distX, ya + distY)]
                                else if xa > xb && ya < yb
                                    then filter (\c -> isInside c maxX maxY) [(xa + distX, ya - distY), (xb - distX, yb + distY)]
                                else if xa == xb && ya > yb
                                    then filter (\c -> isInside c maxX maxY) [(xa, ya + distY), (xb, yb - distY)]
                                else if ya == yb && xa > xb
                                    then filter (\c -> isInside c maxX maxY) [(xa + distX, ya), (xb - distX, yb)]
                                else getAntinodes b a maxX maxY

main = do
    contents <- lines <$> readFile "day8.txt"
    let antennas = getAntennas contents
        x = length contents
        y = length $ head contents
        antinodes = Set.fromList $ concat [ getAntinodes a b x y | a <- antennas, b <- antennas, a /= b, frequency a == frequency b ] 
    print $ length antinodes
