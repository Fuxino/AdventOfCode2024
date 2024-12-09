import Data.List (intersperse)
import Data.Char (digitToInt)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Applicative

parseDiskMap :: [Int] -> S.Seq Int
parseDiskMap xs = let values = intersperse (-1) [0..]
                  in  S.fromList . concat . getZipList $ replicate <$> ZipList xs <*> ZipList values

compact :: S.Seq Int -> S.Seq Int
compact xs
    | fileIndex == -1 = xs
    | otherwise       = S.filter (/= -1) $ startDisk S.>< (compact . S.insertAt 0 fileVal . S.deleteAt 0 $ S.deleteAt fileIndex endDisk)
        where spaceIndex           = fromJust $ S.elemIndexL (-1) xs
              (startDisk, endDisk) = S.splitAt spaceIndex xs
              fileIndex            = fromMaybe (-1) (S.findIndexR (/= -1) endDisk)
              fileVal              = S.index endDisk fileIndex

checksum :: [Int] -> Int
checksum xs = sum $ zipWith (*) xs [0..]

main = do
    contents <- init <$> readFile "day9.txt"
    let diskMap = map digitToInt contents
    print . checksum . F.toList . compact $ parseDiskMap diskMap
