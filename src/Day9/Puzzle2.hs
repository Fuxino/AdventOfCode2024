{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day9.Puzzle2 (day9_2) where

import Data.Char (digitToInt)
import qualified Data.Foldable as F
import Data.Function (on)
import Data.List (groupBy, intersperse)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as S

type DiskElem = (Int, Int)

parseDiskMap :: [Int] -> S.Seq DiskElem
parseDiskMap xs =
  let values = intersperse (-1) [0 ..]
   in S.fromList $ zip values xs

isSpaceEnough :: Int -> DiskElem -> Bool
isSpaceEnough n (-1, l) = l >= n
isSpaceEnough _ _ = False

updateSpace :: Int -> DiskElem -> DiskElem
updateSpace n (-1, l) = (-1, l - n)

combineSpace :: DiskElem -> DiskElem -> DiskElem
combineSpace (-1, l1) (-1, l2) = (-1, l1 + l2)

compareFileValue :: Int -> DiskElem -> Bool
compareFileValue x (v, _) = x == v

moveFile :: Int -> Int -> DiskElem -> DiskElem -> S.Seq DiskElem -> S.Seq DiskElem
moveFile i sIndex sVal fVal xs =
  let xs' = F.toList . S.insertAt sIndex fVal . S.insertAt sIndex sVal . S.deleteAt sIndex . S.insertAt i (-1, snd fVal) $ S.deleteAt i xs
   in S.fromList $ map (foldl1 combineSpace) $ groupBy ((==) `on` fst) xs'

compactFiles :: Int -> S.Seq DiskElem -> S.Seq DiskElem
compactFiles (-1) xs = xs
compactFiles 0 xs = xs
compactFiles n xs =
  if fst fVal == -1 || sIndex == -1 || sIndex >= n
    then compactFiles (n - 1) xs
    else compactFiles fIndex xs'
  where
    fVal = S.index xs n
    sIndex = fromMaybe (-1) $ S.findIndexL (isSpaceEnough (snd fVal)) xs
    sVal = updateSpace (snd fVal) (fromMaybe (-1, 0) $ S.lookup sIndex xs)
    xs' = moveFile n sIndex sVal fVal xs
    fIndex = fromMaybe (-1) $ S.findIndexR (compareFileValue (fst fVal - 1)) xs'

maskMinus1 :: [Int] -> [Int]
maskMinus1 [] = []
maskMinus1 (l : ls)
  | l == -1 = 0 : maskMinus1 ls
  | otherwise = l : maskMinus1 ls

tuplesToIntList :: S.Seq DiskElem -> [Int]
tuplesToIntList disk =
  let listDisk = F.toList disk
   in concatMap (\x -> replicate (snd x) (fst x)) listDisk

checksum :: [Int] -> Int
checksum xs = sum $ zipWith (*) (maskMinus1 xs) [0 ..]

day9_2 :: IO ()
day9_2 = do
  contents <- init <$> readFile "input/day9.txt"
  let disk = parseDiskMap $ map digitToInt contents
      i = fromMaybe (-1) $ S.findIndexR (\x -> fst x /= -1) disk
      compactedDisk = tuplesToIntList $ S.filter (\x -> snd x > 0) $ compactFiles i disk
  putStrLn $ "Day 9, Puzzle 2 solution: " ++ show (checksum compactedDisk)
