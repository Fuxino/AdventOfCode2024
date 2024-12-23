module Day23 (day23_1) where

import Control.Monad
import Data.Containers.ListUtils (nubOrd)
import Data.List (nub, sort)
import Data.List.Split (splitOn)

startsWithT :: String -> Bool
startsWithT [] = False
startsWithT (x : _)
  | x == 't' = True
  | otherwise = False

getConns3 :: [[String]] -> [[String]]
getConns3 connections = do
  conn1 <- connections
  conn2 <- connections

  guard $
    conn1 /= conn2
      && length (nub (conn1 ++ conn2)) == 3
      && any startsWithT (conn1 ++ conn2)

  conn3 <- connections

  guard $
    conn1 /= conn3
      && conn2 /= conn3
      && length (nub (conn1 ++ conn2 ++ conn3)) == 3
      && any startsWithT (conn1 ++ conn2 ++ conn3)

  return . nub $ conn1 ++ conn2 ++ conn3

day23_1 :: IO ()
day23_1 = do
  contents <- lines <$> readFile "input/day23.txt"
  let connections = map (splitOn "-") contents
  putStrLn $
    "Day 23, Puzzle 1 solution: "
      ++ show (length . nubOrd . map sort $ getConns3 connections)
