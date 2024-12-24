{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day24 (day24_1) where

import Data.Bits
import qualified Data.HashMap.Strict as M
import Data.List (sort)
import Data.List.Split (splitOn)

getWireValue :: String -> M.HashMap String (Maybe Int) -> M.HashMap String String -> Int
getWireValue w wm cm =
  case wm M.! w of
    Just x -> x
    Nothing ->
      let (w1 : op : w2 : _) = words $ cm M.! w
       in if op == "AND"
            then getWireValue w1 wm cm .&. getWireValue w2 wm cm
            else
              if op == "OR"
                then getWireValue w1 wm cm .|. getWireValue w2 wm cm
                else getWireValue w1 wm cm `xor` getWireValue w2 wm cm

toDecimal :: [Int] -> Int
toDecimal n = sum $ zipWith (*) n (iterate (*2) 1)

day24_1 :: IO ()
day24_1 = do
  [inputs, connections] <- splitOn [""] . lines <$> readFile "input/day24.txt"
  let inputsList = [(i, Just ((read :: String -> Int) v)) | [i, v] <- map (splitOn ": ") inputs]
      wireConnections = [(w, c) | [c, w] <- map (splitOn " -> ") connections]
      connectionsMap = M.fromList wireConnections
      wiresMap = M.fromList $ [(fst wc, Nothing) | wc <- wireConnections] ++ inputsList
      outputs = map (\x -> getWireValue x wiresMap connectionsMap) (filter (\(x : _) -> x == 'z') . sort $ M.keys wiresMap)
  print $ toDecimal outputs
