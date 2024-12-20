{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day17 (day17_1) where

import Control.Monad.State
import Data.Bits
import Data.Char (isDigit)
import Data.List (uncons)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

type Program = [Int]

data Computer = Computer
  { registerA :: Integer,
    registerB :: Integer,
    registerC :: Integer,
    program :: Program,
    pointer :: Int,
    output :: String
  }
  deriving (Show)

adv :: Int -> State Computer ()
adv n = state $ \c ->
  if n `elem` [0 .. 3]
    then ((), c {registerA = registerA c `div` (2 ^ n), pointer = pointer c + 2})
    else
      if n == 4
        then ((), c {registerA = registerA c `div` (2 ^ registerA c), pointer = pointer c + 2})
        else
          if n == 5
            then ((), c {registerA = registerA c `div` (2 ^ registerB c), pointer = pointer c + 2})
            else ((), c {registerA = registerA c `div` (2 ^ registerC c), pointer = pointer c + 2})

bxl :: Int -> State Computer ()
bxl n = state $ \c -> ((), c {registerB = registerB c `xor` fromIntegral n, pointer = pointer c + 2})

bst :: Int -> State Computer ()
bst n = state $ \c ->
  if n `elem` [0 .. 3]
    then ((), c {registerB = fromIntegral n `mod` 8, pointer = pointer c + 2})
    else
      if n == 4
        then ((), c {registerB = registerA c `mod` 8, pointer = pointer c + 2})
        else
          if n == 5
            then ((), c {registerB = registerB c `mod` 8, pointer = pointer c + 2})
            else ((), c {registerB = registerC c `mod` 8, pointer = pointer c + 2})

jnz :: Int -> State Computer ()
jnz n = state $ \c ->
  if registerA c == 0
    then ((), c {pointer = pointer c + 2})
    else ((), c {pointer = n})

bxc :: State Computer ()
bxc = state $ \c -> ((), c {registerB = registerB c `xor` registerC c, pointer = pointer c + 2})

out :: Int -> State Computer ()
out n = state $ \c ->
  if n `elem` [0 .. 3]
    then ((), c {output = output c ++ "," ++ show (n `mod` 8), pointer = pointer c + 2})
    else
      if n == 4
        then ((), c {output = output c ++ "," ++ show (registerA c `mod` 8), pointer = pointer c + 2})
        else
          if n == 5
            then ((), c {output = output c ++ "," ++ show (registerB c `mod` 8), pointer = pointer c + 2})
            else ((), c {output = output c ++ "," ++ show (registerC c `mod` 8), pointer = pointer c + 2})

bdv :: Int -> State Computer ()
bdv n = state $ \c ->
  if n `elem` [0 .. 3]
    then ((), c {registerB = registerA c `div` (2 ^ n), pointer = pointer c + 2})
    else
      if n == 4
        then ((), c {registerB = registerA c `div` (2 ^ registerA c), pointer = pointer c + 2})
        else
          if n == 5
            then ((), c {registerB = registerA c `div` (2 ^ registerB c), pointer = pointer c + 2})
            else ((), c {registerB = registerA c `div` (2 ^ registerC c), pointer = pointer c + 2})

cdv :: Int -> State Computer ()
cdv n = state $ \c ->
  if n `elem` [0 .. 3]
    then ((), c {registerC = registerA c `div` (2 ^ n), pointer = pointer c + 2})
    else
      if n == 4
        then ((), c {registerC = registerA c `div` (2 ^ registerA c), pointer = pointer c + 2})
        else
          if n == 5
            then ((), c {registerC = registerA c `div` (2 ^ registerB c), pointer = pointer c + 2})
            else ((), c {registerC = registerA c `div` (2 ^ registerC c), pointer = pointer c + 2})

getInstruction :: State Computer (Int, Int)
getInstruction = state $ \c -> ((program c !! pointer c, program c !! (pointer c + 1)), c)

runProgram :: State Computer ()
runProgram = do
  c <- get
  if pointer c >= length (program c)
    then return ()
    else do
      (ins, op) <- getInstruction
      case ins of
        0 -> adv op
        1 -> bxl op
        2 -> bst op
        3 -> jnz op
        4 -> bxc
        5 -> out op
        6 -> bdv op
        7 -> cdv op
        _ -> return ()
      runProgram

day17_1 :: IO ()
day17_1 = do
  contents <- lines <$> readFile "input/day17.txt"
  let [r, [p]] = splitOn [""] contents
      registers = map (read . filter isDigit) r
      prog = map (read . filter isDigit) $ splitOn "," p
      computer = Computer {registerA = fst . fromJust $ uncons registers, registerB = registers !! 1, registerC = registers !! 2, pointer = 0, program = prog, output = ""}
  putStr "Day 17, Puzzle 1 solution: "
  print . drop 1 . output $ execState runProgram computer
