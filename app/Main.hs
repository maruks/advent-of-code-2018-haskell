module Main where

import qualified Day1
import qualified Day2

day1 :: IO ()
day1 = do
  file <- readFile "./test/day1.txt"
  let xs = lines file
      numbers = map (\s -> read $ removePlus s :: Int) xs where removePlus = filter (/='+')
  print $ Day1.solution1 numbers
  print $ Day1.solution2 numbers

day2 :: IO ()
day2 = do
  file <- readFile "./test/day2.txt"
  let xs = lines file
  print $ Day2.solution1 xs
  print $ Day2.solution2 xs

main :: IO ()
main = do
  day1
  day2
