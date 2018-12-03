module Main where

import qualified Day1
import qualified Day2
import qualified Day3

import Text.Regex.PCRE

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

day3 :: IO ()
day3 = do
  file <- readFile "./test/day3.txt"
  let rs = map parseLine $ lines file where
        parseLine :: String -> Day3.Rectangle
        parseLine s = let stringResult = s =~ "\\d+" :: AllTextMatches [] String
                          [id, x, y, w, h] = map read $ getAllTextMatches stringResult :: [Int]
                      in Day3.Rectangle id x y w h
  print $ Day3.solution1 rs
  print $ Day3.solution2 rs

main :: IO ()
main = do
  day1
  day2
  day3
