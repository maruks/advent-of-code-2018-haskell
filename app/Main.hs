module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4

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
        parseLine s = let result = s =~ "\\d+" :: AllTextMatches [] String
                          [i, x, y, w, h] = map read $ getAllTextMatches result :: [Int]
                      in Day3.Rectangle i x y w h
  print $ Day3.solution1 rs
  print $ Day3.solution2 rs

parseDay4Input :: String -> Day4.Log
parseDay4Input s =
  let result1 = s =~ "\\d+" :: AllTextMatches [] String
      (_:month:day:hour:minute:more) = map read $ getAllTextMatches result1 :: [Int]
      timestamp = 1440 * 31 * month + 1440 * day + 60 * hour + minute
      result2 = s =~ "begins|wakes|falls" :: AllTextMatches [] String
      logType = head $ getAllTextMatches result2
  in
    case logType of
      "begins" -> Day4.BeginShift timestamp (head more)
      "falls" -> Day4.FallAsleep timestamp minute
      "wakes" -> Day4.WakeUp timestamp minute
      _ -> error "invalid log entry"

day4 :: IO ()
day4 = do
  file <- readFile "./test/day4.txt"
  let rs = Day4.sortLog $ map parseDay4Input $ lines file
  print $ Day4.solution1 rs
  print $ Day4.solution2 rs

main :: IO ()
main = do
  day1
  day2
  day3
  day4
