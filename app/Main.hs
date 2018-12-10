module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10

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

day5 :: IO ()
day5 = do
  file <- readFile "./test/day5.txt"
  print $ Day5.solution1 file
  print $ Day5.solution2 file

day6 :: IO ()
day6 = do
  file <- readFile "./test/day6.txt"
  let rs = map parseLine $ lines file where
      parseLine :: String -> (Int, Int)
      parseLine s = let result = s =~ "\\d+" :: AllTextMatches [] String
                        [x, y] = map read $ getAllTextMatches result :: [Int]
                    in (x,y)
  print $ Day6.solution1 rs
  print $ Day6.solution2 rs 10000

day7 :: IO ()
day7 = do
  file <- readFile "./test/day7.txt"
  let rs = map parseLine $ lines file where
      parseLine :: String -> (Char, Char)
      parseLine s = let result = s =~ "Step ([A-Z]) must be finished before step ([A-Z])" :: AllTextSubmatches [] String
                        [_, a, b] = getAllTextSubmatches result :: [String]
                    in (head a, head b)
  print $ Day7.solution1 rs
  print $ Day7.solution2 rs 5 60

day8 :: IO ()
day8 = do
  file <- readFile "./test/day8.txt"
  let rs = map read $ words file :: [Int]
  print $ Day8.solution1 rs
  print $ Day8.solution2 rs

day9 :: IO ()
day9 = do
  s <- readFile "./test/day9.txt"
  let result = s =~ "\\d+" :: AllTextMatches [] String
      [x, y] = map read $ getAllTextMatches result :: [Int]
  print $ Day9.solution1 x y
  print $ Day9.solution2 x y

day10 :: IO ()
day10 = do
  file <- readFile "./test/day10.txt"
  let rs = map parseLine $ lines file
      pts = Day10.solution1 rs
      img = Day10.renderMessage pts 80 13 where
      parseLine :: String -> ((Int, Int), (Int, Int))
      parseLine s = let result = s =~ "-?\\d+" :: AllTextMatches [] String
                        [x, y, vx, vy] = map read $ getAllTextMatches result :: [Int]
                    in ((vx,vy), (x,y))
  putStr img
  print $ Day10.solution2 rs


main :: IO ()
main = do
  day1
  day2
  day3
  day4
  day5
  day6
  day7
  day8
  day9
  day10
