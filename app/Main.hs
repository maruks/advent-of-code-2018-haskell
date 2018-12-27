module Main where

import qualified Data.Map.Strict as Map
import Data.List as List

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
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16

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

day11 :: IO ()
day11 = do
  print $ Day11.solution1 6548
  print $ Day11.solution2 6548

parseDay12Input :: String -> [Bool]
parseDay12Input s = let result = s =~ "(\\.|#)" :: AllTextMatches [] String
                        rs = getAllTextMatches result
                    in map (=="#") rs

day12 :: IO ()
day12 = do
  file <- readFile "./test/day12.txt"
  let (x:xs) = lines file
      init = parseDay12Input x
      ts = Map.fromList $ map (\[b1,b2,b3,b4,b5,b6] -> ((b1,b2,b3,b4,b5),b6))  $ filter (not . null) $ map parseDay12Input xs
      gen2 = 50000000000
      r200 = fromIntegral $ Day12.solution1 ts init 200
      r300 = fromIntegral $ Day12.solution1 ts init 300
      d100 = r300 - r200
      solution2 :: Integer
      solution2 = r300 + ((50000000000 - 300) `div` 100) * d100

  print $ Day12.solution1 ts init 20
  print $ solution2

day13 :: IO ()
day13 = do
  file <- readFile "./test/day13.txt"
  let (carts,tracks) = Day13.readInput $ lines file
  print $ Day13.solution1 tracks carts
  print $ Day13.solution2 tracks carts

day14 :: IO ()
day14 = do
  print $ Day14.solution1 503761
  print $ Day14.solution2 [5,0,3,7,6,1]

day15 :: IO ()
day15 = do
  file <- readFile "./test/day15.txt"
  let map = Day15.buildMap $ lines file
  print $ Day15.solution1 map
  print $ Day15.solution2 map

parseDay16Input :: [String] -> [Day16.Sample]
parseDay16Input [] = []
parseDay16Input xs = let ([_,before, opcode, after], rs) = List.splitAt 4 xs
                         bf_result = before =~ "Before: (\\[(\\d,?\\s?)+\\])" :: AllTextSubmatches [] String
                         [a0,b0,c0,d0] = read $ getAllTextSubmatches bf_result !! 1 :: [Int]
                         op_result = opcode =~ "\\d+" :: AllTextMatches [] String
                         [a1,b1,c1,d1] = map read $ getAllTextMatches op_result :: [Int]
                         af_result = after =~ "After:  (\\[(\\d,?\\s?)+\\])" :: AllTextSubmatches [] String
                         [a2,b2,c2,d2] = read $ getAllTextSubmatches af_result !! 1 :: [Int]
                     in
                       ((a0,b0,c0,d0),(a1,b1,c1,d1),(a2,b2,c2,d2)) : parseDay16Input rs

parseDay16Input_part2 :: String -> Day16.BinOpcode
parseDay16Input_part2 s =
  let result = s =~ "\\d+" :: AllTextMatches [] String
      [a,b,c,d] = map read $ getAllTextMatches result :: [Int]
  in (a,b,c,d)

day16 :: IO ()
day16 = do
  file1 <- readFile "./test/day16-part-1.txt"
  file2 <- readFile "./test/day16-part-2.txt"
  let samples = parseDay16Input $ lines file1
      opcodes = map parseDay16Input_part2 $ lines file2
      opcodeNumbers = Day16.findOpcodeNumbers samples
  print $ Day16.solution1 samples
  print $ opcodeNumbers
  print $ Day16.solution2 opcodes opcodeNumbers

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
  day11
  day12
  day13
  day14
  day15
  day16
