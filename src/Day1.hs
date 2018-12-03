module Day1
  ( solution1, solution2
  ) where

import qualified Data.Set as Set
import qualified Data.List as List

solution1 :: [Int] -> Int
solution1 = List.sum

firstFreq :: Set.Set Int -> Int -> [Int] -> Int
firstFreq set freq (x:xs) = if Set.member freq set
                            then freq
                            else firstFreq (Set.insert freq set) (freq + x) xs

solution2 :: [Int] -> Int
solution2 xs = firstFreq Set.empty 0 (cycle xs)
