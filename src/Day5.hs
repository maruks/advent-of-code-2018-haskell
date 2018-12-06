module Day5
  ( solution1
  , solution2
  ) where

import Data.List as List
import Data.Map.Strict as Map
import Data.Maybe as Maybe
import Data.Char as Char

solution1 :: String -> Int
solution1 xs = length $ alchemicalReduction xs [] True

alchemicalReduction :: String -> String -> Bool -> String
alchemicalReduction [] result _ = result
alchemicalReduction [x] result True = x:result
alchemicalReduction (x:y:xs) result True = if isUpper x && toLower x == y || isUpper y && toLower y == x
                                           then alchemicalReduction xs result False
                                           else alchemicalReduction (y:xs) (x:result) True
alchemicalReduction a@(x:xs) b@(y:ys) False = if isUpper x && toLower x == y || isUpper y && toLower y == x
                                              then alchemicalReduction xs ys False
                                              else alchemicalReduction a b True
alchemicalReduction a b False = alchemicalReduction a b True

solution2 :: String -> Int
solution2 xs = let withCharRemoved c = solution1 $ List.filter (\e -> e /= c && e /= Char.toUpper c) xs
               in List.foldl (\a e -> let a' = withCharRemoved e in if a' < a then a' else a) (maxBound :: Int) ['a' .. 'z']
