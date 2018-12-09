module Day8
  ( solution1
  , solution2
  ) where

import Data.List as List
import Data.Map.Strict as Map
import Data.Maybe as Maybe
import Data.Char as Char
import Data.Set as Set

solution1 :: [Int] -> Int
solution1 = List.sum . fst . metadata

metadata :: [Int] -> ([Int], Int)
metadata (children:meta:xs)
  | children == 0 = (List.take meta xs, 2 + meta)
  | otherwise = loop xs children [] 0 where
      loop ys 0 metas sum = (List.take meta (List.drop sum xs) ++ metas, sum + meta + 2)
      loop ys c metas sum = let (ms, len) = metadata ys
                            in loop (List.drop len ys) (c - 1) (metas ++ ms) (sum + len)

solution2 :: [Int] -> Int
solution2 = fst . value

value :: [Int] -> (Int, Int)
value (children:meta:xs)
  | children == 0 = (List.sum $ List.take meta xs, 2 + meta)
  | otherwise = loop xs children Map.empty 1 0 where
      loop ys 0 valMap idx sum = (List.foldl' (\a e -> a + Maybe.fromMaybe 0 (valMap !? e)) 0 $ List.take meta (List.drop sum xs), sum + meta + 2)
      loop ys c valMap idx sum = let (val, len) = value ys
                                 in loop (List.drop len ys) (c - 1) (Map.insert idx val valMap) (idx + 1) (sum + len)
