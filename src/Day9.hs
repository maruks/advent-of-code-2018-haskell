module Day9
  ( solution1
  ) where

--  , solution2
import Data.List as List
import Data.Map.Strict as Map
import Data.Maybe as Maybe
import Data.Char as Char
import Data.Set as Set

--import Debug.Trace

solution1 :: Int -> Int -> Int
solution1 players marbles = play [1 .. marbles] (0, [0]) players Map.empty

placeMarble :: (Int, [Int]) -> Int -> ((Int, [Int]), Int)
placeMarble (current, xs) marble
  | marble `rem` 23 == 0 =
    let remIdx = (current - 7) `mod` length xs
        removed = xs !! remIdx
    in ((remIdx, List.delete removed xs), marble + removed)
  | otherwise =
    let splitIdx = (current + 1) `rem` length xs
        (xs1, xs2) = List.splitAt (splitIdx + 1) xs
    in ((splitIdx + 1, xs1 ++ marble : xs2), 0)

play :: [Int] -> (Int, [Int]) -> Int -> Map Int Int -> Int
play [] _ _ scores = List.maximum $ Map.elems scores
play (x:xs) m players scores =
  let (marbles, score) = placeMarble m x
      nextScores =
        if score > 0
          then Map.insertWith (+) (x `rem` players) score scores
          else scores
  in play xs marbles players nextScores
