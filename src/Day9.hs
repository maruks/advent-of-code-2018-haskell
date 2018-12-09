module Day9
  ( solution1
  ) where

--  , solution2
import Data.List as List
import Data.Map.Strict as Map
import Data.Maybe as Maybe
import Data.Char as Char
import Data.Set as Set
import Data.Sequence as Seq

solution1 :: Int -> Int -> Int
solution1 players marbles = play [1 .. marbles] (0, Seq.fromList [0]) players Map.empty

placeMarble :: (Int, Seq Int) -> Int -> ((Int, Seq Int), Int)
placeMarble (current, xs) marble
  | marble `rem` 23 == 0 =
    let remIdx = (current - 7) `mod` Seq.length xs
        removed = Maybe.fromJust $ xs Seq.!? remIdx
    in ((remIdx, Seq.deleteAt remIdx xs), marble + removed)
  | otherwise =
    let insertIdx = 1 + (current + 1) `rem` Seq.length xs
    in ((insertIdx, Seq.insertAt insertIdx marble xs), 0)

play :: [Int] -> (Int, Seq Int) -> Int -> Map Int Int -> Int
play [] _ _ scores = List.maximum $ Map.elems scores
play (x:xs) m players scores =
  let (marbles, score) = placeMarble m x
      nextScores =
        if score > 0
          then Map.insertWith (+) (x `rem` players) score scores
          else scores
  in play xs marbles players nextScores
