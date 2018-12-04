module Day4
  ( solution1
  , solution2
  , Log(..)
  , sortLog
  ) where

import Data.List as List
import Data.Map.Strict as Map
import Data.Maybe as Maybe

type Timestamp = Int
type GuardId = Int
type Hour = Int
type Minute = Int

data Log
  = BeginShift Timestamp GuardId
  | FallAsleep Timestamp Minute
  | WakeUp Timestamp Minute
  deriving (Eq, Show)

solution1 :: [Log] -> Int
solution1 xs =
  let stats = guardStats xs 0 Nothing Map.empty
      (guard, minutes) = sleepyGuard stats
      minute = findMinute $ sort minutes
  in guard * minute

sleepyGuard :: Map GuardId [Minute] -> (GuardId, [Minute])
sleepyGuard =
  foldlWithKey
    (\acc@(_, minutes) guard minutes' ->
        if length minutes' > length minutes
          then (guard, minutes')
          else acc)
    (0, [])

findMinute' :: [Minute] -> (Int, Minute) -> (Int, Minute)
findMinute' [] acc = acc
findMinute' all@(x:xs) acc@(count, _) =
  let elems = takeWhile (== x) xs
      count' = 1 + length elems
      newAcc =
        if count' > count
          then (count', x)
          else acc
      newXs = dropWhile (== x) xs
  in findMinute' newXs newAcc

findMinute :: [Minute] -> Minute
findMinute xs = snd $ findMinute' xs (0, 0)

guardStats :: [Log]
           -> GuardId
           -> Maybe Minute
           -> Map GuardId [Minute]
           -> Map GuardId [Minute]
guardStats [] _ _ acc = acc
guardStats (x:xs) guard maybeMinuteFellAsleep acc =
  case x of
    BeginShift _ newGuard -> guardStats xs newGuard Nothing acc
    FallAsleep _ minute -> guardStats xs guard (Just minute) acc
    WakeUp _ minute -> guardStats xs guard Nothing newAcc
      where newAcc = Map.insertWith (++) guard [Maybe.fromJust maybeMinuteFellAsleep .. minute - 1] acc

sortLog :: [Log] -> [Log]
sortLog = sortOn timestamp
  where
    timestamp :: Log -> Timestamp
    timestamp (BeginShift t _) = t
    timestamp (FallAsleep t _) = t
    timestamp (WakeUp t _) = t

solution2 :: [Log] -> Int
solution2 xs =
  let stats = guardStats xs 0 Nothing Map.empty
      (guard, _, minute) = mostFrequentlyAsleep stats
  in guard * minute

mostFrequentlyAsleep :: Map GuardId [Minute] -> (GuardId, Int, Minute)
mostFrequentlyAsleep = foldlWithKey foldFn (0, 0, 0)
  where
    foldFn acc@(guard, count, minute) guard' minutes =
      let (count', minute') = findMinute' (sort minutes) (0, 0)
      in if count' > count
           then (guard', count', minute')
           else acc
