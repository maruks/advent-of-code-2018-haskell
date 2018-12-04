module Day4
  ( solution1
  , Log(..)
  ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

type Timestamp = Int
type GuardId = Int
type Hour = Int
type Minute = Int

data Log
  = BeginShift Timestamp GuardId
  | FallsAsleep Timestamp Minute
  | WakesUp Timestamp Minute
  deriving (Eq, Show)

-- [1518-10-01 00:00] Guard #3491 begins shift
-- [1518-08-31 00:47] wakes up
-- [1518-03-30 00:25] falls asleep

solution1 :: [Log] -> Int
solution1 xs = let stats = guardStats xs 0 Nothing Map.empty
                   (guard, minutes) = sleepyGuard stats
                   m = findMinute minutes (0,0)
               in guard * m

sleepyGuard :: Map.Map GuardId [Minute] -> (GuardId, [Minute])
sleepyGuard = Map.foldlWithKey (\a@(i,s) k v -> if length v > length s then (k,v) else a) (-1,[])

findMinute :: [Minute] -> (Int, Minute) -> Minute
findMinute [] (_, m) = m
findMinute all@(x:xs) acc@(s,m) = let elems = List.takeWhile (==x) all
                                      size = length elems
                                      newAcc = if size > s then (size, x) else acc
                                      newXs = dropWhile (==x) all
                                  in findMinute newXs newAcc

guardStats :: [Log] -> GuardId -> Maybe Minute -> Map.Map GuardId [Minute] -> Map.Map GuardId [Minute]
guardStats [] _ _ acc = acc
guardStats (x:xs) g ms acc =
  case x of
    BeginShift _ newId -> guardStats xs newId Nothing acc
    FallsAsleep _ m -> guardStats xs g (Just m) acc
    WakesUp _ m -> guardStats xs g Nothing newAcc
      where newAcc = Map.insertWith (++) g [Maybe.fromJust ms .. m - 1] acc
