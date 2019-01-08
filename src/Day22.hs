{-# LANGUAGE RecordWildCards #-}

module Day22
  (
    solution1, Point(..)
  ) where

import Data.Sequence as Seq
import Data.List as List
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Maybe as Maybe
import Data.Tuple as Tuple

import Debug.Trace

type Grid = Map Point RegionType

data Point = Point
  { x :: Int
  , y :: Int
  } deriving (Show, Eq)

instance Ord Point where
  compare Point {x = x1
                ,y = y1} Point {x = x2
                               ,y = y2} = compare (y1, x1) (y2, x2)

data RegionType = Rocky | Narrow | Wet deriving (Eq, Show, Enum)

data Equipment = Torch | ClimbingGear | Neither deriving (Eq, Show)

adjacent :: Point -> [Point]
adjacent Point {..} =
  (Point <$> [x] <*> [y - 1, y + 1]) ++ (Point <$> [x - 1, x + 1] <*> [y])

buildMap :: Seq Point -> Map Point Int -> Int -> Int -> Int -> Map Point Int
buildMap Empty erosionMap _ _ _ = erosionMap
buildMap (queue :|> point@Point{..}) erosionMap maxX maxY depth =
  if Map.member point erosionMap
  then buildMap queue erosionMap maxX maxY depth
  else
    let gIdx = geologicIndex point erosionMap
        erosion = (depth + gIdx) `rem` 20183
        nextMap = Map.insert point erosion erosionMap
        points = [Point (x + 1) y , Point x (y + 1)]
        enqueue = List.filter (\p@Point{..} -> x <= maxX && y <= maxY && Map.notMember p erosionMap) points
        nextQueue = List.foldl' (flip (<|)) queue enqueue
    in buildMap nextQueue nextMap maxX maxY depth

geologicIndex :: Point -> Map Point Int -> Int
geologicIndex Point{..} erosionMap
  | x == 0 && y == 0 = 0
  | x == 0 = 48271 * y
  | y == 0 = 16807 * x
  | otherwise = e1 * e2 where e1 = erosionMap ! Point (x - 1) y
                              e2 = erosionMap ! Point x (y - 1)

erosionMap' :: Int -> Point -> Map Point Int
erosionMap' depth target@Point{..} =
  let m = buildMap (Point 0 0 <| Seq.empty) Map.empty (x + 30) (y + 30) depth
  in Map.insert target (depth `rem` 20183) m

regionMap :: Map Point Int -> Map Point RegionType
regionMap = Map.map (regionType . (`rem` 3))

regionType :: Int -> RegionType
regionType = toEnum

riskLevel :: RegionType -> Int
riskLevel = fromEnum

areaRiskLevel :: Map Point Int -> Point -> Int
areaRiskLevel erosion target@Point{..} =
  let rMap = Map.map riskLevel $ regionMap erosion
  in List.sum $ Map.elems $ Map.filterWithKey (\Point{x=xp,y=yp} _ -> xp <= x && yp <= y) rMap

solution1 :: Point -> Int -> Int
solution1 target@Point{..} depth =
  let erosion = erosionMap' depth target
  in areaRiskLevel erosion target
