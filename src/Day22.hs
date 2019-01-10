{-# LANGUAGE RecordWildCards #-}

module Day22
  ( solution1
  , Point(..)
  , solution2
  ) where

import Data.Sequence as Seq
import Data.List as List
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Maybe as Maybe
import Data.Tuple as Tuple

type Grid = Map Point RegionType

data Point = Point
  { x :: Int
  , y :: Int
  } deriving (Show, Eq, Ord)

data RegionType
  = Rocky
  | Wet
  | Narrow
  deriving (Eq, Show, Enum)

data Equipment
  = Torch
  | Climbing
  | Neither
  deriving (Eq, Show, Ord)

extraSpaceX = 50

extraSpaceY = 50

timeToChangeEquipment = 7

erosionModNumber = 20183

adjacent :: Point -> [Point]
adjacent Point {..} =
  let pts = (Point <$> [x] <*> [y - 1, y + 1]) ++ (Point <$> [x - 1, x + 1] <*> [y])
  in List.filter (\Point {..} -> x >= 0 && y >= 0) pts

buildMap :: Seq Point -> Map Point Int -> Int -> Int -> Int -> Map Point Int
buildMap Empty erosionMap _ _ _ = erosionMap
buildMap (queue :|> point@Point {..}) erosionMap maxX maxY depth =
  if Map.member point erosionMap
    then buildMap queue erosionMap maxX maxY depth
    else let gIdx = geologicIndex point erosionMap
             erosion = (depth + gIdx) `rem` erosionModNumber
             nextMap = Map.insert point erosion erosionMap
             points = [Point (x + 1) y, Point x (y + 1)]
             enqueue =
               List.filter
                 (\p@Point {..} -> x <= maxX && y <= maxY && Map.notMember p erosionMap)
                 points
             nextQueue = List.foldl' (flip (<|)) queue enqueue
         in buildMap nextQueue nextMap maxX maxY depth

geologicIndex :: Point -> Map Point Int -> Int
geologicIndex Point {..} erosionMap
  | x == 0 && y == 0 = 0
  | x == 0 = 48271 * y
  | y == 0 = 16807 * x
  | otherwise = e1 * e2
  where
    e1 = erosionMap ! Point (x - 1) y
    e2 = erosionMap ! Point x (y - 1)

erosionMap' :: Int -> Point -> Map Point Int
erosionMap' depth target@Point {..} =
  let m =
        buildMap
          (Point 0 0 <| Seq.empty)
          Map.empty
          (x + extraSpaceX)
          (y + extraSpaceY)
          depth
  in Map.insert target (depth `rem` erosionModNumber) m

regionMap :: Map Point Int -> Map Point RegionType
regionMap = Map.map (regionType . (`rem` 3))

regionType :: Int -> RegionType
regionType = toEnum

riskLevel :: RegionType -> Int
riskLevel = fromEnum

areaRiskLevel :: Map Point Int -> Point -> Int
areaRiskLevel erosion target@Point {..} =
  let rMap = Map.map riskLevel $ regionMap erosion
  in List.sum $
     Map.elems $
     Map.filterWithKey
       (\Point {x = xp
               ,y = yp} _ -> xp <= x && yp <= y)
       rMap

solution1 :: Point -> Int -> Int
solution1 target@Point {..} depth =
  let erosion = erosionMap' depth target
  in areaRiskLevel erosion target

distance :: Point -> Point -> Int
distance Point {x = x1
               ,y = y1} Point {x = x2
                              ,y = y2} = abs (x2 - x1) + abs (y2 - y1)

sortQueue :: Seq (Point, Equipment, Int) -> Seq (Point, Equipment, Int)
sortQueue = Seq.reverse . Seq.sortOn (\(_, _, time) -> time)

isEquipmentValid :: RegionType -> Equipment -> Bool
isEquipmentValid region equipment = List.elem equipment $ requiredTools region

requiredTools :: RegionType -> [Equipment]
requiredTools Rocky = [Climbing, Torch]
requiredTools Wet = [Climbing, Neither]
requiredTools Narrow = [Torch, Neither]

data Node = Node
  { point :: Point
  , equipment :: Equipment
  } deriving (Show, Eq, Ord)

selectPoint :: (Node, Int) -> Grid -> Point -> (Node, Int)
selectPoint (node@Node {..}, time) grid dest
  | grid ! point == grid ! dest || isEquipmentValid (grid ! dest) equipment =
    (Node dest equipment, time + 1)
  | otherwise =
    let needEquip = requiredTools $ grid ! dest
        currentEquip = requiredTools $ grid ! point
        nextEquip = Maybe.fromJust $ List.find (`List.elem` currentEquip) needEquip
    in (Node dest nextEquip, time + timeToChangeEquipment + 1)

smallest :: Map Node Int -> Node
smallest distMap =
  let acc = head $ Map.assocs distMap
  in fst $
     Map.foldlWithKey'
       (\a@(n, d) k v ->
           if v < d
             then (k, v)
             else a)
       acc
       distMap

search :: Node -> Map Node Int -> Set Node -> Grid -> Point -> Int
search node@Node {..} distMap visited grid target
  | point == target =
    if equipment == Torch
      then distMap ! node
      else let nextDistMap =
                 Map.insert
                   (Node point Torch)
                   (distMap ! node + timeToChangeEquipment)
                   (Map.delete node distMap)
           in search
                (smallest nextDistMap)
                nextDistMap
                (Set.insert node visited)
                grid
                target
  | otherwise =
    let Point {..} = target
        adj =
          List.filter
            (\p@Point {x = xp
                      ,y = yp} -> xp <= x + extraSpaceX && yp <= y + extraSpaceY) $
          adjacent point
        dist = distMap ! node
        enq = List.map (selectPoint (node, dist) grid) adj
        enqueue = List.filter (\(n, _) -> Set.notMember n visited) enq
        nextVisited = Set.insert node visited
        nextDistMap =
          List.foldl'
            (\m (node, dist) -> Map.insertWith min node dist m)
            (Map.delete node distMap)
            enqueue
        nextNode = smallest nextDistMap
    in search nextNode nextDistMap nextVisited grid target

shortestPath :: Grid -> Point -> Int
shortestPath grid target =
  let start = Node (Point 0 0) Torch
      queue = Map.insert start 0 Map.empty
  in search start queue Set.empty grid target

solution2 :: Point -> Int -> Int
solution2 target@Point {..} depth =
  let grid = regionMap $ erosionMap' depth target
  in shortestPath grid target
