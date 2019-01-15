{-# LANGUAGE RecordWildCards #-}

module Day23
  ( solution1
  , Nanobot(..)
  , Point(..)
  , solution2
  ) where

import Data.List as List
import Data.Sequence as Seq

data Point = Point
  { x :: Int
  , y :: Int
  , z :: Int
  } deriving (Show, Eq, Ord)

data Cube = Cube
  { point1 :: Point
  , point2 :: Point
  } deriving (Show, Ord, Eq)

data Nanobot = Nanobot
  { point :: Point
  , radius :: Int
  } deriving (Show, Ord, Eq)

newtype SearchArea = SearchArea (Cube, [Nanobot]) deriving (Eq, Show)

targetPoint = Point 0 0 0

distance :: Point -> Point -> Int
distance Point {x = x1 ,y = y1 ,z = z1} Point {x = x2 ,y = y2 ,z = z2} = abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)

solution1 :: [Nanobot] -> Int
solution1 ns =
  let (mPoint, mRadius) = List.foldl' (\a@(maxP, maxR) Nanobot {..} -> if radius > maxR then (point, radius) else a) (Point 0 0 0, 0) ns
  in List.length $ List.filter (\Nanobot {..} -> distance point mPoint <= mRadius) ns

boundingBox :: [Nanobot] -> Cube
boundingBox ns =
  let [xs, ys, zs] = List.map <$> [x, y, z] <*> [List.map point ns]
      [minx, miny, minz, maxx, maxy, maxz] = [List.minimum, List.maximum] <*> [xs, ys, zs]
      point1 = Point minx miny minz
      size = max (maxz - minz) $ max (maxx - minx) (maxy - miny)
      point2 = Point (minx + size) (miny + size) (minz + size)
  in Cube point1 point2

clamp :: Int -> Int -> Int -> Int
clamp min max val
  | val > max = max
  | val < min = min
  | otherwise = val

clampBox :: Cube -> Point -> Point
clampBox Cube {..} Point {..} =
  let Point {x = x1 ,y = y1 ,z = z1} = point1
      Point {x = x2 ,y = y2 ,z = z2} = point2
  in Point (clamp x1 x2 x) (clamp y1 y2 y) (clamp z1 z2 z)

inRange :: Cube -> Nanobot -> Bool
inRange cube@Cube {..} Nanobot {..} =
  let clamped = clampBox cube point
  in distance clamped point <= radius

splitCube :: Cube -> [Cube]
splitCube Cube {..} =
  let Point {x = x1 ,y = y1 ,z = z1} = point1
      Point {x = x2 ,y = y2 ,z = z2} = point2
      size = (x2 - x1) `div` 2
      midPoint1 = Point (x1 + size) (y1 + size) (z1 + size)
      z' = z1 + size + 1
      x' = x1 + size + 1
      y' = y1 + size + 1
      midPoint2 = Point x' y' z'
      c1 = Cube point1 midPoint1
      c2 = Cube midPoint2 point2
      c3 = Cube (Point x' y1 z1) (Point x2 (y1 + size) (z1 + size))
      c4 = Cube (Point x1 y' z1) (Point (x1 + size) y2 (z1 + size))
      c5 = Cube (Point x' y' z1) (Point x2 y2 (z1 + size))
      c6 = Cube (Point x1 y' z') (Point (x1 + size) y2 z2)
      c7 = Cube (Point x' y1 z') (Point x2 (y1 + size) z2)
      c8 = Cube (Point x1 y1 z') (Point (x1 + size) (y1 + size) z2)
  in if even (x2 - x2)
       then [c1, c2, c3, c4, c5, c6, c7, c8]
       else splitCube (Cube point1 (Point (x2 + 1) (y2 + 1) (z2 + 1)))

distanceToTarget :: Cube -> Int
distanceToTarget cube = let clamped = clampBox cube targetPoint
                        in distance clamped targetPoint

instance Ord SearchArea where
  compare (SearchArea (cube1, bots1)) (SearchArea (cube2, bots2)) =
    let ordLen = compare (List.length bots1) (List.length bots2)
        ordSize = compare (cubeSize cube2) (cubeSize cube1)
        ordDist = compare (distanceToTarget cube2) (distanceToTarget cube1)
    in ordLen <> ordDist <> ordSize

splitArea :: SearchArea -> [SearchArea]
splitArea (SearchArea (cube, ns)) =
  let cubes = splitCube cube
  in List.map SearchArea $ List.zip cubes $ List.map (\c -> List.filter (inRange c) ns) cubes

cubeSize :: Cube -> Int
cubeSize Cube{..} = let Point {x = x1} = point1
                        Point {x = x2} = point2
                      in x2 - x1

findCube :: Seq SearchArea -> Cube
findCube (queue :|> s@(SearchArea (cube, ns)) ) =
  if cubeSize cube == 0
  then cube
  else let nextQueue = List.foldr (<|) queue $ splitArea s
       in findCube $ Seq.sort nextQueue

solution2 :: [Nanobot] -> Int
solution2 ns = let box = boundingBox ns
                   Cube{..} = findCube $ SearchArea (box, ns) <| Seq.empty
               in distance targetPoint point1
