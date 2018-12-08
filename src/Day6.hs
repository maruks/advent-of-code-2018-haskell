module Day6
  ( solution1
  , solution2
  ) where

import Data.List as List
import Data.Map.Strict as Map
import Data.Maybe as Maybe
import Data.Char as Char
import Data.Ord as Ord

type Point = (Int, Int)

solution1 :: [Point] -> Int
solution1 ps = let fin = List.filter (isFinite' ps) ps
               in
                 List.foldl (\s p -> let s' = areaSize ps p in if s < s' then s' else s) 0 fin

distance :: Point -> Point -> Int
distance (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

isFinite' :: [Point] -> Point -> Bool
isFinite' ps (x,y) = let xa = List.all (\(xp,yp) -> abs (yp - y) > abs (xp - x)) $ List.filter (\(xp, _) -> xp > x) ps
                         xb = List.all (\(xp,yp) -> abs (yp - y) > abs (xp - x)) $ List.filter (\(xp, _) -> xp < x) ps
                         ya = List.all (\(xp,yp) -> abs (xp - x) > abs (yp - y)) $ List.filter (\(_, yp) -> yp > y) ps
                         yb = List.all (\(xp,yp) -> abs (xp - x) > abs (yp - y)) $ List.filter (\(_, yp) -> yp < y) ps
                     in
                       not (xa || xb || ya || yb)

areaSize :: [Point] -> Point -> Int
areaSize allPoints point = areaSize' 1 1 point (List.delete point allPoints)

areaSize':: Int -> Int -> Point -> [Point] -> Int
areaSize' d s point allPoints = let pts = List.filter (\p -> List.all (\p' -> distance p' p > d) allPoints) $ addPoints point d
                                in
                                  if List.null pts
                                  then s
                                  else areaSize' (1 + d) (s + length pts) point allPoints

addPoints :: Point -> Int -> [Point]
addPoints (x,y) d = let r1 = [0 .. d - 1]
                    in List.map (\e -> (x + e, y + d - e)) r1 ++
                       List.map (\e -> (x + d - e, y - e)) r1 ++
                       List.map (\e -> (x - e, y - d + e)) r1 ++
                       List.map (\e -> (x - d + e, y + e)) r1

solution2 :: [Point] -> Int -> Int
solution2  points size = let pts = (,) <$> [1 .. 1000] <*> [1 .. 1000]
                             area = List.filter (\p -> size > List.sum (List.map (distance p) points)) pts
                         in
                           length area
