module Day10
  ( solution1
  , solution2
  , renderMessage
  ) where

import Data.List as List
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Monoid as Monoid

type Point = (Int, Int)
type Velocity = (Int, Int)

instance Semigroup Int where
  (<>) = (+)

solution1 :: [(Velocity, Point)] -> Set Point
solution1 xs = fst $ solution xs (maxBound :: Int) 0

solution2 :: [(Velocity, Point)] -> Int
solution2 xs = snd $ solution xs (maxBound :: Int) 0

solution :: [(Velocity, Point)] -> Int -> Int -> (Set Point, Int)
solution points area time = let nextPoints = List.map (\(v, p) -> (v, v <> p)) points
                                pts = List.map snd nextPoints
                                xs = List.map fst pts
                                ys = List.map snd pts
                                nextArea = (List.maximum xs - List.minimum xs) * (List.maximum ys - List.minimum ys)
                            in if nextArea >= area
                               then (Set.fromList $ List.map snd points, time)
                               else solution nextPoints nextArea (time + 1)

renderMessage :: Set Point -> Int -> Int -> String
renderMessage pts w h = let ps = flip (,) <$> [0..h] <*> [0..w]
                            points = Set.toList pts
                            minX = List.minimum $ List.map fst points
                            minY = List.minimum $ List.map snd points
                            scaled = Set.map (\(x,y) -> (8 + x - minX , 2 + y - minY)) pts
                        in
                          List.map (\p@(x,y) -> if Set.member p scaled then '\x2593' else if x == w then '\n' else '\x2591') ps
