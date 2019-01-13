{-# LANGUAGE RecordWildCards #-}

module Day23 (
  solution1, Nanobot(..), Point(..)
  ) where

import Data.List as List
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Maybe as Maybe

data Point = Point
  { x :: Int
  , y :: Int
  , z :: Int
  } deriving (Show, Eq, Ord)

data Nanobot = Nanobot
  { point :: Point
  , radius :: Int} deriving (Show, Ord, Eq)

distance :: Point -> Point -> Int
distance Point {x = x1 ,y = y1 ,z = z1} Point {x = x2 ,y = y2 ,z = z2} = abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)

solution1 :: [Nanobot] -> Int
solution1 ns = let (mPoint, mRadius) = List.foldl' (\a@(maxP, maxR) Nanobot{..}-> if radius > maxR then (point, radius) else a) (Point 0 0 0, 0) ns
               in List.length $ List.filter (\Nanobot{..} -> distance point mPoint <= mRadius) ns
