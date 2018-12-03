module Day3
  (
    parseInput, solution1
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe

data Rectangle = Rectangle Int Int Int Int deriving (Eq, Show)

data Rect = Rect {x1 :: Int,
                  y1 :: Int,
                  x2 :: Int,
                  y2 :: Int} deriving (Eq, Show)

parseInput :: String -> Rectangle
parseInput _ = Rectangle 1 1 3 4

solution1 :: [Rectangle] -> Int
solution1 _ = 0

overlap' :: Rect -> Rect -> (Int,Int)
overlap' (Rect x1p1 y1p1 x2p1 y2p1) (Rect x1p2 y1p2 x2p2 y2p2) = let dx
                                                                        | x1p2 > x1p1 && x1p2 < x2p1 = x2p1 - x1p2
                                                                        | x1p1 > x1p2 && x1p1 < x2p2 = x2p2 - x1p1
                                                                        | otherwise = 0
                                                                     dy
                                                                        | y1p2 > y1p1 && y1p2 < y2p1 = y2p1 - y1p2
                                                                        | y1p1 > y1p2 && y1p1 < y2p2 = y2p2 - y1p1
                                                                        | otherwise = 0
                                                                 in (dx , dy)

overlap :: Rectangle -> Rectangle -> (Int, Int)
overlap (Rectangle x1 y1 w1 h1) (Rectangle x2 y2 w2 h2) = let x1p2 = x1 + w1
                                                              y1p2 = y1 + h1
                                                              x2p2 = x2 + w2
                                                              y2p2 = y2 + h2
                                                          in overlap' (Rect x1 y1 x1p2 y1p2) (Rect x2 y2 x2p2 y2p2)
