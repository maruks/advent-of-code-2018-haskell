module Day3
  ( solution1
  , solution2
  , Rectangle(..)
  ) where

import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe

type Point = (Int, Int)

data Rectangle = Rectangle
  { i :: Int
  , x :: Int
  , y :: Int
  , w :: Int
  , h :: Int
  } deriving (Eq, Show)

data Rect = Rect
  { x1 :: Int
  , y1 :: Int
  , x2 :: Int
  , y2 :: Int
  } deriving (Eq, Show)

solution1 :: [Rectangle] -> Int
solution1 = allOverlaps Set.empty

pointsRange :: Int -> Int -> Int -> Int -> [Int]
pointsRange s1 e1 s2 e2 = [max s1 s2 .. min e1 e2]

overlap' :: Rect -> Rect -> [Point]
overlap' (Rect x1p1 y1p1 x2p1 y2p1) (Rect x1p2 y1p2 x2p2 y2p2) = do
  x <- pointsRange x1p1 x2p1 x1p2 x2p2
  y <- pointsRange y1p1 y2p1 y1p2 y2p2
  return (x, y)

overlap :: Rectangle -> Rectangle -> [Point]
overlap (Rectangle _ x1 y1 w1 h1) (Rectangle _ x2 y2 w2 h2) =
  let x1p = x1 + w1 - 1
      y1p = y1 + h1 - 1
      x2p = x2 + w2 - 1
      y2p = y2 + h2 - 1
  in overlap' (Rect x1 y1 x1p y1p) (Rect x2 y2 x2p y2p)

allOverlaps :: Set.Set Point -> [Rectangle] -> Int
allOverlaps points [] = Set.size points
allOverlaps points (x:xs) =
  let newPoints = List.concatMap (overlap x) xs
      newSet = Set.union points (Set.fromList newPoints)
  in allOverlaps newSet xs

solution2 :: [Rectangle] -> Int
solution2 rs =
  Maybe.fromMaybe (-1) $
  i <$> List.find (\e -> List.all (null . overlap e) $ List.delete e rs) rs
