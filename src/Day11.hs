module Day11
  ( solution1
  , solution2
  , grid
  ) where

import Data.List as List
import Data.Map.Strict as Map
import Data.Set as Set

type Point = (Int, Int)

type Grid = Map Point Int

type SumGrid = Map Point Integer

instance Semigroup Int where
  (<>) = (+)

solution1 :: Int -> Point
solution1 gridNumber =
  let g = grid gridNumber
      points = (,) <$> [1 .. 298] <*> [1 .. 298]
  in List.maximumBy (\p1 p2 -> compare (squareSum g p1) (squareSum g p2)) points

grid :: Int -> Grid
grid gridNumber = Map.fromList $ List.zip points $ power <$> points
  where
    points = (,) <$> [1 .. 300] <*> [1 .. 300]
    power (x, y) =
      (((x + 10) * y + gridNumber) * (x + 10) `div` 100) `rem` 10 - 5

squareSum :: Grid -> Point -> Int
squareSum g p =
  let points = (,) <$> [0 .. 2] <*> [0 .. 2]
  in List.sum $ (\e -> Map.findWithDefault 0 (e <> p) g) <$> points

solution2 :: Int -> (Int, Int, Int)
solution2 gridNumber =
  let g = grid gridNumber
      sum = summedArea g
      points = (,) <$> [1 .. 300] <*> [1 .. 300]
      squares =
        List.concatMap
          (\p@(x, y) ->
              [ (s, p)
              | s <- [1 .. min (300 - x) (300 - y)] ])
          points
      (_, (s, (x, y))) =
        List.foldl
          (\a@(size, result) r@(s, p) ->
              let size' = squareSize sum p s
              in if size' > size
                   then (size', r)
                   else a)
          (0, (0, (0, 0)))
          squares
  in (x + 1, y + 1, s)

squareSize :: SumGrid -> Point -> Int -> Integer
squareSize summed p@(x, y) s =
  (summed ! p) + (summed ! (x + s, y + s)) - (summed ! (x, y + s)) -
  (summed ! (x + s, y))

-- https://en.wikipedia.org/wiki/Summed-area_table
summedArea' :: [Point] -> Grid -> SumGrid -> SumGrid
summedArea' [] _ summed = summed
summedArea' (p:ps) grid summed = summedArea' ps grid (Map.insert p s summed)
  where
    (x, y) = p
    a = Map.findWithDefault 0 (x - 1, y) summed
    b = Map.findWithDefault 0 (x, y - 1) summed
    c = Map.findWithDefault 0 (x - 1, y - 1) summed
    s = fromIntegral (grid ! p) + a + b - c

summedArea :: Grid -> SumGrid
summedArea g = summedArea' points g Map.empty
  where
    pts = (,) <$> [1 .. 300] <*> [1 .. 300]
    points = List.sortBy (\(x1, y1) (x2, y2) -> compare (x1 + y1) (x2 + y2)) pts
