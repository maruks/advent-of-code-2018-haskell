module Day25
  ( solution1
  )
where

import qualified Data.UnionFind.ST             as UF
import qualified Data.Set                      as Set
import           Control.Monad.ST
import           Control.Monad

type Point = (Int, Int, Int, Int)

distance :: Point -> Point -> Int
distance (x1, y1, z1, t1) (x2, y2, z2, t2) =
  abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2) + abs (t1 - t2)

isConnected :: Point -> Point -> Bool
isConnected p1 p2 = distance p1 p2 <= 3

constellations :: [Point] -> ST s [Point]
constellations points = do
  pts <- mapM UF.fresh points
  let pairs = zip points pts
  forM_
    pairs
    (\(p1, f1) -> forM_
      pairs
      (\(p2, f2) -> do
        eq <- UF.equivalent f1 f2
        let join = p1 /= p2 && not eq && isConnected p1 p2
        when join $ UF.union f1 f2
      )
    )
  mapM UF.descriptor pts

uniqSize :: [Point] -> Int
uniqSize = Set.size . Set.fromList

solution1 :: [Point] -> Int
solution1 pts = uniqSize $ runST $ constellations pts
