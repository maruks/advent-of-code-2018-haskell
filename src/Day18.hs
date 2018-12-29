{-# LANGUAGE RecordWildCards #-}

module Day18
  (
    buildMap, solution1, solution2
  ) where

import Data.Sequence as Seq
import Data.List as List
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Maybe as Maybe
import Data.Tuple as Tuple

data Acre = Open | Trees | Lumberyard deriving (Eq, Show, Ord)
type Scan = Map Point Acre

data Point = Point
  { x :: Int
  , y :: Int
  } deriving (Show, Eq)

instance Ord Point where
  compare Point {x = x1
                ,y = y1} Point {x = x2
                               ,y = y2} = compare (y1, x1) (y2, x2)

adjacent:: Point -> [Point]
adjacent p@Point{..} = [ Point xp yp | xp <- [(x-1)..(x+1)], yp <- [(y-1)..(y+1)], Point xp yp /= p ]

adjacentLumberyardsAndTrees :: Point -> Scan -> (Int, Int)
adjacentLumberyardsAndTrees point scan = List.foldl' (\a@(l,t) p -> case scan Map.!? p of
                                                                      Just Trees -> (l, t + 1 )
                                                                      Just Lumberyard -> (l + 1,t )
                                                                      _ -> a) (0,0) $ adjacent point

mapDimensions :: Scan -> (Int, Int)
mapDimensions scan = (maxX, maxY) where maxX = List.maximum $ List.map x $ Map.keys scan
                                        maxY = List.maximum $ List.map y $ Map.keys scan

fill :: Acre -> (Int, Int) -> Acre
fill prev (lumberyards, trees)
  | prev == Open && trees >= 3 = Trees
  | prev == Trees && lumberyards >= 3 = Lumberyard
  | prev == Lumberyard = if lumberyards >= 1 && trees >= 1 then Lumberyard else Open
  | otherwise = prev

fillAcre :: Scan -> Point -> Acre -> Acre
fillAcre scan point prev = let adj = adjacentLumberyardsAndTrees point scan
                           in fill prev adj

magic :: (Int, Int) -> Scan -> Scan
magic (maxX,maxY) scan = let pts = [ Point xp yp | xp <- [0..maxX], yp <- [0..maxY] ]
                         in List.foldl' (flip (Map.adjustWithKey (fillAcre scan))) scan pts

totalTreesAndLumberyards :: Scan -> (Int, Int)
totalTreesAndLumberyards scan = (trees, lumberyards) where trees = List.length $ List.filter (==Trees) $ Map.elems scan
                                                           lumberyards = List.length $ List.filter (==Lumberyard) $ Map.elems scan

applyMagic :: Scan -> [Scan]
applyMagic scan = iterate (magic md) scan where md = mapDimensions scan

buildMap' :: [String] -> Int -> Int -> Scan -> Scan
buildMap' [] _ _ result = result
buildMap' ([]:xs) x y result = buildMap' xs 0 (y + 1) result
buildMap' ((c:cs):ys) x y result =
  buildMap' (cs : ys) (x + 1) y $ Map.insert point (square c) result
  where
    point = Point x y
    square '.' = Open
    square '#' = Lumberyard
    square '|' = Trees

buildMap :: [String] -> Scan
buildMap xs = buildMap' xs 0 0 Map.empty

finalResourceValue :: Int -> Scan -> Int
finalResourceValue minutes scan =
  let finalScan = applyMagic scan !! minutes
      (trees, lumberyards) = totalTreesAndLumberyards finalScan
  in trees * lumberyards

solution1 :: Scan -> Int
solution1 = finalResourceValue 10

solution2 :: Scan -> Int
solution2 scan = let total = 1000000000
                     dropFirst = 600
                     (x:xs) = List.drop dropFirst $ applyMagic scan
                     cycleLen = snd $ Maybe.fromJust $ List.find ((==x) . fst) $ List.zip xs (iterate (+1) 1)
                     after = (total - dropFirst ) `rem` cycleLen
                 in finalResourceValue after x

printMap' :: [Point] -> Int -> Scan -> String
printMap' [] _ _ = []
printMap' (p@Point{..}:xs) prevY scan =
  let c =
        case scan ! p of
          Open -> '.'
          Trees -> '|'
          Lumberyard -> '#'
  in if y > prevY
       then '\n' : c : printMap' xs y scan
       else c : printMap' xs y scan

printMap :: Scan -> String
printMap scan = printMap' (Map.keys scan) 0 scan
