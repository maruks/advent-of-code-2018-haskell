{-# LANGUAGE RecordWildCards #-}

module Day20
  (
    buildMap, printMap, solution1, solution2
  ) where

import Data.Sequence as Seq
import Data.List as List
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Maybe as Maybe
import Data.Tuple as Tuple

data Location = Room | Door | Wall deriving (Eq, Show, Ord)
type Grid = Map Point Location

data Point = Point
  { x :: Int
  , y :: Int
  } deriving (Show, Eq)

instance Ord Point where
  compare Point {x = x1 ,y = y1} Point {x = x2 ,y = y2} = compare (y1, x1) (y2, x2)  -- used in printMap

splitOptions' :: String -> String -> Int -> ([String], String)
splitOptions' (')':xs) acc 0 = ([List.reverse acc], xs)
splitOptions' ('|':xs) acc 0 = let (opts, rest) = splitOptions' xs "" 0
                               in (List.reverse acc : opts, rest)
splitOptions' ('(':xs) acc level = splitOptions' xs ('(':acc) (level + 1)
splitOptions' (')':xs) acc level = splitOptions' xs (')':acc) (level - 1)
splitOptions' (x:xs) acc level = splitOptions' xs (x:acc) level

splitOptions :: String -> ([String], String)
splitOptions xs = splitOptions' xs "" 0

-- this code assumes that each detour returns to original location
buildMap' :: String -> Point -> [(Point, Location)]
buildMap' [] _ = []
buildMap' ('(':xs) point@Point{..} = let (opts, rest) = splitOptions xs
                                     in List.concatMap (`buildMap'` point) opts ++ buildMap' rest point
buildMap' ('N':xs) Point{..} = (Point x (y-1),Door) : (Point x (y-2),Room) : buildMap' xs (Point x (y-2))
buildMap' ('S':xs) Point{..} = (Point x (y+1),Door) : (Point x (y+2),Room) : buildMap' xs (Point x (y+2))
buildMap' ('E':xs) Point{..} = (Point (x+1) y,Door) : (Point (x+2) y,Room) : buildMap' xs (Point (x+2) y)
buildMap' ('W':xs) Point{..} = (Point (x-1) y,Door) : (Point (x-2) y,Room) : buildMap' xs (Point (x-2) y)
buildMap' (_:xs) point = buildMap' xs point

buildMap :: String -> Grid
buildMap s = let m = Map.fromList $ (Point 0 0, Room) : buildMap' s (Point 0 0)
                 ks = Map.keys m
                 xs = x <$> ks
                 ys = y <$> ks
                 emptykeys = [ Point x y | x <- [minimum xs - 1..maximum xs + 1] , y <- [minimum ys - 1..maximum ys + 1], Map.notMember (Point x y) m]
                 wallsMap = Map.fromList $ List.zip emptykeys $ repeat Wall
             in Map.union m wallsMap

adjacent :: Point -> [Point]
adjacent p@Point{..} = [ Point xp yp | xp <- [(x-1)..(x+1)], yp <- [(y-1)..(y+1)], Point xp yp /= p ]

nextRoom :: Point -> Point -> Point
nextRoom Point{..} Point{x = xp, y = yp}
  | xp > x = Point (xp + 1) y
  | xp < x = Point (xp - 1) y
  | yp > y = Point x (yp + 1)
  | yp < y = Point x (yp - 1)

adjacentRooms :: Point -> Grid -> [Point]
adjacentRooms point@Point{..} grid = let adj = adjacent point
                                         doors = List.filter (\p -> case grid Map.!? p of
                                                                     Just Door -> True
                                                                     _ -> False) adj
                                     in nextRoom point <$> doors

bfs  :: Seq (Point, Int) -> Set Point -> Grid -> Map Point Int -> Map Point Int
bfs Empty _ _ distMap = distMap
bfs (queue :|> (point, distance)) visited grid distMap =
    let adj = adjacentRooms point grid
        enqueue = List.filter (\p -> not $ Set.member p visited) adj
        nextVisited = List.foldl' (flip Set.insert) visited enqueue
        nextDist = distance + 1
        nextQueue =
          List.foldl' (flip (<|)) queue $
          List.zip enqueue (repeat nextDist)
        nextDistMap =
          if Map.notMember point distMap
            then Map.insert point distance distMap
            else distMap
    in bfs nextQueue nextVisited grid nextDistMap

distanceMap :: Grid -> Map Point Int
distanceMap grid = bfs ((Point 0 0, 0) <| Seq.empty) Set.empty grid Map.empty

solution1 :: Grid -> Int
solution1 = maximum . Map.elems . distanceMap

solution2 :: Grid -> Int
solution2 = List.length . List.filter (>=1000) . Map.elems. distanceMap

printMap' :: [Point] -> Int -> Grid -> String
printMap' [] _ _ = []
printMap' (p@Point{..}:xs) prevY grid =
  let c =
        case grid ! p of
          Room -> '.'
          Door -> if odd y then '-' else '|'
          Wall -> '#'
  in if y > prevY
       then '\n' : c : printMap' xs y grid
       else c : printMap' xs y grid

printMap :: Grid -> String
printMap grid = printMap' (Map.keys grid) 0 grid
