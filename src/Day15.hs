{-# LANGUAGE RecordWildCards, PatternSynonyms #-}

module Day15
  ( pointsToTargets, buildMap, Point(..), Unit(..), Square(..), shortestPath, solution1, solution2
  ) where

import Data.List as List
import Data.Set as Set
import Data.Maybe as Maybe
import Data.Map.Strict as Map
import Data.Sequence as Seq

import Debug.Trace

data Point = Point {x :: Int,
                    y :: Int} deriving (Show, Eq)

instance Ord Point where
  compare Point{x = x1, y = y1} Point{x = x2, y = y2} = compare (y1, x1) (y2, x2)

data Unit = Elf Int | Goblin Int deriving (Show, Eq)

data Square = Open | Wall | Warrior Unit deriving (Show, Eq)

type Terrain = Map Point Square

adjacent :: Point -> [Point]
adjacent Point{..} = (Point <$> [x] <*> [y - 1 ,y + 1]) ++ (Point <$> [x - 1, x + 1] <*> [y])

adjacentOpenSquares :: Terrain -> Point -> [Point]
adjacentOpenSquares terrain point = List.filter (isOpen terrain) $ adjacent point

adjacentEnemyUnits :: Terrain -> Point -> Unit -> [Point]
adjacentEnemyUnits terrain point unit = let elf = isElf unit
                                            in List.filter (\p -> case terrain ! p of
                                                                    Warrior u -> isElf u /= elf
                                                                    _ -> False) $ adjacent point

buildMap' :: [String] -> Int -> Int -> Terrain -> Terrain
buildMap' [] _ _ result = result
buildMap' ([]:xs) x y result = buildMap' xs 0 (y + 1) result
buildMap' ((c:cs):ys) x y result =
  buildMap' (cs:ys) (x + 1) y $ Map.insert point (square c) result
  where
    point = Point x y
    square '.' = Open
    square '#' = Wall
    square 'E'= Warrior $ Elf 200
    square 'G'= Warrior $ Goblin 200

buildMap :: [String] -> Terrain
buildMap xs = buildMap' xs 0 0 Map.empty

isElf :: Unit -> Bool
isElf (Elf _) = True
isElf _ = False

hitPoints :: Unit -> Int
hitPoints (Elf h) = h
hitPoints (Goblin h) = h

isOpen :: Terrain -> Point -> Bool
isOpen terrain point = case terrain ! point of
                         Open -> True
                         _ -> False

targets :: Unit -> Terrain -> [Point]
targets unit terrain = let elf = isElf unit
                       in Map.keys $ Map.filter (\e -> case e of
                                                         Warrior u -> isElf u /= elf
                                                         _ -> False) terrain

enemyUnits :: Unit -> Terrain -> Bool
enemyUnits unit terrain = let elf = isElf unit
                          in List.any (\e -> case e of
                                               Warrior u -> isElf u /= elf
                                               _ -> False) $ Map.elems terrain

compareUnits :: (Point, Unit) -> (Point, Unit) -> Ordering
compareUnits (p1,u1) (p2,u2) = let ch = compare (hitPoints u1) (hitPoints u2)
                                   in if ch == EQ
                                      then compare p1 p2
                                      else ch

selectTarget :: [Point] -> Terrain -> (Point, Unit)
selectTarget points terrain = let units = List.map (\p -> case terrain ! p of
                                                            Warrior u -> u
                                                            _ -> error "not a unit") points
                                  unitsPoints = List.zip points units
                                  in List.minimumBy compareUnits unitsPoints

distance :: Point -> Point -> Int
distance Point{x = x1, y = y1} Point{x = x2, y = y2} = abs (x1 - x2) + abs (y1 - y2)

pointsToTargets :: Unit -> Point -> Terrain -> [Point]
pointsToTargets unit point terrain = let ts = targets unit terrain
                                     in List.concatMap (adjacentOpenSquares terrain) ts

findPath :: Point -> Int -> Map Int (Set Point) -> Terrain -> [Point]
findPath point 1 _ _ = [point]
findPath point distance distanceMap terrain = let adj = List.sort $ adjacentOpenSquares terrain point
                                                  pts = distanceMap ! (distance - 1)
                                                  nextPoint = Maybe.fromJust $ List.find (`Set.member` pts) adj
                                              in point : findPath nextPoint (distance - 1) distanceMap terrain

bfs :: Seq (Point,Int) -> Set Point -> Set Point -> Map Int (Set Point) -> Maybe Int -> Terrain -> [Point]
bfs Empty _ _ _ Nothing _ = []
bfs Empty targets _ distanceMap (Just stopDistance) terrain = let targetPoint = List.minimum $ Set.toList $ Set.intersection targets $ distanceMap ! stopDistance
                                                             in List.reverse $ findPath targetPoint  stopDistance  distanceMap  terrain
bfs (queue :|> (point,distance)) targets visited distanceMap stopAfter terrain
  | distance > Maybe.fromMaybe (maxBound :: Int) stopAfter = let stopDistance = Maybe.fromJust stopAfter
                                                                 targetPoint = List.minimum $ Set.toList $ Set.intersection targets $ distanceMap ! stopDistance
                                                             in List.reverse $ findPath targetPoint  stopDistance distanceMap terrain
  | otherwise = let adj = adjacentOpenSquares terrain point
                    enqueue = List.sort $ List.filter (\p -> not $ Set.member p visited) adj
                    nextVisited = List.foldl (flip Set.insert) visited enqueue
                    nextDist = distance + 1
                    nextStop = if Maybe.isNothing stopAfter && Set.member point targets then Just distance else stopAfter
                    nextQueue = List.foldl (flip (<|)) queue $ List.zip enqueue (repeat nextDist)
                    enqSet = Set.fromList enqueue
                    nextDistMap = Map.insertWith Set.union nextDist  enqSet distanceMap
                in bfs nextQueue targets nextVisited nextDistMap  nextStop  terrain


shortestPath :: Point -> [Point] -> Terrain -> [Point]
shortestPath point targets terrain = let queue = (point,0) <| Seq.empty
                                     in bfs queue (Set.fromList targets) (Set.fromList [point]) (Map.fromList [(0, Set.fromList [point])]) Nothing terrain

allUnits :: Terrain -> [(Point, Unit)]
allUnits terrain = let results = Map.foldlWithKey (\acc point unit -> case unit of
                                                                        Warrior u -> (point,u) : acc
                                                                        _ -> acc) [] terrain
                   in List.sortOn fst results

moveUnit :: Point -> Point -> Terrain -> Terrain
moveUnit from to terrain = let t = Map.insert from Open terrain
                           in case terrain ! from of
                                w@(Warrior unit) -> Map.insert to w t
                                _ -> error "not a unit"

attackUnit :: (Point, Unit) -> Terrain -> Int -> Terrain
attackUnit (point, Elf hp) terrain _ = if hp - 3 < 1
                                     then Map.insert point Open terrain
                                     else Map.insert point (Warrior (Elf (hp - 3))) terrain
attackUnit (point, Goblin hp) terrain attackPower = if hp - attackPower < 1
                                        then Map.insert point Open terrain
                                        else Map.insert point (Warrior (Goblin (hp - attackPower))) terrain

totalHitPoints :: Terrain -> Int
totalHitPoints terrain = List.sum $ List.map (\e -> case e of
                                                      Warrior (Elf h) -> h
                                                      Warrior (Goblin h) -> h
                                                      _ -> 0) $ Map.elems terrain

numberOfElves :: Terrain -> Int
numberOfElves terrain = List.sum $ List.map (\e -> case e of
                                                      Warrior (Elf _) -> 1
                                                      _ -> 0) $ Map.elems terrain

applyTurns :: [(Point, Unit)] -> Bool -> Terrain -> Int -> (Terrain, Bool)
applyTurns [] _ terrain _ = (terrain, False)
applyTurns ((point, unit) : xs) moved terrain attackPower
  | not $ enemyUnits unit terrain = (terrain, True)
  | terrain ! point == Open = applyTurns xs False terrain attackPower
  | otherwise = case adjacentEnemyUnits terrain point unit of
                  [] | moved -> applyTurns xs False terrain attackPower
                  [] -> let targets = pointsToTargets unit point terrain
                            path = shortestPath point targets terrain
                        in case path of
                             (to:_) -> let unitMoved = moveUnit point to terrain
                                       in applyTurns ((to,unit) : xs) True unitMoved attackPower
                             [] -> applyTurns xs False terrain attackPower
                  rs -> let target = selectTarget rs terrain
                        in applyTurns xs False (attackUnit target terrain attackPower) attackPower

finishRounds :: Terrain -> Int -> Int -> Int
finishRounds terrain rounds attackPower = let turns = allUnits terrain
                                              (nextTerrain, stop) = applyTurns turns False terrain attackPower
                              in if stop
                                 then totalHitPoints nextTerrain * rounds
                                 else finishRounds nextTerrain (rounds + 1) attackPower

tryAttackPower :: Int -> Int -> Int -> Terrain -> Maybe Int
tryAttackPower attackPower elves rounds terrain = let turns = allUnits terrain
                                                      (nextTerrain, stop) = applyTurns turns False terrain attackPower
                                                      allElvesAlive = numberOfElves nextTerrain == elves
                                                  in
                                                    if not allElvesAlive
                                                    then Nothing
                                                    else if stop
                                                         then Just (totalHitPoints nextTerrain * rounds)
                                                         else tryAttackPower attackPower elves (rounds + 1) nextTerrain

solution1 :: Terrain -> Int
solution1 terrain = finishRounds terrain 0 3

solution2' :: Int -> Terrain -> Int -> Int
solution2' elves terrain attackPower = let res = tryAttackPower attackPower elves 0 terrain
                                       in fromMaybe (solution2' elves terrain (attackPower + 1)) res

solution2 :: Terrain -> Int
solution2 terrain = solution2' (numberOfElves terrain) terrain 4


map1 = buildMap ["################################",
 "##############.#################",
 "##########G##....###############",
 "#########.....G.################",
 "#########...........############",
 "#########...........############",
 "##########.....G...#############",
 "###########.........############",
 "########.#.#..#..G....##########",
 "#######..........G......########",
 "##..GG..................###.####",
 "##G..........................###",
 "####G.G.....G.#####...E.#.G..###",
 "#....##......#######........####",
 "#.GG.#####.G#########.......####",
 "###..####...#########..E...#####",
 "#...####....#########........###",
 "#.G.###.....#########....E....##",
 "#..####...G.#########E.....E..##",
 "#..###G......#######E.........##",
 "#..##.........#####..........###",
 "#......................#..E....#",
 "##...G........G.......#...E...##",
 "##............#..........#..####",
 "###.....#...#.##..#......#######",
 "#####.###...#######...#..#######",
 "#########...E######....#########",
 "###########...######.###########",
 "############..#####..###########",
 "#############.E..##.############",
 "################.#..############",
 "################################"]
