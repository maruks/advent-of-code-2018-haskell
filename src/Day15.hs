{-# LANGUAGE RecordWildCards, PatternSynonyms #-}

module Day15
  ( pointsToTargets
  , buildMap
  , Point(..)
  , Unit(..)
  , Square(..)
  , shortestPath
  , performRound
  , rounds
  , printMap
  , solution1
  , solution2
  ) where

import Data.List as List
import Data.Set as Set
import Data.Maybe as Maybe
import Data.Map.Strict as Map
import Data.Sequence as Seq

import Debug.Trace

data Point = Point
  { x :: Int
  , y :: Int
  } deriving (Show, Eq)

instance Ord Point where
  compare Point {x = x1
                ,y = y1} Point {x = x2
                               ,y = y2} = compare (y1, x1) (y2, x2)

data Unit
  = Elf Int
        String
  | Goblin Int
           String
  deriving (Show, Eq)

data Square
  = Open
  | Wall
  | Warrior Unit
  deriving (Show, Eq)

type Terrain = Map Point Square

adjacent :: Point -> [Point]
adjacent Point {..} =
  (Point <$> [x] <*> [y - 1, y + 1]) ++ (Point <$> [x - 1, x + 1] <*> [y])

adjacentOpenSquares :: Terrain -> Point -> [Point]
adjacentOpenSquares terrain point = List.filter (isOpen terrain) $ adjacent point

adjacentEnemyUnits :: Terrain -> Point -> Unit -> [Point]
adjacentEnemyUnits terrain point unit =
  let elf = isElf unit
  in List.filter
       (\p ->
           case terrain ! p of
             Warrior u -> isElf u /= elf
             _ -> False) $
     adjacent point

buildMap' :: [String] -> Int -> Int -> Terrain -> Terrain
buildMap' [] _ _ result = result
buildMap' ([]:xs) x y result = buildMap' xs 0 (y + 1) result
buildMap' ((c:cs):ys) x y result =
  buildMap' (cs : ys) (x + 1) y $ Map.insert point (square c) result
  where
    point = Point x y
    square '.' = Open
    square '#' = Wall
    square 'E' = Warrior $ Elf 200 (show x ++ "-" ++ show y)
    square 'G' = Warrior $ Goblin 200 (show x ++ "-" ++ show y)

buildMap :: [String] -> Terrain
buildMap xs = buildMap' xs 0 0 Map.empty

isElf :: Unit -> Bool
isElf (Elf _ _) = True
isElf _ = False

hitPoints :: Unit -> Int
hitPoints (Elf h _) = h
hitPoints (Goblin h _) = h

unitId :: Unit -> String
unitId (Elf _ i) = i
unitId (Goblin _ i) = i

isOpen :: Terrain -> Point -> Bool
isOpen terrain point =
  case terrain ! point of
    Open -> True
    _ -> False

targets :: Unit -> Terrain -> [Point]
targets unit terrain =
  let elf = isElf unit
  in Map.keys $
     Map.filter
       (\e ->
           case e of
             Warrior u -> isElf u /= elf
             _ -> False)
       terrain

enemyUnits :: Unit -> Terrain -> Bool
enemyUnits unit terrain =
  let elf = isElf unit
  in List.any
       (\e ->
           case e of
             Warrior u -> isElf u /= elf
             _ -> False) $
     Map.elems terrain

compareUnits :: (Point, Unit) -> (Point, Unit) -> Ordering
compareUnits (p1, u1) (p2, u2) =
  let ch = compare (hitPoints u1) (hitPoints u2)
  in if ch == EQ
       then compare p1 p2
       else ch

selectTarget :: [Point] -> Terrain -> (Point, Unit)
selectTarget points terrain =
  let units =
          (\p ->
              case terrain ! p of
                Warrior u -> u
                _ -> error "not a unit") <$> points
      unitsPoints = List.zip points units
  in List.minimumBy compareUnits unitsPoints

pointsToTargets :: Unit -> Point -> Terrain -> [Point]
pointsToTargets unit point terrain =
  let ts = targets unit terrain
  in List.concatMap (adjacentOpenSquares terrain) ts

findPath :: Point -> Point -> Map Point Point -> Maybe Point
findPath from current pointsMap =
  pointsMap Map.!? current >>= (\p -> if p == from then Just current else findPath from p pointsMap)

bfs
  :: Seq (Point, Int, Point)
  -> Set Point
  -> Set Point
  -> Map Point Point
  -> Maybe Int
  -> Terrain
  -> Map Point Point
bfs Empty _ _ _ Nothing _ = Map.empty
bfs Empty targets _ pointsMap (Just stopDistance) terrain = pointsMap
bfs (queue :|> (point, distance, parent)) targets visited pointsMap stopAfter terrain
  | distance > Maybe.fromMaybe (maxBound :: Int) stopAfter = pointsMap
  | otherwise =
    let adj = adjacentOpenSquares terrain point
        enqueue = List.sort $ List.filter (\p -> not $ Set.member p visited) adj
        nextVisited = List.foldl (flip Set.insert) visited enqueue
        nextDist = distance + 1
        nextStop =
          if Maybe.isNothing stopAfter && Set.member point targets
            then Just distance
            else stopAfter
        nextQueue = List.foldl (flip (<|)) queue $ List.zip3 enqueue (repeat nextDist) (repeat point)
        nextPointsMap = if Map.notMember point pointsMap then Map.insert point parent pointsMap else pointsMap
    in bfs nextQueue targets nextVisited nextPointsMap nextStop terrain

shortestPath :: Point -> [Point] -> Terrain -> Maybe Point
shortestPath point targets terrain =
  let queue = (point, 0, point) <| Seq.empty
      pointsMap = bfs queue (Set.fromList targets) (Set.fromList [point]) Map.empty Nothing terrain
      target = List.foldl (\a t -> if Map.notMember t pointsMap
                                   then a
                                   else case a of
                                          Just p -> if t < p then Just t else a
                                          Nothing -> Just t) Nothing targets
  in case target of
    Just t -> findPath point t pointsMap
    Nothing -> Nothing

allUnits :: Terrain -> [(Point, Unit, String)]
allUnits terrain =
  let results :: [(Point, Unit, String)]
      results =
        Map.foldlWithKey
          (\acc point unit ->
              case unit of
                Warrior u@(Elf _ i) -> (point, u, i) : acc
                Warrior u@(Goblin _ i) -> (point, u, i) : acc
                _ -> acc)
          []
          terrain
  in List.sortOn (\(a, _, _) -> a) results

moveUnit :: Point -> Point -> Terrain -> Terrain
moveUnit from to terrain =
  let t = Map.insert from Open terrain
  in case terrain ! from of
       w@(Warrior unit) -> Map.insert to w t
       _ -> error "not a unit"

attackUnit :: (Point, Unit) -> Terrain -> Int -> Terrain
attackUnit (point, Elf hp i) terrain _ =
  if hp - 3 < 1
    then Map.insert point Open terrain
    else Map.insert point (Warrior (Elf (hp - 3) i)) terrain
attackUnit (point, Goblin hp i) terrain attackPower =
  if hp - attackPower < 1
    then Map.insert point Open terrain
    else Map.insert point (Warrior (Goblin (hp - attackPower) i)) terrain

totalHitPoints :: Terrain -> Int
totalHitPoints terrain =
  List.sum $
    (\e ->
        case e of
          Warrior (Elf h _) -> h
          Warrior (Goblin h _) -> h
          _ -> 0) <$> Map.elems terrain

numberOfElves :: Terrain -> Int
numberOfElves terrain =
  List.sum $
    (\e ->
        case e of
          Warrior (Elf _ _) -> 1
          _ -> 0) <$> Map.elems terrain

getUnitId :: Terrain -> Point -> String
getUnitId terrain point =
  case terrain ! point of
    Warrior (Elf _ i) -> i
    Warrior (Goblin _ i) -> i
    _ -> ""

applyTurns :: [(Point, Unit, String)]
           -> Bool
           -> Terrain
           -> Int
           -> (Terrain, Bool)
applyTurns [] _ terrain _ = (terrain, False)
applyTurns ((point, unit, unitId):xs) moved terrain attackPower
  | getUnitId terrain point /= unitId = applyTurns xs False terrain attackPower
  | not $ enemyUnits unit terrain = (terrain, True)
  | otherwise =
    case adjacentEnemyUnits terrain point unit of
      []
        | moved -> applyTurns xs False terrain attackPower
      [] ->
        let targets = pointsToTargets unit point terrain
            path = shortestPath point targets terrain
        in case path of
             Just to ->
               let unitMoved = moveUnit point to terrain
               in applyTurns ((to, unit, unitId) : xs) True unitMoved attackPower
             Nothing -> applyTurns xs False terrain attackPower
      rs ->
        let target = selectTarget rs terrain
        in applyTurns xs False (attackUnit target terrain attackPower) attackPower

performRound :: Terrain -> Int -> (Terrain, Bool)
performRound terrain attackPower =
  let turns = allUnits terrain
  in applyTurns turns False terrain attackPower

rounds :: Terrain -> Int -> [(Terrain, Bool)]
rounds terrain attackPower = let result = performRound terrain attackPower
                             in result : rounds (fst result) attackPower

finishRounds :: Terrain -> Int -> Int
finishRounds terrain attackPower =
  let (finishedRounds, (lastRound, _) : _ ) = List.break snd $ rounds terrain attackPower
  in totalHitPoints lastRound * List.length finishedRounds

tryAttackPower :: Int -> Int -> Terrain -> Maybe Int
tryAttackPower attackPower elves terrain =
  let allElvesAlive terrain = numberOfElves terrain == elves
      (finishedRounds, (lastRound, _) : _ ) = List.break (\(t, f) -> f || not (allElvesAlive t)) $ rounds terrain attackPower
  in if allElvesAlive lastRound
       then Just $ totalHitPoints lastRound * List.length finishedRounds
       else Nothing

solution1 :: Terrain -> Int
solution1 terrain = finishRounds terrain 3

solution2' :: Int -> Terrain -> Int -> Int
solution2' elves terrain attackPower =
  let res = tryAttackPower attackPower elves terrain
  in fromMaybe (solution2' elves terrain (attackPower + 1)) res

solution2 :: Terrain -> Int
solution2 terrain = solution2' (numberOfElves terrain) terrain 4

units :: Terrain -> Terrain
units =
  Map.filter
    (\v ->
        case v of
          Warrior _ -> True
          _ -> False)

printMap' :: [Point] -> Int -> Terrain -> String
printMap' [] _ _ = []
printMap' (p@Point {..}:xs) prevY terrain =
  let c =
        case terrain ! p of
          Warrior (Elf _ _) -> 'E'
          Warrior (Goblin _ _) -> 'G'
          Open -> '.'
          Wall -> '#'
  in if y > prevY
       then '\n' : c : printMap' xs y terrain
       else c : printMap' xs y terrain

printMap :: Terrain -> String
printMap terrain = printMap' (Map.keys terrain) 0 terrain

pm :: Terrain -> IO ()
pm = putStrLn . printMap
