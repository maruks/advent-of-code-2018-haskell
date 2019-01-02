{-# LANGUAGE RecordWildCards #-}

module Day7
  ( solution1
  , solution2
  ) where

import Data.List as List
import Data.Map.Strict as Map
import Data.Maybe as Maybe
import Data.Char as Char
import Data.Set as Set

type Step = (Char, Char)

data Tasks = Tasks
  { steps :: String
  , before :: Map Char (Set Char)
  , after :: Map Char (Set Char)
  } deriving (Show)

data Worker = Worker
  { step :: Char
  , time :: Int
  } deriving (Eq, Show)

updateValue :: Map Char (Set Char) -> Char -> Char -> Map Char (Set Char)
updateValue map key val = Map.insertWith Set.union key (Set.singleton val) map

buildTasks :: [Step] -> Tasks
buildTasks xs =
  let (a, ma, mb) =
        List.foldl
          (\(all, mapB, mapA) (before, step) ->
              ( before : step : all
              , updateValue mapB step before
              , updateValue mapA before step))
          ([], Map.empty, Map.empty)
          xs
  in Tasks (head <$> (group $ sort a)) ma mb

availableTasks :: Tasks -> String
availableTasks Tasks {..} =
  List.filter
    (\e ->
        case before !? e of
          Nothing -> True
          Just ds -> Set.null ds)
    steps

stepFinished :: Char -> Tasks -> Tasks
stepFinished s t@Tasks {..} =
  let afs = Set.toList $ after ! s
      nextBefore =
        List.foldl (flip (Map.adjustWithKey (\k a -> Set.delete s a))) before afs
      nextSteps = List.delete s steps
  in t { before = nextBefore , steps = nextSteps}

solution1' :: String -> Tasks -> String
solution1' result t@Tasks {..} =
  if List.null steps
    then reverse result
    else let s = head $ availableTasks t
         in solution1' (s : result) $ stepFinished s t

solution1 :: [Step] -> String
solution1 ps = solution1' [] $ buildTasks ps

solution2 :: [Step] -> Int -> Int -> Int
solution2 xs maxWorkers duration =
  solution2' 0 0 maxWorkers duration [] $ buildTasks xs

solution2' :: Int -> Int -> Int -> Int -> [Worker] -> Tasks -> Int
solution2' result seconds maxWorkers duration workers t@Tasks {..} =
  let updatedWs = (\w@Worker {..} -> w { time = time - seconds}) <$> workers
      finishedSteps = step <$> List.filter ((== 0) . time) updatedWs
      nextTasks@Tasks {steps = nextSteps} = List.foldl (flip stepFinished) t finishedSteps
      activeWorkers = List.filter ((/= 0) . time) updatedWs
      availWorkers = maxWorkers - length activeWorkers
      availTasks = availableTasks nextTasks List.\\ (step <$> activeWorkers)
      tasksToStart = List.take availWorkers availTasks
      nextWorkers =
        activeWorkers ++
        ((\s -> Worker s (duration + 1 + Char.ord s - Char.ord 'A')) <$> tasksToStart)
      nextSeconds = List.minimum $ time <$> nextWorkers
  in if List.null nextWorkers
       then result + seconds
       else solution2' (result + seconds) nextSeconds maxWorkers duration nextWorkers nextTasks
