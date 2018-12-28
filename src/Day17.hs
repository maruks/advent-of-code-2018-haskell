{-# LANGUAGE RecordWildCards #-}

module Day17
  (
    parseInput, solution1
  ) where

import Data.Sequence as Seq
import Data.List as List
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Tuple as Tuple

import Text.Regex.PCRE

--import Debug.Trace

trace _ i= i

type Point = (Int, Int)
type Scan = Map Point Char

waterFlows :: Point -> Scan -> Bool
waterFlows p s = Map.notMember p s || s ! p == '|'

flow :: Seq Point -> Int -> Scan -> Scan
flow Empty _ scan = scan
flow (queue :|> point@(x,y)) maxY scan =
  let down = [ (x, yd) | yd <- [y..maxY] ]
      downFlow = List.takeWhile (`waterFlows` scan) down
      stopped@(xp,yp) = List.last (trace ("Down " ++ show downFlow) downFlow)
      verticalFlowMap = Map.fromList $ List.zip downFlow (repeat '|')
      left = [ (xl, yp) | xl <- [xp, (xp-1)..] ]
      leftFlow = List.takeWhile (\p@(xt,yt) -> waterFlows p scan && Map.member (xt,yt+1) scan) left
      right = [ (xr, yp) | xr <- [xp, (xp+1)..] ]
      rightFlow = List.takeWhile (\p@(xt,yt) -> waterFlows p scan && Map.member (xt,yt+1) scan) right
      lastLeft@(xl, yl) = List.last (trace ("Left " ++ show leftFlow) leftFlow)
      leftClosed = Map.member (xl - 1, yl) scan
      lastRight@(xr, yr) = List.last (trace ("Right " ++ show rightFlow) rightFlow)
      rightClosed = Map.member (xr + 1, yr) scan
      bothClosed = leftClosed && rightClosed
      horizontalFlow = leftFlow ++ rightFlow
      horizontalFlowChar = if bothClosed then '~' else '|'
      horizontalFlowMap = Map.fromList $ List.zip horizontalFlow (repeat horizontalFlowChar)
      lrPoints = [(xl - 1, yl), (xr + 1, yr)]
      openPoints = List.filter (`Map.notMember` scan) lrPoints
      nextQueue = if (trace ("closed " ++ show bothClosed) bothClosed)
                  then queue |> point
                  else List.foldr (<|) queue openPoints
      verticalFlowAddedMap = Map.union verticalFlowMap scan
      bothFlowsAddedMap = Map.union horizontalFlowMap verticalFlowAddedMap
  in if yp == maxY
     then flow queue maxY verticalFlowAddedMap
     else flow nextQueue maxY (trace ("MAP " ++ show bothFlowsAddedMap) bothFlowsAddedMap)

flowResult :: Scan -> Scan
flowResult scan =
  let ys = List.map snd $ Map.keys scan
      minY = List.minimum ys
      maxY = List.maximum ys
  in flow ((500, minY) <| Seq.empty) maxY scan

solution1 :: Scan -> Int
solution1 = List.length . List.filter (\t -> t=='~' || t=='|') . Map.elems . flowResult

parseInput' :: [String] -> [(Int,Int)]
parseInput' [] = []
parseInput' (x:xs) = let result = x =~ "(x|y)=(\\d+), (x|y)=(\\d+)..(\\d+)" :: AllTextSubmatches [] String
                         [_, a, b , _ ,c , d] = getAllTextSubmatches result :: [String]
                         inverted = a == "y"
                         xp = read b :: Int
                         r1 = read c :: Int
                         r2 = read d :: Int
                         pairs = [ (xp, y) | y <- [r1..r2] ]
                         mpairs = if inverted then List.map Tuple.swap pairs else pairs
                     in mpairs ++ parseInput' xs

parseInput :: [String] -> Scan
parseInput = Map.fromList . (\p -> List.zip p (repeat '#')) . parseInput'

lol = do
  file <- readFile "./test/day17.txt"
  print $ parseInput $ lines file
  print $ solution1 $ parseInput $ lines file
