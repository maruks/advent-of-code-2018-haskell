module Day17
  (
    parseInput, solution1, solution2
  ) where

import Data.Sequence as Seq
import Data.List as List
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Maybe as Maybe
import Data.Tuple as Tuple

import Text.Regex.PCRE

type Point = (Int, Int)
type Scan = Map Point Char

waterFlows :: Point -> Scan -> Bool
waterFlows p s = Map.notMember p s || s ! p == '|'

canFlow :: Scan -> Point -> Bool
canFlow s p = let m = Maybe.fromMaybe '-' $ s Map.!? p
              in m == '#' || m == '~'

flow :: Seq Point -> Int -> Scan -> Scan
flow Empty _ scan = scan
flow (queue :|> point@(x,y)) maxY scan =
  let floatY = Maybe.fromJust $ List.find (\e -> Map.notMember (x,e) scan || scan ! (x,e) == '|') [y,(y-1)..]
      down = [ (x, yd) | yd <- [floatY..maxY] ]
      downFlow = List.takeWhile (`waterFlows` scan) down
      stopped@(xp,yp) = List.last downFlow
      verticalFlowMap = Map.fromList $ List.zip downFlow (repeat '|')
      left = [ (xl, yp) | xl <- [xp, (xp-1)..] ]
      leftFlow = List.takeWhile (\p@(xt,yt) -> waterFlows p scan && canFlow scan (xt,yt+1)) left
      right = [ (xr, yp) | xr <- [xp, (xp+1)..] ]
      rightFlow = List.takeWhile (\p@(xt,yt) -> waterFlows p scan && canFlow scan (xt,yt+1)) right
      lastLeft@(xl, yl) = List.last leftFlow
      leftClosed = scan Map.!? (xl - 1, yl) == Just '#'
      lastRight@(xr, yr) = List.last rightFlow
      rightClosed = scan Map.!? (xr + 1, yr) == Just '#'
      bothClosed = leftClosed && rightClosed
      horizontalFlow = leftFlow ++ rightFlow
      horizontalFlowChar = if bothClosed then '~' else '|'
      horizontalFlowMap = Map.fromList $ List.zip horizontalFlow (repeat horizontalFlowChar)
      lrPoints = [(xl - 1, yl), (xr + 1, yr)]
      openPoints = List.filter (`Map.notMember` scan) lrPoints
      nextQueue = if bothClosed
                  then queue |> point
                  else List.foldr (<|) queue openPoints
      horizontalFlowMapWithOpenPoints = List.foldl (\a p -> Map.insert p '|' a) horizontalFlowMap openPoints
      verticalFlowAddedMap = Map.union verticalFlowMap scan
      bothFlowsAddedMap = Map.union horizontalFlowMapWithOpenPoints verticalFlowAddedMap
  in
    if yp == maxY
     then flow queue maxY verticalFlowAddedMap
     else flow nextQueue maxY bothFlowsAddedMap

flowResult :: Scan -> Scan
flowResult scan =
  let ys = snd <$> Map.keys scan
      minY = List.minimum ys
      maxY = List.maximum ys
  in flow ((500, minY) <| Seq.empty) maxY scan

solution1 :: Scan -> Int
solution1 = List.length . List.filter (\t -> t=='~' || t=='|') . Map.elems . flowResult

solution2 :: Scan -> Int
solution2 = List.length . List.filter (=='~') . Map.elems . flowResult

parseInput' :: [String] -> [(Int,Int)]
parseInput' [] = []
parseInput' (x:xs) = let result = x =~ "(x|y)=(\\d+), (x|y)=(\\d+)..(\\d+)" :: AllTextSubmatches [] String
                         [_, a, b , _ ,c , d] = getAllTextSubmatches result :: [String]
                         inverted = a == "y"
                         xp = read b :: Int
                         r1 = read c :: Int
                         r2 = read d :: Int
                         pairs = [ (xp, y) | y <- [r1..r2] ]
                         mpairs = if inverted then Tuple.swap <$> pairs else pairs
                     in mpairs ++ parseInput' xs

parseInput :: [String] -> Scan
parseInput = Map.fromList . (\p -> List.zip p (repeat '#')) . parseInput'
