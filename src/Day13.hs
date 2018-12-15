{-# LANGUAGE RecordWildCards #-}

module Day13
  ( solution1
  , solution2
  , readInput
  ) where

import Data.Either as Either
import Data.Tuple as Tuple
import Data.List as List
import Data.Maybe as Maybe
import Data.Map.Strict as Map

type Point = (Int, Int)

type Tracks = Map Point Char

data Turn
  = TLeft
  | TStraight
  | TRight
  deriving (Show, Eq, Enum)

data Direction
  = DLeft
  | DRight
  | DUp
  | DDown
  deriving (Show, Eq, Ord)

data Cart = Cart
  { pos :: Point
  , turn :: Turn
  , direction :: Direction
  } deriving (Show, Eq)

instance Ord Cart where
  compare c1 c2 = compare (swap $ pos c1) (swap $ pos c2) -- sort by Y then X coordinate

instance Semigroup Int where
  (<>) = (+)

movement :: Map Direction (Int, Int)
movement =
  Map.fromList
    [(DUp, (0, -1)), (DDown, (0, 1)), (DLeft, (-1, 0)), (DRight, (1, 0))]

cartDirection :: Direction -> Turn -> Direction
cartDirection DUp TLeft = DLeft
cartDirection DUp TRight = DRight
cartDirection DDown TLeft = DRight
cartDirection DDown TRight = DLeft
cartDirection DLeft TLeft = DDown
cartDirection DLeft TRight = DUp
cartDirection DRight TLeft = DUp
cartDirection DRight TRight = DDown
cartDirection d _ = d

turnCart :: Cart -> (Direction, Turn)
turnCart Cart {..} =
  let nextTurn =
        if turn == TRight
          then TLeft
          else succ turn
  in (cartDirection direction turn, nextTurn)

move :: Tracks -> Cart -> Cart
move tr cart@Cart {..} =
  let (nextDir, nextTurn) =
        case tr !? pos of
          Just '+' -> turnCart cart
          Just '/'
            | direction == DUp -> (DRight, turn)
          Just '/'
            | direction == DDown -> (DLeft, turn)
          Just '/'
            | direction == DRight -> (DUp, turn)
          Just '/'
            | direction == DLeft -> (DDown, turn)
          Just '\\'
            | direction == DUp -> (DLeft, turn)
          Just '\\'
            | direction == DDown -> (DRight, turn)
          Just '\\'
            | direction == DLeft -> (DUp, turn)
          Just '\\'
            | direction == DRight -> (DDown, turn)
          _ -> (direction, turn)
  in cart
     { pos = pos <> (movement ! nextDir)
     , direction = nextDir
     , turn = nextTurn
     }

moveCarts :: [Cart] -> [Cart] -> Tracks -> Either [Cart] (Int, Int)
moveCarts [] result _ = Left result
moveCarts (cart:carts) result tracks =
  let movedCart = move tracks cart
      positions = List.map pos carts
  in case List.find (\p -> p == pos movedCart || p == pos cart) positions of
       Nothing -> moveCarts carts (movedCart : result) tracks
       Just p -> Right p

moveCarts' :: [Cart] -> [Cart] -> Tracks -> [Cart]
moveCarts' [] result _ = result
moveCarts' (cart:carts) result tracks =
  let movedCart = move tracks cart
  in case List.find (\p -> pos p == pos movedCart || pos p == pos cart) carts of
       Nothing -> moveCarts' carts (movedCart : result) tracks
       Just collided -> moveCarts' (List.delete collided carts) result tracks

solution1 :: Tracks -> [Cart] -> Point
solution1 tracks carts =
  let moved = moveCarts (List.sort carts) [] tracks
  in case moved of
       Left m -> solution1 tracks m
       Right p -> p

removeCrashed :: [Cart] -> [Cart]
removeCrashed [] = []
removeCrashed (cart:carts) =
  if List.any (\c -> pos c == pos cart) carts
    then removeCrashed (List.filter (\c -> pos c /= pos cart) carts)
    else cart : removeCrashed carts

solution2 :: Tracks -> [Cart] -> Point
solution2 tracks carts =
  let moved = removeCrashed $ moveCarts' (List.sort carts) [] tracks
  in if length moved == 1
       then pos $ head moved
       else solution2 tracks moved

inputCartDirection =
  Map.fromList [('<', DLeft), ('>', DRight), ('v', DDown), ('^', DUp)]

readInput' :: [String] -> Int -> Int -> [Cart] -> Tracks -> ([Cart], Tracks)
readInput' [[]] _ _ carts tracks = (carts, tracks)
readInput' ([]:xs) x y carts tracks = readInput' xs 0 (y + 1) carts tracks
readInput' ((c:cs):ys) x y carts tracks =
  let nextCarts =
        if c `elem` "^v<>"
          then Cart (x, y) TLeft (inputCartDirection ! c) : carts
          else carts
      nextTracks =
        if c `elem` "/+\\"
          then Map.insert (x, y) c tracks
          else tracks
  in readInput' (cs : ys) (x + 1) y nextCarts nextTracks

readInput :: [String] -> ([Cart], Tracks)
readInput lines = readInput' lines 0 0 [] Map.empty
