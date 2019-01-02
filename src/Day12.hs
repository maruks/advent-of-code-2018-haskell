module Day12
  ( solution1
  , growPlants
  ) where

import Data.List as List
import Data.Maybe as Maybe
import Data.Map.Strict as Map

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable as M

type PotState = (Bool, Bool, Bool, Bool, Bool)

type Transition = Map PotState Bool

solution1 :: Transition -> [Bool] -> Int -> Int
solution1 t xs iter = let plants = growPlants xs iter t
                          pots = potNumbers iter plants
                      in List.sum $ snd <$> List.filter fst pots

replace :: (PrimMonad m) => Bool -> Bool -> Int -> Int -> Transition -> M.MVector (PrimState m) Bool -> m ()
replace p1 p2 idx len t vec =
  when (idx < (len - 2)) $
  do cu <- M.read vec idx
     n1 <- M.read vec (idx + 1)
     n2 <- M.read vec (idx + 2)
     M.write vec idx $ Maybe.fromMaybe False (t Map.!? (p1, p2, cu, n1, n2))
     replace p2 cu (idx + 1) len t vec

growPlantsM :: (PrimMonad m) => Int -> Int -> Transition -> M.MVector (PrimState m) Bool -> m ()
growPlantsM 0 _ _ v = return ()
growPlantsM n len t v = do
  b1 <- M.read v 0
  b2 <- M.read v 1
  replace b1 b2 2 len t v
  growPlantsM (n - 1) len t v

growPlants :: [Bool] -> Int -> Transition -> [Bool]
growPlants init iterations transition =
  let v = V.fromList $ List.replicate iterations False List.++ init List.++ List.replicate iterations False
      modified = V.modify (growPlantsM iterations (V.length v) transition) v
  in
    V.toList modified

potNumbers :: Int -> [Bool] -> [(Bool,Int)]
potNumbers iter xs = let numbers = [(-iter)..(-1)] List.++ List.take (List.length xs - iter) [0..]
                     in List.zip xs numbers
