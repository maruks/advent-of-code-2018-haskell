module Day14
  ( solution1
  , solution2
  ) where

import Data.List as List
import Control.Monad.ST as ST
import Control.Monad (when, unless)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable as M

addRecipes :: (PrimMonad m) => [Int] -> Int -> M.MVector (PrimState m) Int -> m ()
addRecipes [] _ _ = return ()
addRecipes (r:rs) index vec =
  do M.write vec index r
     addRecipes rs (index+1) vec

digits :: Int -> [Int]
digits 0 = []
digits n = n `rem` 10 : digits (n `div` 10)

makeRecipesM :: (PrimMonad m) => Int -> Int -> Int -> Int -> M.MVector (PrimState m) Int -> m ()
makeRecipesM n index1 index2 len vec
  | len >= n = return ()
  | otherwise = do
      r1 <- M.read vec index1
      r2 <- M.read vec index2
      let ds = if r1 + r2 == 0 then [0] else List.reverse $ digits (r1 + r2)
          newLen = len + List.length ds
          newIdx i idx = (i + 1 + idx) `rem` newLen
      addRecipes ds len vec
      makeRecipesM n (newIdx r1 index1) (newIdx r2 index2) newLen vec

makeRecipes' :: (PrimMonad m) => Int -> m (Vector Int)
makeRecipes' i = do
    vec <- M.new (i + 12)
    M.write vec 0 3
    M.write vec 1 7
    makeRecipesM i 0 1 2 vec
    V.unsafeFreeze vec

solution1 :: Int -> [Int]
solution1 numberOfRecipes = let v = ST.runST (makeRecipes' $ numberOfRecipes + 10)
                            in V.toList $ V.take 10 $ V.drop numberOfRecipes v

solution2 :: [Int] -> Int
solution2 num = let ds = List.reverse num
                    len = List.length ds
                    vec = ST.runST $ findRecipes' ds
                    result = List.reverse $ List.takeWhile (/=(-1)) $ V.toList vec
                    resultLen = List.length result
                    offset = if List.take len result == ds then 0 else (-1)
                in resultLen - len + offset

findRecipesM :: (PrimMonad m) => [Int] -> [Int] -> Int -> Int -> Int -> M.MVector (PrimState m) Int -> m ()
findRecipesM b n index1 index2 len vec =
  do
    r1 <- M.read vec index1
    r2 <- M.read vec index2
    let ds = if r1 + r2 == 0 then [0] else List.reverse $ digits (r1 + r2)
        newLen = len + List.length ds
        newIdx i idx = (i + 1 + idx) `rem` newLen
        nb = List.reverse ds List.++ b
        nlen = List.length n
        stop = List.take nlen nb == n || List.take nlen (List.drop 1 nb) == n
    addRecipes ds len vec
    unless stop $ findRecipesM (List.take nlen nb) n (newIdx r1 index1) (newIdx r2 index2) newLen vec

findRecipes' :: (PrimMonad m) => [Int] -> m (Vector Int)
findRecipes' i = do
    vec <- M.replicate 50000000 (-1)
    M.write vec 0 3
    M.write vec 1 7
    findRecipesM [] i 0 1 2 vec
    V.unsafeFreeze vec
