module Day2
  ( solution1, solution2
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe

solution1 :: [String] -> Int
solution1 xs = two * three
  where
    (two, three) = foldl (\(a, b) s -> let (c, d) = twoAndThree s in (a + c, b + d)) (0, 0) xs

frequencies :: (Foldable t, Ord a) => t a -> [Int]
frequencies xs =
  Map.elems $ foldl (\map elem -> Map.insertWith (+) elem 1 map) Map.empty xs

twoAndThree :: String -> (Int, Int)
twoAndThree xs =
  let fs = frequencies xs
      lenFilter n xs = if n `elem` xs then 1 else 0
  in (lenFilter 2 fs, lenFilter 3 fs)

solution2 :: [String] -> String
solution2 [] = "?"
solution2 (x:xs) = Maybe.fromMaybe (solution2 xs) $ findDiff xs x

strDiff :: Int -> String -> String -> String -> Maybe String
strDiff diff result [] [] = if diff == 1 then Just $ reverse result else Nothing
strDiff diff result (x:xs) (y:ys) = if x /= y
                                    then if 1 == diff then Nothing else strDiff (1+diff) result xs ys
                                    else strDiff diff (x : result) xs ys

findDiff :: [String] -> String -> Maybe String
findDiff [] _ = Nothing
findDiff (x:xs) s = case strDiff 0 [] s x of
                      Nothing -> findDiff xs s
                      just -> just
