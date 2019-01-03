module Day21
  (
    solution1, solution2
  ) where

import Data.List as List
import Data.Map.Strict as Map
import Data.Set as Set

import qualified Day19 as D

solution1 :: (D.InstructionPointerRegister, D.Program) -> Int
solution1 code = D.solution1 code 1

lastUnique :: [Int] -> Set Int -> Int -> Int -> Int
lastUnique _ _ 0 last = last
lastUnique (x:xs) seen n last = let nextLast = if Set.member x seen then last else x
                                    nextSeen = Set.insert x seen
                                in lastUnique xs nextSeen (n - 1) nextLast

solution2 :: (D.InstructionPointerRegister, D.Program) -> Int
solution2 code = let values = (`D.registerValue` 1) <$> List.iterate (D.runElfCode code) (D.RegisterState 0 0 0 0 0 0)
                 in lastUnique values Set.empty 20000 0
