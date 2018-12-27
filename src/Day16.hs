{-# LANGUAGE RecordWildCards #-}

module Day16
  (
    solution1, Sample, findOpcodeNumbers, solution2, BinOpcode
  ) where

import Data.Bits as Bits
import Data.List as List
import Data.Map.Strict as Map
import Data.Set as Set

import Debug.Trace

data State = State
  { register_0 :: Int
  , register_1 :: Int
  , register_2 :: Int
  , register_3 :: Int
  } deriving (Eq, Show)

data OpcodeName
  = Addr
  | Addi
  | Mulr
  | Muli
  | Banr
  | Bani
  | Borr
  | Bori
  | Setr
  | Seti
  | Gtir
  | Gtri
  | Gtrr
  | Eqir
  | Eqri
  | Eqrr
  deriving (Eq, Show, Ord, Enum)

data Opcode = Opcode {name :: OpcodeName,
                      operand_0 :: Int,
                      operand_1 :: Int,
                      result :: Int} deriving (Eq, Show)

type Sample = ((Int, Int, Int, Int), (Int, Int, Int, Int), (Int, Int, Int, Int))

type BinOpcode = (Int, Int, Int, Int)

registerValue :: State -> Int -> Int
registerValue State{..} registerNumber
  | registerNumber == 0 = register_0
  | registerNumber == 1 = register_1
  | registerNumber == 2 = register_2
  | registerNumber == 3 = register_3
  | otherwise = error "Invalid register number"

setRegister :: State -> Int -> Int -> State
setRegister s@State{..} registerNumber value
  | registerNumber == 0 = s {register_0 = value}
  | registerNumber == 1 = s {register_1 = value}
  | registerNumber == 2 = s {register_2 = value}
  | registerNumber == 3 = s {register_3 = value}
  | otherwise = error "Invalid register number"

execute :: Opcode -> State -> State
execute o@Opcode{..} s@State{..} = case name of
                               Addr -> setRegister s result $ registerValue s operand_0 + registerValue s operand_1
                               Addi -> setRegister s result $ registerValue s operand_0 + operand_1
                               Mulr -> setRegister s result $ registerValue s operand_0 * registerValue s operand_1
                               Muli -> setRegister s result $ registerValue s operand_0 * operand_1
                               Banr -> setRegister s result $ registerValue s operand_0 .&. registerValue s operand_1
                               Bani -> setRegister s result $ registerValue s operand_0 .&. operand_1
                               Borr -> setRegister s result $ registerValue s operand_0 .|. registerValue s operand_1
                               Bori -> setRegister s result $ registerValue s operand_0 .|. operand_1
                               Setr -> setRegister s result (registerValue s operand_0)
                               Seti -> setRegister s result operand_0
                               Gtir -> setRegister s result r where r = if operand_0 > registerValue s operand_1 then 1 else 0
                               Gtri -> setRegister s result r where r = if registerValue s operand_0 > operand_1 then 1 else 0
                               Gtrr -> setRegister s result r where r = if registerValue s operand_0 > registerValue s operand_1 then 1 else 0
                               Eqir -> setRegister s result r where r = if operand_0 == registerValue s operand_1 then 1 else 0
                               Eqri -> setRegister s result r where r = if registerValue s operand_0 == operand_1 then 1 else 0
                               Eqrr -> setRegister s result r where r = if registerValue s operand_0 == registerValue s operand_1 then 1 else 0

allOpcodesWithOperands :: (Int, Int, Int, Int) -> [Opcode]
allOpcodesWithOperands (_, a, b, c) = List.map (\name -> Opcode name a b c) $ enumFromTo Addr Eqrr

stateFromList :: (Int, Int, Int, Int) -> State
stateFromList (a,b,c,d) = State a b c d

validOpcode :: State -> State -> Opcode -> Bool
validOpcode before after opcode = execute opcode before == after

validOpcodes :: State -> State -> [Opcode] -> [Opcode]
validOpcodes before after = List.filter (validOpcode before after)

solution1 :: [Sample] -> Int
solution1 xs = let opcodeNums = List.map (\(b,o,a) -> length $ validOpcodes (stateFromList b) (stateFromList a) $ allOpcodesWithOperands o) xs
               in length $ List.filter (>=3) opcodeNums

opcodeNumber :: (Int,Int,Int,Int) -> Int
opcodeNumber (n,_,_,_) = n

findOpcodeNumbers' :: [Sample] -> Map Int (Set OpcodeName) -> Map Int (Set OpcodeName)
findOpcodeNumbers' [] opcodes = opcodes
findOpcodeNumbers' ((b, o, a) : xs) opcodes =
  let allOpcodes = allOpcodesWithOperands o
      before = stateFromList b
      after = stateFromList a
      validNames = List.map name $ validOpcodes before after allOpcodes
      opNumber = opcodeNumber o
      nextOpcodes = Map.insertWith Set.intersection opNumber (Set.fromList validNames) opcodes
  in findOpcodeNumbers' xs nextOpcodes

findSingleOpcode :: Map Int (Set OpcodeName) -> Map Int OpcodeName -> Map Int OpcodeName
findSingleOpcode opcodes opmap =
  let singleCodes = Map.filter ((== 1) . Set.size) opcodes
      singleCodesMap = Map.map (head . Set.toList) singleCodes
      allFound = Set.fromList $ Map.elems singleCodesMap
      nextOpcodes = Map.map (`Set.difference` allFound) opcodes
  in if Map.null singleCodes
     then opmap
     else findSingleOpcode nextOpcodes $ Map.union opmap singleCodesMap

findOpcodeNumbers :: [Sample] -> Map Int OpcodeName
findOpcodeNumbers xs =
  findSingleOpcode numbers Map.empty where numbers = findOpcodeNumbers' xs Map.empty

solution2 :: [BinOpcode] -> Map Int OpcodeName -> Int
solution2 xs codes = let opcodes = List.map (\(a,b,c,d) -> Opcode (codes ! a) b c d) xs
                         state = List.foldl (flip execute) (State 0 0 0 0) opcodes
                     in register_0 state
