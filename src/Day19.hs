{-# LANGUAGE RecordWildCards #-}

module Day19
  (
    parseCode, solution1, solution2, InstructionPointerRegister, Program, RegisterState (..), runElfCode, registerValue
  ) where

import Data.Bits as Bits
import Data.List as List
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Char as Char

import Control.Monad.State
import Text.Regex.PCRE

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
  | Nop
  deriving (Eq, Show, Ord, Enum, Read)

data RegisterState = RegisterState
  { register_0 :: Int
  , register_1 :: Int
  , register_2 :: Int
  , register_3 :: Int
  , register_4 :: Int
  , register_5 :: Int
  } deriving (Eq, Show)

data Opcode = Opcode {name :: OpcodeName,
                      operand_0 :: Int,
                      operand_1 :: Int,
                      result :: Int} deriving (Eq, Show)

type Program = Map Int Opcode
type ReturnRegister = Int
type InstructionPointerRegister = Int

registerValue :: RegisterState -> Int -> Int
registerValue RegisterState{..} registerNumber
  | registerNumber == 0 = register_0
  | registerNumber == 1 = register_1
  | registerNumber == 2 = register_2
  | registerNumber == 3 = register_3
  | registerNumber == 4 = register_4
  | registerNumber == 5 = register_5
  | otherwise = error "Invalid register number"

setRegister :: RegisterState -> Int -> Int -> RegisterState
setRegister s@RegisterState{..} registerNumber value
  | registerNumber == 0 = s {register_0 = value}
  | registerNumber == 1 = s {register_1 = value}
  | registerNumber == 2 = s {register_2 = value}
  | registerNumber == 3 = s {register_3 = value}
  | registerNumber == 4 = s {register_4 = value}
  | registerNumber == 5 = s {register_5 = value}
  | otherwise = error "Invalid register number"

execute :: Opcode -> RegisterState -> RegisterState
execute o@Opcode{..} s@RegisterState{..} = case name of
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
                               Nop  -> s

readOpcode :: String -> Int -> Int -> Int -> Opcode
readOpcode name op0 op1 res = let n = Char.toUpper (head name) : tail name
                                  opname = read n :: OpcodeName
                              in Opcode opname op0 op1 res

runCode :: Program -> InstructionPointerRegister -> ReturnRegister -> Int -> State RegisterState Int
runCode _ _ returnRegister (-1) =
  do
    state <- get
    return $ registerValue state returnRegister
runCode program ipRegister returnRegister instructionPointer =
  do
    state <- get
    let state0 = setRegister state ipRegister instructionPointer
        opcode = program ! instructionPointer
        state1 = execute opcode state0
        nextIp = 1 + registerValue state1 ipRegister
    put state1
    runCode program ipRegister returnRegister (if Map.member nextIp program then nextIp else (-1))

parseCode' :: [String] -> Int -> [(Int, Opcode)]
parseCode' [] _ = []
parseCode' (x:xs) i =
  let result = x =~ "(\\w+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)" :: AllTextSubmatches [] String
      (_:op:rs) = getAllTextSubmatches result
      [a,b,c] = fmap read rs :: [Int]
      opcode = readOpcode op a b c
  in (i, opcode) : parseCode' xs (i + 1)

parseCode :: [String] -> (InstructionPointerRegister , Program)
parseCode (x:xs) =
    let result = x =~ "\\d+" :: AllTextSubmatches [] String
        (r:_) = read <$> getAllTextSubmatches result :: [Int]
    in (r, Map.fromList $ parseCode' xs 0)

solution1 :: (InstructionPointerRegister , Program) -> Int -> Int
solution1 (ipNum, code) returnReg = evalState (runCode code ipNum returnReg 0) (RegisterState 0 0 0 0 0 0)

runElfCode :: (InstructionPointerRegister , Program) -> RegisterState -> RegisterState
runElfCode (ipNum, code) = execState (runCode code ipNum 0 0)

sumOfFactors :: Int -> Int
sumOfFactors n = let s = truncate $ sqrt $ fromIntegral n
                 in List.foldl' (\a x -> let (d,r) = quotRem n x
                                         in if r==0 then a + (if d == x then d else x + d)
                                                    else a) 0 [1..s]


solution2 :: Int -> Int
solution2 = sumOfFactors
