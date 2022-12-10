module Day10 where

import Data.Attoparsec.Text (Parser, decimal, endOfInput, endOfLine, sepBy1, signed, skipSpace, string)
import Data.Set qualified as Set
import Text.Show (Show (..))

import Exercise (Exercise (..), Solution (..))

exercise :: Exercise
exercise =
  Exercise
    { exerciseNum = 10
    , exerciseParser = parser
    , exerciseSolutions =
        [ Solution "Part 1" part1
        , Solution "Part 2" part2
        ]
    }

data Instruction
  = Add Int
  | Noop
  deriving (Show, Eq)

type Program = [Instruction]

data SystemState = SystemState
  { systemCycle :: Int
  , systemRegister :: Int
  }
  deriving (Show)

parser :: Parser Program
parser =
  (instruction `sepBy1` endOfLine) <* skipSpace <* endOfInput
 where
  instruction = add <|> noop
  add = Add <$> (string "addx" *> skipSpace *> signed decimal)
  noop = Noop <$ string "noop"

eval :: Instruction -> SystemState -> SystemState
eval Noop st = st{systemCycle = systemCycle st + 1}
eval (Add n) st = st{systemCycle = systemCycle st + 2, systemRegister = systemRegister st + n}

evalProgram :: Program -> [SystemState]
evalProgram =
  (initialState :) . evalProgram' initialState
 where
  initialState = SystemState{systemCycle = 1, systemRegister = 1}
  -- There should be a pattern for this.
  -- It's not a standard fold because the state threading through and the output don't match
  evalProgram' _ [] = []
  evalProgram' reg (inst : rest) =
    let reg' = eval inst reg
     in reg' : evalProgram' reg' rest

signalStrength :: SystemState -> Int
signalStrength SystemState{..} =
  systemCycle * systemRegister

part1 :: Program -> Int
part1 =
  sum
    . map signalStrength
    . extractCycles [20, 60 ..]
    . evalProgram

extractCycles :: [Int] -> [SystemState] -> [SystemState]
extractCycles (n : ns) (st1 : st2 : sts) =
  let currState = SystemState{systemCycle = n, systemRegister = systemRegister st1}
   in if systemCycle st1 <= n && systemCycle st2 > n
        then currState : extractCycles ns (st1 : st2 : sts)
        else extractCycles (n : ns) (st2 : sts)
extractCycles _ _ = []

newtype Output = Output (Set Int)

instance Show Output where
  show (Output pixels) =
    render [1 .. 40]
      <> "\n"
      <> render [41 .. 80]
      <> "\n"
      <> render [81 .. 120]
      <> "\n"
      <> render [121 .. 160]
      <> "\n"
      <> render [161 .. 200]
      <> "\n"
      <> render [201 .. 240]
   where
    render = map (\i -> if Set.member i pixels then '#' else '.')

part2 :: Program -> Output
part2 =
  Output
    . Set.fromList
    . map systemCycle
    . filter (\SystemState{..} -> abs (((systemCycle - 1) `mod` 40) - systemRegister) <= 1)
    . extractCycles [1 ..]
    . evalProgram
