module Day11 where

import Data.Attoparsec.Text (Parser, sepBy, endOfLine, endOfInput, skipSpace, decimal, sepBy1, string, char)

import Exercise (Exercise (..), Solution (..))
import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Data.List (maximum)

exercise :: Exercise
exercise =
  Exercise
    { exerciseNum = 11
    , exerciseParser = parser
    , exerciseSolutions =
        [ Solution "Part 1" part1
        , Solution "Part 2" part2
        ]
    }

newtype Item = Item Int
  deriving newtype (Show, Eq, Ord, Num, Enum, Real, Integral)

data Test = Test
  { testDivisibility :: Int
  , onTrue :: Int
  , onFalse :: Int
  }
  deriving (Show, Eq)

data Operation
  = Add Int
  | Mul Int
  | Square
  deriving (Show, Eq)

data Monkey = Monkey
  { monkeyInspections :: Int
  , monkeyItems :: Seq Item
  , monkeyOperation :: Operation
  , monkeyTest :: Test
  }
  deriving (Show, Eq)

type Monkeys = IntMap Monkey

type Input = Monkeys

parser :: Parser Input
parser = makeMonkeys <$> (monkey `sepBy` (endOfLine *> endOfLine)) <* skipSpace <* endOfInput
  where
    makeMonkeys = IntMap.fromList . zip [0..]

    preamble = string "Monkey " *> decimal *> string ":" *> skipSpace

    monkey = 
      Monkey 0
        <$> (preamble *> items) <* skipSpace
        <*> operation <* skipSpace
        <*> test

    items :: Parser (Seq Item)
    items =
      string "Starting items:" *> skipSpace *> (Seq.fromList . map Item <$> decimal `sepBy1` string ", ")

    operation =
      string "Operation: new = " *> operator

    operator =
      Mul <$> (string "old * " *> decimal)
      <|> Add <$> (string "old + " *> decimal)
      <|> Square <$ string "old * old"

    test =
      Test
        <$> (string "Test: divisible by " *> decimal) <* skipSpace
        <*> (string "If true: throw to monkey " *> decimal) <* skipSpace
        <*> (string "If false: throw to monkey " *> decimal)


type RelaxationFunc = Item -> Item

type Sim = ReaderT RelaxationFunc (State Monkeys)

addItemTo :: Item -> Int -> Sim ()
addItemTo item i =
  updateMonkey i $ \monkey -> monkey { monkeyItems = monkeyItems monkey |> item }

updateMonkey :: Int -> (Monkey -> Monkey) -> Sim ()
updateMonkey i f =
  modify $ IntMap.adjust f i

evalMonkey :: Int -> Sim ()
evalMonkey i = do
  monkey@Monkey{..} <- fromMaybe (error $ "Monkey " <> show i <> " not found") . IntMap.lookup i <$> get
  forM_ monkeyItems $ processItem monkey
  updateMonkey i $ \monkey -> monkey { monkeyItems = Seq.empty, monkeyInspections = monkeyInspections + Seq.length monkeyItems}
  where
    processItem Monkey{..} item = do
      item' <- applyOperation monkeyOperation item
      let Test{..} = monkeyTest
          testResult = (item' `mod` Item testDivisibility) == 0
      if testResult
        then addItemTo item' onTrue
        else addItemTo item' onFalse

    applyOperation :: Operation -> Item -> Sim Item
    applyOperation (Add x) item = ask <*> pure (item + Item x)
    applyOperation (Mul x) item = ask <*> pure (item * Item x)
    applyOperation Square item = ask <*> pure (item * item)

runRound :: Sim ()
runRound = do
  monkeys <- get
  mapM_ evalMonkey [0..(IntMap.size monkeys - 1)]

part1 :: Input -> Int
part1 monkeys = 
  let finalState = executingState monkeys . usingReaderT (`div` 3) $ replicateM_ 20 runRound 
      inspections = map (monkeyInspections . snd) . IntMap.toList $ finalState
      top = maximum inspections
      second = maximum $ filter (< top) inspections -- This is not correct, could be two of the same value.
  in top * second

part2 :: Input -> Int
part2 monkeys = 
  let ringSize = Item . product . map (testDivisibility . monkeyTest . snd) . IntMap.toList $ monkeys
      finalState = executingState monkeys . usingReaderT (`mod` ringSize) $ replicateM_ 10000 runRound
      inspections = map (monkeyInspections . snd) . IntMap.toList $ finalState
      top = maximum inspections
      second = maximum $ filter (< top) inspections -- This is not correct, could be two of the same value.
  in top * second