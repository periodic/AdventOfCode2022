module Day06 where

import Data.Attoparsec.Text (Parser, endOfInput, letter, skipSpace)
import Data.Map qualified as Map
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq

import Exercise (Exercise (..), Solution (..))
import Day06.Windows qualified as Windows

type Input = String

exercise :: Exercise
exercise =
  Exercise
    { exerciseNum = 6
    , exerciseParser = parser
    , exerciseSolutions =
        [ Solution "Part 1" part1
        , Solution "Part 1 - Windows" Windows.part1
        , Solution "Part 2" part2
        , Solution "Part 2 - Windows" Windows.part2
        ]
    }

parser :: Parser Input
parser = many letter <* skipSpace <* endOfInput

data Queue a = Queue
  { contents :: Seq a
  , counts :: Map a Int
  }
  deriving (Show)

instance Foldable Queue where
  foldr f x (Queue{contents}) = foldr f x contents

emptyQueue :: Queue a
emptyQueue = Queue Seq.empty Map.empty

isAllUnique :: Queue a -> Bool
isAllUnique Queue{counts} =
  all (== 1) counts

push :: Ord a => a -> Queue a -> Queue a
push a (Queue{..}) =
  let contents' = contents |> a
      counts' = Map.alter increment a counts
  in Queue contents' counts'
  where
    increment (Just n) = Just $ n + 1
    increment Nothing = Just 1

pop :: Ord a => Queue a -> Queue a
pop (Queue{..}) =
  case contents of
    Empty -> error "Attempted to pop an empy queue"
    (a :<| contents') ->
      Queue contents' $ Map.update decrement a counts
  where
    decrement n 
      | n > 1 = Just (n - 1)
      | otherwise = Nothing

findUniqueSubstr :: (Eq a, Ord a) => Int -> [a] -> Int
findUniqueSubstr len =
  findSubstr 0 emptyQueue
 where
  findSubstr i queue (x : xs)
    | length queue < len = findSubstr (i + 1) (push x queue) xs
    | isAllUnique queue = i
    | otherwise = findSubstr (i + 1) (push x . pop $ queue) xs
  findSubstr _ _ [] = error "Reached end of input"

part1 :: Input -> Int
part1 = findUniqueSubstr 4

part2 :: Input -> Int
part2 = findUniqueSubstr 14