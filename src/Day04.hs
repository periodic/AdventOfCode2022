module Day04 where

import Data.Attoparsec.Text (Parser, char, decimal, endOfInput, endOfLine, sepBy, skipSpace)
import Data.Ix (inRange)

import Exercise (Exercise (..), Solution (..))

exercise :: Exercise
exercise =
  Exercise
    { exerciseNum = 4
    , exerciseParser = parser
    , exerciseSolutions =
        [ Solution "Part 1" part1
        , Solution "Part 2" part2
        ]
    }

type Range = (Int, Int)

type Input = [(Range, Range)]

parser :: Parser Input
parser =
  (pair `sepBy` endOfLine) <* skipSpace <* endOfInput
 where
  pair = (,) <$> range <* char ',' <*> range
  range = (,) <$> decimal <* char '-' <*> decimal

part1 :: Input -> Int
part1 = length . filter (uncurry fullOverlap)
 where
  -- These functions could be rewritten to use fewer comparisons, but `inRange` reads much better.
  fullOverlap a b = a `containedIn` b || b `containedIn` a
  containedIn (a1, a2) b =
    inRange b a1 && inRange b a2

part2 :: Input -> Int
part2 = length . filter (uncurry overlaps)
 where
  -- These functions could be rewritten to use fewer comparisons, but `inRange` reads much better.
  overlaps a b = a `endsInside` b || b `endsInside` a
  endsInside (a1, a2) b =
    inRange b a1 || inRange b a2