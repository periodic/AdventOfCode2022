module Day08 where

import Data.Attoparsec.Text (Parser, digit)
import Data.Char (digitToInt)
import Utils.ParserHelpers (parseGrid)

import Exercise (Exercise(..), Solution (..))
import Day08.Map qualified as MapSol

exercise :: Exercise
exercise = Exercise {
  exerciseNum = 8,
  exerciseParser = parser,
  exerciseSolutions =
    [ Solution "Part 1" MapSol.part1
    , Solution "Part 2" MapSol.part2
    ]
}

type Input = [((Int, Int), Int)]

parser :: Parser Input
parser =
  parseGrid (some (digitToInt <$> digit))
