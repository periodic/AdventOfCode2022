module Day01 where

import Data.Attoparsec.Text (endOfLine, sepBy, decimal, Parser)
import Data.Foldable (maximum)

import Exercise ( Exercise(..), Solution(..) )

type Input = [[Int]]

exercise :: Exercise
exercise = Exercise {
  exerciseName = ["Day01"], 
  exerciseSolution = Solution {
    solutionParser = parser,
    solutionPart1 = part1,
    solutionPart2 = part2
  }
}

parser :: Parser Input
parser =
  (decimal `sepBy` endOfLine) `sepBy` (endOfLine *> endOfLine)

part1 :: Input -> Int
part1 = maximum . map sum

part2 :: Input -> Int
part2 = sum . take 3 . reverse . sort . map sum