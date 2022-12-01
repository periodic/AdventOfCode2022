module Day01 where

import Data.Attoparsec.Text (endOfLine, sepBy, decimal, Parser)
import Data.Foldable (maximum)
import Data.List qualified as List

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

part2Simple :: Input -> Int
part2Simple = sum . take 3 . reverse . sort . map sum

part2 :: Input -> Int
part2 = sum . findNLargest 3 . map sum
  where
    findNLargest n list = findNLargest' n [] list

    findNLargest' _ found [] = found
    findNLargest' n found (l:ls)
      | length found < n = findNLargest' n (l : found) ls
      | l > List.head found = findNLargest' n (List.insert l $ take (n - 1) found) ls
      | otherwise = findNLargest' n found ls