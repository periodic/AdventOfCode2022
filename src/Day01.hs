module Day01 where

import Data.Attoparsec.Text (Parser, decimal, endOfLine, sepBy)
import Data.Foldable (maximum)
import Data.List qualified as List

import Exercise (Exercise (..), Solution (..))

type Input = [[Int]]

exercise :: Exercise
exercise =
  Exercise
    { exerciseNum = 1
    , exerciseParser = parser
    , exerciseSolutions =
        [ Solution
            { solutionName = "Part 1"
            , solutionExec = part1
            }
        , Solution
            { solutionName = "Part 2 - Sorted"
            , solutionExec = part2Simple
            }
        , Solution
            { solutionName = "Part 2 - Selection"
            , solutionExec = part2Selection
            }
        ]
    }

parser :: Parser Input
parser =
  (decimal `sepBy` endOfLine) `sepBy` (endOfLine *> endOfLine)

part1 :: Input -> Int
part1 = maximum . map sum

part2Simple :: Input -> Int
part2Simple = sum . take 3 . reverse . sort . map sum

part2Selection :: Input -> Int
part2Selection = sum . findNLargest 3 . map sum
 where
  findNLargest n = findNLargest' n [] 

  findNLargest' _ found [] = found
  findNLargest' n found (l : ls)
    | length found < n = findNLargest' n (l : found) ls
    | l > List.head found = findNLargest' n (List.insert l $ take (n - 1) found) ls
    | otherwise = findNLargest' n found ls