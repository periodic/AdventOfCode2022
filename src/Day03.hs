module Day03 where

import Data.Attoparsec.Text (Parser, endOfInput, endOfLine, letter, sepBy, skipSpace)
import Data.Char (isLower, isUpper)
import Data.List (foldr1, intersect)

import Exercise (Exercise (..), Solution (..))

exercise :: Exercise
exercise =
  Exercise
    { exerciseNum = 3
    , exerciseParser = parser
    , exerciseSolutions =
        [ Solution "Part 1" part1
        , Solution "Part 2" part2
        ]
    }

type Input = [String]

parser :: Parser Input
parser =
  (some letter `sepBy` endOfLine) <* skipSpace <* endOfInput

toPriority :: Char -> Int
toPriority x
  | isUpper x = ord x - 64 + 26
  | isLower x = ord x - 96
  | otherwise = error $ "Got invalid bag item: " <> show x

findShared :: [String] -> Char
findShared bags =
  case foldr1 intersect bags of
    [] -> error $ "Bags have no common elements: " <> show bags
    x : _ -> x

part1 :: Input -> Int
part1 = sum . map (toPriority . findShared . splitRow)
 where
  splitRow row =
    let index = length row `div` 2
        (left, right) = splitAt index row
     in [left, right]

part2 :: Input -> Int
part2 = sum . map (toPriority . findShared) . chunksOf 3
 where
  chunksOf _ [] = []
  chunksOf n xs =
    let (chunk, rest) = splitAt n xs
     in chunk : chunksOf n rest
