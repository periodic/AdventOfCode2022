module Day03 where

import Data.Attoparsec.Text (Parser, endOfInput, endOfLine, letter, sepBy, skipSpace)
import Data.Char (isLower, isUpper)
import Data.List (foldr1, intersect, nub)

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
  | isLower x = ord x - ord 'a' + 1 -- Remove 96 to one-index.
  | isUpper x = ord x - ord 'A' + 27 -- remove 64 to one-index, then add 26 for the second half.
  | otherwise = error $ "Got invalid bag item: " <> show x

findShared :: [String] -> Char
findShared bags =
  case nub $ foldr1 intersect bags of
    [] -> error $ "Bags have no common elements: " <> show bags
    [x] -> x
    xs -> error $ "Bags have more than one common element: " <> show xs <> " in " <> show bags

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
