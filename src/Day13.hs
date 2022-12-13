module Day13 where

import Data.Attoparsec.Text (Parser, char, decimal, endOfInput, endOfLine, sepBy, skipSpace)
import Exercise (Exercise (..), Solution (..))

exercise :: Exercise
exercise =
  Exercise
    { exerciseNum = 13
    , exerciseParser = parser
    , exerciseSolutions =
        [ Solution "Part 1" part1
        , Solution "Part 2" part2
        ]
    }

data Packet
  = L [Packet]
  | N Int
  deriving (Show, Eq)

type Input = [(Packet, Packet)]

parser :: Parser Input
parser =
  (((,) <$> packet <* endOfLine <*> packet) `sepBy` (endOfLine *> endOfLine)) <* skipSpace <* endOfInput
 where
  packet = list <|> number
  list = L <$> (char '[' *> (packet `sepBy` (char ',' *> skipSpace))) <* char ']'
  number = N <$> decimal

instance Ord Packet where
  compare (N a) (N b) = compare a b
  compare (L a) (L b) = compare a b
  compare (L a) (N b) = compare a [N b]
  compare (N a) (L b) = compare [N a] b

part1 :: Input -> Int
part1 = sum . map fst . filter (\(_, (a, b)) -> a <= b) . zip [1 ..]

part2 :: Input -> Int
part2 = computeKey . sort . (dividers ++) . (>>= \(a, b) -> [a, b])
 where
  divider1 = L [L [N 2]]
  divider2 = L [L [N 6]]
  dividers = [divider1, divider2]

  computeKey ps =
    let indexed = zip [1 ..] ps
     in case (find ((== divider1) . snd) indexed, find ((== divider2) . snd) indexed) of
          (Just (i1, _), Just (i2, _)) -> i1 * i2
          _ -> error "Divider packets not found."
