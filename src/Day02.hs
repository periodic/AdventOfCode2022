module Day02 where

import Data.Attoparsec.Text (Parser, char, endOfInput, endOfLine, sepBy, skipSpace)
import Exercise (
  Exercise (..),
  Solution (..),
 )

exercise :: Exercise
exercise =
  Exercise
    { exerciseNum = 2
    , exerciseParser = parser
    , exerciseSolutions =
        [ Solution "Part 1" part1
        , Solution "Part 2" part2
        ]
    }

data Throw = Rock | Paper | Scissors
  deriving (Show, Eq)

better :: Throw -> Throw
better Rock = Paper
better Paper = Scissors
better Scissors = Rock

worse :: Throw -> Throw
worse Paper = Rock
worse Scissors = Paper
worse Rock = Scissors

data UnknownCol = X | Y | Z

type Input = [(Throw, UnknownCol)]

parser :: Parser Input
parser =
  (round `sepBy` endOfLine) <* skipSpace <* endOfInput
 where
  round = (,) <$> (theirThrow <* skipSpace) <*> ourThrow

  theirThrow =
    (char 'A' $> Rock)
      <|> (char 'B' $> Paper)
      <|> (char 'C' $> Scissors)

  ourThrow =
    (char 'X' $> X)
      <|> (char 'Y' $> Y)
      <|> (char 'Z' $> Z)

data Result = Player1Won | Draw | Player2Won
  deriving (Eq)

roundResult :: Throw -> Throw -> Result
roundResult them us
  | them == better us = Player1Won
  | us == better them = Player2Won
  | otherwise = Draw

score :: Throw -> Throw -> Int
score them us =
  let shapeScore = case us of
        Rock -> 1
        Paper -> 2
        Scissors -> 3
      resultScore = case roundResult them us of
        Player1Won -> 0
        Draw -> 3
        Player2Won -> 6
   in shapeScore + resultScore

part1 :: Input -> Int
part1 = sum . map (uncurry score . fmap unknownToThrow)
 where
  unknownToThrow X = Rock
  unknownToThrow Y = Paper
  unknownToThrow Z = Scissors

part2 :: Input -> Int
part2 = sum . map (uncurry score . makeChoice)
 where
  makeChoice (them, X) = (them, worse them)
  makeChoice (them, Z) = (them, better them)
  makeChoice (them, Y) = (them, them)
