{-# LANGUAGE InstanceSigs #-}
module Day05 where

import Data.Attoparsec.Text (Parser, char, decimal, endOfInput, endOfLine, letter, sepBy1, skipSpace, space, string)
import Data.Vector (Vector, (!), (//))

import Exercise (Exercise (..), Solution (..))
import Text.Show (Show (..))

exercise :: Exercise
exercise =
    Exercise
        { exerciseNum = 5
        , exerciseParser = parser
        , exerciseSolutions =
            [ Solution "Part 1" part1
            , Solution "Part 2" part2
            ]
        }

newtype Crate = Crate {getCrateChar :: Char}
    deriving newtype (Show, Eq, Ord)

newtype Crates = Crates {
    toVector :: Vector [Crate] -- Using a vector for efficient indexing
    } deriving newtype (Show)

data Move = Move
    { moveAmount :: Int
    , moveFrom :: Int
    , moveTo :: Int
    }
    deriving (Show)

type Input = (Crates, [Move])

parser :: Parser Input
parser =
    (,)
        <$> parseCrates
        <* endOfLine
        <* endOfLine
        <*> (parseMove `sepBy1` endOfLine)
        <* skipSpace
        <* endOfInput

parseCrates :: Parser Crates
parseCrates =
    (Crates . fromList <$> parseGrid)
        <* endOfLine
        <* parseFooter
  where
    -- A single crate
    parseCrate :: Parser Crate
    parseCrate =
        Crate <$> (char '[' *> letter <* char ']')
    -- A crate or the blank space
    parseCell :: Parser (Maybe Crate)
    parseCell =
        (Just <$> parseCrate)
            <|> (Nothing <$ (string "   " <|> fail "empty cell"))
    parseRow =
        parseCell `sepBy1` char ' '
    -- The whole grid of crates and spaces.
    parseGrid :: Parser [[Crate]]
    parseGrid =
        map catMaybes -- Remove the empty spaces from the top of the stacks
            . transpose -- Switch from row-first to column-first
            <$> parseRow
            `sepBy1` endOfLine
    parseFooter :: Parser [Int]
    parseFooter =
        char ' ' *> (decimal `sepBy1` (string "   " <|> fail "footer sep")) <* space

parseMove :: Parser Move
parseMove =
    Move
        <$> ( string "move "  -- This needs to be nested due to operator precedence.
                *> decimal
            )
        <* string " from "
        <*> ((+ (-1)) <$> decimal)
        <* string " to "
        <*> ((+ (-1)) <$> decimal)

newtype Output = Output String

instance Show Output where
    show :: Output -> String
    show (Output text) = text

part1 :: Input -> Output
part1 (crates, moves) = Output . getTop . foldr makeMove crates $ reverse moves
  where
    getTop =
        map (maybe ' ' getCrateChar . viaNonEmpty head) . toList . toVector
    makeMove :: Move -> Crates -> Crates
    makeMove Move{..} (Crates crateVector) =
        Crates $
            let (movedCrates, fromRow') = splitAt moveAmount $ crateVector ! moveFrom
                toRow' = reverse movedCrates <> crateVector ! moveTo
             in crateVector // [(moveFrom, fromRow'), (moveTo, toRow')]

part2 :: Input -> Output
part2 (crates, moves) = Output . getTop . foldr makeMove crates $ reverse moves
  where
    getTop =
        map (maybe ' ' getCrateChar . viaNonEmpty head) . toList . toVector
    makeMove :: Move -> Crates -> Crates
    makeMove Move{..} (Crates crateVector) =
        Crates $
            let (movedCrates, fromRow') = splitAt moveAmount $ crateVector ! moveFrom
                toRow' = movedCrates <> crateVector ! moveTo
             in crateVector // [(moveFrom, fromRow'), (moveTo, toRow')]
