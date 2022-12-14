module Day14 where

import Data.Attoparsec.Text (Parser, char, decimal, endOfInput, endOfLine, sepBy, skipSpace, string)
import Data.Ix (Ix (range))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Exercise (Exercise (..), Solution (..))
import Linear (V2 (..))

exercise :: Exercise
exercise =
  Exercise
    { exerciseNum = 14
    , exerciseParser = parser
    , exerciseSolutions =
        [ Solution "Part 1" part1
        , Solution "Part 2" part2
        ]
    }

type Coord = V2 Int

type Input = [[Coord]]

parser :: Parser Input
parser =
  ((coord `sepBy` string " -> ") `sepBy` endOfLine) <* skipSpace <* endOfInput
 where
  coord = V2 <$> decimal <* char ',' <*> decimal

data Cell
  = Rock
  | Sand
  deriving (Show, Eq, Ord)

type RockMap = Map Coord Cell

inputToRockMap :: Input -> RockMap
inputToRockMap =
  foldr makeRocks Map.empty
 where
  makeRocks (c1 : c2 : rest) rockMap =
    let cs = if c1 < c2 then range (c1, c2) else range (c2, c1)
        rockMap' = foldr (`Map.insert` Rock) rockMap cs
     in makeRocks (c2 : rest) rockMap'
  makeRocks _ rockMap = rockMap

getY :: V2 a -> a
getY (V2 _ y) = y

data SandResult
  = Done
  | Next
  | FallDown
  | FallDownLeft
  | FallDownRight

type SandBehavior = Coord -> Maybe Cell -> Maybe Cell -> Maybe Cell -> SandResult

dropSand :: SandBehavior -> RockMap -> Maybe RockMap
dropSand behavior rockMap = do
  sandLoc <- execSand (V2 500 0)
  pure $ Map.insert sandLoc Sand rockMap
 where
  execSand coord =
    let down = coord + V2 0 1
        downLeft = coord + V2 (-1) 1
        downRight = coord + V2 1 1
     in case behavior coord (Map.lookup down rockMap) (Map.lookup downLeft rockMap) (Map.lookup downRight rockMap) of
          FallDown -> execSand down
          FallDownLeft -> execSand downLeft
          FallDownRight -> execSand downRight
          Next -> Just coord
          Done -> Nothing

iterateUntilDone :: Int -> (a -> Maybe a) -> a -> (Int, a)
iterateUntilDone i f a =
  case f a of
    Nothing -> (i, a)
    Just a' -> iterateUntilDone (i + 1) f a'

part1 :: Input -> Int
part1 input =
  fst $ iterateUntilDone 0 (dropSand sandWithVoid) rockMap
 where
  rockMap = inputToRockMap input
  maxY = List.maximum . map getY . concat $ input
  sandWithVoid coord down downLeft downRight =
    if getY coord >= maxY
      then Done
      else case (down, downLeft, downRight) of
        (Nothing, _, _) -> FallDown
        (_, Nothing, _) -> FallDownLeft
        (_, _, Nothing) -> FallDownRight
        _ -> Next

part2 :: Input -> Int
part2 input =
  (+ 1) . fst $ iterateUntilDone 0 (dropSand sandWithFloor) rockMap
 where
  rockMap = inputToRockMap input
  maxY = List.maximum . map getY . concat $ input
  sandWithFloor coord down downLeft downRight =
    if getY coord == maxY + 1
      then Next
      else case (down, downLeft, downRight) of
        (Nothing, _, _) -> FallDown
        (_, Nothing, _) -> FallDownLeft
        (_, _, Nothing) -> FallDownRight
        _ ->
          if coord == V2 500 0
            then Done
            else Next