
module Day15 where

import Data.Attoparsec.Text (Parser, decimal, endOfInput, endOfLine, sepBy, skipSpace, string, signed)
import Data.List qualified as List
import Exercise (Exercise (..), Solution (..))
import Linear (V2 (..))

exercise :: Exercise
exercise =
  Exercise
    { exerciseNum = 15
    , exerciseParser = parser
    , exerciseSolutions =
        [ Solution "Part 1" part1
        , Solution "Part 2" part2
        ]
    }

type Coord = V2 Int
type Sensor = (Coord, Coord)
type Input = [(Coord, Coord)]

parser :: Parser Input
parser =
  (sensor `sepBy` endOfLine) <* skipSpace <* endOfInput
  where
    sensor =
      (,)
        <$> (string "Sensor at " *> coord)
        <* string ": closest beacon is at "
        <*> coord
    coord =
      V2
        <$> (string "x=" *> signed decimal)
        <* string ", y="
        <*> signed decimal

getX :: V2 a -> a
getX (V2 x _) = x

getY :: V2 a -> a
getY (V2 _ y) = y

bounds :: Input -> (Coord, Coord)
bounds sensors =
  let allCoords = concatMap (\(a, b) -> [a, b]) sensors
      xs = map getX allCoords
      ys = map getY allCoords
      minX = List.minimum xs
      minY = List.minimum ys
      maxX = List.maximum xs
      maxY = List.maximum ys
  in (V2 minX minY, V2 maxX maxY)

overlapInRow :: Int -> Sensor -> Maybe (Int, Int)
overlapInRow row (V2 sx sy, V2 bx by) =
  let db = abs (sx - bx) + abs (sy - by)
      dy = abs (row - sy)
      dx = db - dy
      x1 = sx - dx
      x2 = sx + dx
  in if dy > db
    then Nothing
    else Just (x1, x2)

addRanges :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
addRanges r [] = [r]
addRanges r@(a, b) rs@((a1, b1):rest)
  | b < a1 = r : rs -- less than all entries -> add to front of list
  | b <= b1 = (min a a1, b1) : rest -- overlaps on the front of the first range -> add to range
  | a <= b1 = addRanges (min a a1, max b b1) rest -- overlaps but extends on the back -> merge and then check cascading merges
  | otherwise = (a1, b1) : addRanges r rest -- No overlap --> proceed to next

part1 :: Input -> Int
part1 = calculateNotBeacon 2000000

coverage :: Int -> Input -> [(Int, Int)]
coverage row = foldr addRanges [] . mapMaybe (overlapInRow row)

calculateNotBeacon :: Int -> Input -> Int
calculateNotBeacon row input =
  let coveredRanges = coverage row input
      totalCovered = sum $ map (\(a, b) -> b - a + 1) coveredRanges
      beaconsInRow = length . List.nub . filter ((== row) . getY) . map snd $ input
  in totalCovered - beaconsInRow

part2 :: Input -> Int
part2 = tuningFreq . findBeacon (0,4000000)

tuningFreq :: V2 Int -> Int
tuningFreq (V2 x y) = 4000000 * x + y

findBeacon :: (Int, Int) -> Input -> V2 Int
findBeacon (minC, maxC) input =
  let ranges = zip [minC..maxC] $ map (`coverage` input) [minC .. maxC]
      interestingRange = find (\(_, ((a, b):rest)) -> rest /= [] || a > minC || b < maxC) ranges
  in case interestingRange of
    Nothing -> error "No interesting ranges found"
    Just (i, ranges) -> 
      let gap = findFirstHole ranges
      in V2 gap i
  where
    findFirstHole [] = error "No hole in range"
    findFirstHole [(a, b)] 
      | a > minC = minC
      | otherwise = maxC
    findFirstHole ((_, b1):(_, _):_) = b1 + 1
