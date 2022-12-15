
module Day15Spec where

import Data.Attoparsec.Text (parseOnly)
import Linear (V2 (..))
import NeatInterpolation (text)
import Test.Hspec (Spec, describe, it, shouldBe)

import Day15

exampleText :: Text
exampleText =
  [text|
  Sensor at x=2, y=18: closest beacon is at x=-2, y=15
  Sensor at x=9, y=16: closest beacon is at x=10, y=16
  Sensor at x=13, y=2: closest beacon is at x=15, y=3
  Sensor at x=12, y=14: closest beacon is at x=10, y=16
  Sensor at x=10, y=20: closest beacon is at x=10, y=16
  Sensor at x=14, y=17: closest beacon is at x=10, y=16
  Sensor at x=8, y=7: closest beacon is at x=2, y=10
  Sensor at x=2, y=0: closest beacon is at x=2, y=10
  Sensor at x=0, y=11: closest beacon is at x=2, y=10
  Sensor at x=20, y=14: closest beacon is at x=25, y=17
  Sensor at x=17, y=20: closest beacon is at x=21, y=22
  Sensor at x=16, y=7: closest beacon is at x=15, y=3
  Sensor at x=14, y=3: closest beacon is at x=15, y=3
  Sensor at x=20, y=1: closest beacon is at x=15, y=3
  |]

exampleInput :: Input
exampleInput =
  [ (V2 2 18, V2 (-2) 15)
  , (V2 9 16, V2 10 16)
  , (V2 13 2, V2 15 3)
  , (V2 12 14, V2 10 16)
  , (V2 10 20, V2 10 16)
  , (V2 14 17, V2 10 16)
  , (V2 8 7, V2 2 10)
  , (V2 2 0, V2 2 10)
  , (V2 0 11, V2 2 10)
  , (V2 20 14, V2 25 17)
  , (V2 17 20, V2 21 22)
  , (V2 16 7, V2 15 3)
  , (V2 14 3, V2 15 3)
  , (V2 20 1, V2 15 3)
  ]

spec :: Spec
spec = do
  describe "Parsing" $ do
    it "handles the example input" $ do
      parseOnly parser exampleText `shouldBe` Right exampleInput

  describe "Part 1" $ do
    it "handles disconnected areas" $ do
      calculateNotBeacon 10 [(V2 0 0, V2 18 2), (V2 100 0, V2 118 2)] `shouldBe` 21 + 21
    it "handles beacons in the row" $ do
      calculateNotBeacon 10 [(V2 0 0, V2 (-10) 10)] `shouldBe` 20
    it "handles overlapping areas" $ do
      calculateNotBeacon 10 [(V2 0 0, V2 10 10), (V2 20 0, V2 10 10)] `shouldBe` 21 + 21 - 1 {- overlap -} - 1 {- beacon -}
    it "handles everything on the same row" $ do
      calculateNotBeacon 0 [(V2 0 0, V2 10 0), (V2 20 0, V2 10 0)] `shouldBe` ((30 - (-10)) + 1) - 1
    it "handles the example input" $ do
      calculateNotBeacon 10 exampleInput `shouldBe` 26

  describe "Part 2" $ do
    it "handles the example input" $ do
      findBeacon (0,20) exampleInput `shouldBe` V2 14 11