module Day14Spec where

import Data.Attoparsec.Text (parseOnly)
import Linear (V2 (..))
import NeatInterpolation (text)
import Test.Hspec (Spec, describe, it, shouldBe)
import Data.HashSet qualified as Set

import Day14

exampleText :: Text
exampleText =
  [text|
  498,4 -> 498,6 -> 496,6
  503,4 -> 502,4 -> 502,9 -> 494,9
  |]

exampleInput :: Input
exampleInput =
  [ [V2 498 4, V2 498 6, V2 496 6]
  , [V2 503 4, V2 502 4, V2 502 9, V2 494 9]
  ]

spec :: Spec
spec = do
  describe "Parsing" $ do
    it "handles the example input" $ do
      parseOnly parser exampleText `shouldBe` Right exampleInput

  describe "Part 1" $ do
    describe "rock map" $ do
      it "vertical, down" $ do
        inputToRockMap [[V2 1 1, V2 1 10]] `shouldBe` Set.fromList (map (\i -> (V2 1 i)) [1..10])
      it "horizontal, right" $ do
        inputToRockMap [[V2 1 1, V2 10 1]] `shouldBe` Set.fromList (map (\i -> (V2 i 1)) [1..10])
      it "vertical, up" $ do
        inputToRockMap [[V2 1 10, V2 1 1]] `shouldBe` Set.fromList (map (\i -> (V2 1 i)) [1..10])
      it "horizontal, left" $ do
        inputToRockMap [[V2 10 1, V2 1 1]] `shouldBe` Set.fromList (map (\i -> (V2 i 1)) [1..10])
    it "handles the example input" $ do
      part1 exampleInput `shouldBe` 24

  describe "Part 2" $ do
    it "handles the example input" $ do
      part2 exampleInput `shouldBe` 93