module Day09Spec where

import Data.Attoparsec.Text (parseOnly)
import NeatInterpolation (text)
import Test.Hspec

import Day09

exampleText :: Text
exampleText =
  [text|
  R 4
  U 4
  L 3
  D 1
  R 4
  D 1
  L 5
  R 2
  |]

exampleInput :: Input
exampleInput =
  [ R 4
  , U 4
  , L 3
  , D 1
  , R 4
  , D 1
  , L 5
  , R 2
  ]

biggerExample :: Input
biggerExample =
  [ R 5
  , U 8
  , L 8
  , D 3
  , R 17
  , D 10
  , L 25
  , U 20
  ]

spec :: Spec
spec = do
  describe "parsing" $ do
    it "should parse the example" $ do
      parseOnly parser exampleText `shouldBe` Right exampleInput

  describe "part 1" $ do
    it "should correctly handle the example" $ do
      part1 exampleInput `shouldBe` 13

  describe "part 2" $ do
    it "should correctly handle the example" $ do
      part2 exampleInput `shouldBe` 1
    it "should correctly handle up" $ do
      part2 [U 20] `shouldBe` (20 - 10 + 2)
    it "should correctly handle down" $ do
      part2 [D 20] `shouldBe` (20 - 10 + 2)
    it "should correctly handle left" $ do
      part2 [L 20] `shouldBe` (20 - 10 + 2)
    it "should correctly handle right" $ do
      part2 [R 20] `shouldBe` (20 - 10 + 2)
    it "should correctly handle the larger example" $ do
      part2 biggerExample `shouldBe` 36
