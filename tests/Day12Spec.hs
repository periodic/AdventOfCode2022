module Day12Spec where

import Test.Hspec (Spec, describe, it, shouldBe)

import Data.Attoparsec.Text (parseOnly)
import qualified Data.Map.Strict as Map
import Day12
import NeatInterpolation (text)

exampleText :: Text
exampleText =
  [text|
  Sabqponm
  abcryxxl
  accszExk
  acctuvwj
  abdefghi
  |]

exampleInput :: Input
exampleInput =
  Input
    { start = (1, 1)
    , end = (6, 3)
    , grid =
        Map.fromList $
          zipWith (\i c -> ((i, 1), Height $ ord c)) [1 ..] "aabqponm"
            <> zipWith (\i c -> ((i, 2), Height $ ord c)) [1 ..] "abcryxxl"
            <> zipWith (\i c -> ((i, 3), Height $ ord c)) [1 ..] "accszzxk"
            <> zipWith (\i c -> ((i, 4), Height $ ord c)) [1 ..] "acctuvwj"
            <> zipWith (\i c -> ((i, 5), Height $ ord c)) [1 ..] "abdefghi"
    }

spec :: Spec
spec = do
  describe "Parsing" $ do
    it "handles the example input" $ do
      parseOnly parser exampleText `shouldBe` Right exampleInput

  describe "Part 1" $ do
    it "handles the example input" $ do
      findPath exampleInput `shouldBe` [(1, 2), (2, 2), (2, 3), (2, 4), (3, 4), (3, 5), (4, 5), (5, 5), (6, 5), (7, 5), (8, 5), (8, 4), (8, 3), (8, 2), (8, 1), (7, 1), (6, 1), (5, 1), (4, 1), (4, 2), (4, 3), (4, 4), (5, 4), (6, 4), (7, 4), (7, 3), (7, 2), (6, 2), (5, 2), (5, 3), (6, 3)]
      part1 exampleInput `shouldBe` 31

  describe "Part 2" $ do
    it "handles the example input" $ do
      part2 exampleInput `shouldBe` 29