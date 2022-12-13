module Day13Spec where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Data.Attoparsec.Text (parseOnly)
import Day13
import NeatInterpolation (text)

exampleText :: Text
exampleText =
  [text|
  [1,1,3,1,1]
  [1,1,5,1,1]

  [[1],[2,3,4]]
  [[1],4]

  [9]
  [[8,7,6]]

  [[4,4],4,4]
  [[4,4],4,4,4]

  [7,7,7,7]
  [7,7,7]

  []
  [3]

  [[[]]]
  [[]]

  [1,[2,[3,[4,[5,6,7]]]],8,9]
  [1,[2,[3,[4,[5,6,0]]]],8,9]
  |]

exampleInput :: Input
exampleInput =
  [ (L [N 1, N 1, N 3, N 1, N 1], L [N 1, N 1, N 5, N 1, N 1])
  , (L [L [N 1], L [N 2, N 3, N 4]], L [L [N 1], N 4])
  , (L [N 9], L [L [N 8, N 7, N 6]])
  , (L [L [N 4, N 4], N 4, N 4], L [L [N 4, N 4], N 4, N 4, N 4])
  , (L [N 7, N 7, N 7, N 7], L [N 7, N 7, N 7])
  , (L [], L [N 3])
  , (L [L [L []]], L [L []])
  , (L [N 1, L [N 2, L [N 3, L [N 4, L [N 5, N 6, N 7]]]], N 8, N 9], L [N 1, L [N 2, L [N 3, L [N 4, L [N 5, N 6, N 0]]]], N 8, N 9])
  ]

spec :: Spec
spec = do
  describe "Parsing" $ do
    it "handles the example input" $ do
      parseOnly parser exampleText `shouldBe` Right exampleInput

  describe "Part 1" $ do
    it "handles the example input" $ do
      L [N 1, N 1, N 3, N 1, N 1] `shouldSatisfy` (<= L [N 1, N 1, N 5, N 1, N 1])
      part1 exampleInput `shouldBe` 13

  describe "Part 2" $ do
    it "handles the example input" $ do
      part2 exampleInput `shouldBe` 140
