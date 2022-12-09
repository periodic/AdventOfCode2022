module Day08Spec where

import Test.Hspec (Spec, describe, it, shouldBe)
import NeatInterpolation (text)
import Data.Attoparsec.Text (parseOnly)

import Day08
import Day08.Map

exampleText :: Text
exampleText =
  [text|
  30373
  25512
  65332
  33549
  35390
  |]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "should handle the example" $ do
      case parseOnly parser exampleText of
        Right grid ->
          part1 grid `shouldBe` 21
        Left err ->
          error $ "Unable to parse example: " <> toText err

  describe "part 2 v1" $ do
    it "should handle the example" $ do
      case parseOnly parser exampleText of
        Right grid ->
          part2 grid `shouldBe` 8
        Left err ->
          error $ "Unable to parse example: " <> toText err
