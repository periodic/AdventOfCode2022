module Day11Spec where

import Data.IntMap qualified as IntMap
import Data.Sequence qualified as Seq
import NeatInterpolation (text)
import Test.Hspec (Spec, describe, it, shouldBe)

import Data.Attoparsec.Text (parseOnly)
import Day11

exampleText =
  [text|
  Monkey 0:
    Starting items: 79, 98
    Operation: new = old * 19
    Test: divisible by 23
      If true: throw to monkey 2
      If false: throw to monkey 3

  Monkey 1:
    Starting items: 54, 65, 75, 74
    Operation: new = old + 6
    Test: divisible by 19
      If true: throw to monkey 2
      If false: throw to monkey 0

  Monkey 2:
    Starting items: 79, 60, 97
    Operation: new = old * old
    Test: divisible by 13
      If true: throw to monkey 1
      If false: throw to monkey 3

  Monkey 3:
    Starting items: 74
    Operation: new = old + 3
    Test: divisible by 17
      If true: throw to monkey 0
      If false: throw to monkey 1
  |]

exampleInput :: IntMap Monkey
exampleInput =
  IntMap.fromList . zip [0 ..] $
    [ Monkey 0 (Seq.fromList [79, 98]) (Mul 19) (Test 23 2 3)
    , Monkey 0 (Seq.fromList [54, 65, 75, 74]) (Add 6) (Test 19 2 0)
    , Monkey 0 (Seq.fromList [79, 60, 97]) Square (Test 13 1 3)
    , Monkey 0 (Seq.fromList [74]) (Add 3) (Test 17 0 1)
    ]

spec :: Spec
spec = do
  describe "Parsing" $ do
    it "should handle the example" $ do
      parseOnly parser exampleText `shouldBe` Right exampleInput

  describe "Part 1" $ do
    it "should handle the example" $ do
      part1 exampleInput `shouldBe` 10605

  describe "Part 2" $ do
    it "should handle the example" $ do
      part2 exampleInput `shouldBe` 2713310158
