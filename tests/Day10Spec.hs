module Day10Spec where

import NeatInterpolation (text)
import Test.Hspec (Spec, describe, it, shouldBe)

import Data.Attoparsec.Text (parseOnly)
import Day10


spec :: Spec
spec = do
  describe "Parsing" $ do
    it "should parse the example" $ do
      parseOnly parser exampleText `shouldBe` Right exampleInput

  describe "Part 1" $ do
    it "should handle the example" $ do
      part1 exampleInput `shouldBe` 13140

  describe "Part 2" $ do
    it "should handle the example" $ do
      show (part2 exampleInput) `shouldBe` exampleOutput

exampleText :: Text
exampleText =
  [text|
  addx 15
  addx -11
  addx 6
  addx -3
  addx 5
  addx -1
  addx -8
  addx 13
  addx 4
  noop
  addx -1
  addx 5
  addx -1
  addx 5
  addx -1
  addx 5
  addx -1
  addx 5
  addx -1
  addx -35
  addx 1
  addx 24
  addx -19
  addx 1
  addx 16
  addx -11
  noop
  noop
  addx 21
  addx -15
  noop
  noop
  addx -3
  addx 9
  addx 1
  addx -3
  addx 8
  addx 1
  addx 5
  noop
  noop
  noop
  noop
  noop
  addx -36
  noop
  addx 1
  addx 7
  noop
  noop
  noop
  addx 2
  addx 6
  noop
  noop
  noop
  noop
  noop
  addx 1
  noop
  noop
  addx 7
  addx 1
  noop
  addx -13
  addx 13
  addx 7
  noop
  addx 1
  addx -33
  noop
  noop
  noop
  addx 2
  noop
  noop
  noop
  addx 8
  noop
  addx -1
  addx 2
  addx 1
  noop
  addx 17
  addx -9
  addx 1
  addx 1
  addx -3
  addx 11
  noop
  noop
  addx 1
  noop
  addx 1
  noop
  noop
  addx -13
  addx -19
  addx 1
  addx 3
  addx 26
  addx -30
  addx 12
  addx -1
  addx 3
  addx 1
  noop
  noop
  noop
  addx -9
  addx 18
  addx 1
  addx 2
  noop
  noop
  addx 9
  noop
  noop
  noop
  addx -1
  addx 2
  addx -37
  addx 1
  addx 3
  noop
  addx 15
  addx -21
  addx 22
  addx -6
  addx 1
  noop
  addx 2
  addx 1
  noop
  addx -10
  noop
  noop
  addx 20
  addx 1
  addx 2
  addx 2
  addx -6
  addx -11
  noop
  noop
  noop
  |]

exampleInput :: Program
exampleInput =
  [ Add 15
  , Add (-11)
  , Add (6)
  , Add (-3)
  , Add (5)
  , Add (-1)
  , Add (-8)
  , Add (13)
  , Add (4)
  , Noop
  , Add (-1)
  , Add (5)
  , Add (-1)
  , Add (5)
  , Add (-1)
  , Add (5)
  , Add (-1)
  , Add (5)
  , Add (-1)
  , Add (-35)
  , Add (1)
  , Add (24)
  , Add (-19)
  , Add (1)
  , Add (16)
  , Add (-11)
  , Noop
  , Noop
  , Add (21)
  , Add (-15)
  , Noop
  , Noop
  , Add (-3)
  , Add (9)
  , Add (1)
  , Add (-3)
  , Add (8)
  , Add (1)
  , Add (5)
  , Noop
  , Noop
  , Noop
  , Noop
  , Noop
  , Add (-36)
  , Noop
  , Add (1)
  , Add (7)
  , Noop
  , Noop
  , Noop
  , Add (2)
  , Add (6)
  , Noop
  , Noop
  , Noop
  , Noop
  , Noop
  , Add (1)
  , Noop
  , Noop
  , Add (7)
  , Add (1)
  , Noop
  , Add (-13)
  , Add (13)
  , Add (7)
  , Noop
  , Add (1)
  , Add (-33)
  , Noop
  , Noop
  , Noop
  , Add (2)
  , Noop
  , Noop
  , Noop
  , Add (8)
  , Noop
  , Add (-1)
  , Add (2)
  , Add (1)
  , Noop
  , Add (17)
  , Add (-9)
  , Add (1)
  , Add (1)
  , Add (-3)
  , Add (11)
  , Noop
  , Noop
  , Add (1)
  , Noop
  , Add (1)
  , Noop
  , Noop
  , Add (-13)
  , Add (-19)
  , Add (1)
  , Add (3)
  , Add (26)
  , Add (-30)
  , Add (12)
  , Add (-1)
  , Add (3)
  , Add (1)
  , Noop
  , Noop
  , Noop
  , Add (-9)
  , Add (18)
  , Add (1)
  , Add (2)
  , Noop
  , Noop
  , Add (9)
  , Noop
  , Noop
  , Noop
  , Add (-1)
  , Add (2)
  , Add (-37)
  , Add (1)
  , Add (3)
  , Noop
  , Add (15)
  , Add (-21)
  , Add (22)
  , Add (-6)
  , Add (1)
  , Noop
  , Add (2)
  , Add (1)
  , Noop
  , Add (-10)
  , Noop
  , Noop
  , Add (20)
  , Add (1)
  , Add (2)
  , Add (2)
  , Add (-6)
  , Add (-11)
  , Noop
  , Noop
  , Noop
  ]

exampleOutput :: Text
exampleOutput =
  [text|
  ##..##..##..##..##..##..##..##..##..##..
  ###...###...###...###...###...###...###.
  ####....####....####....####....####....
  #####.....#####.....#####.....#####.....
  ######......######......######......####
  #######.......#######.......#######.....
  |]