module Day07Spec where

import Data.Attoparsec.Text (parseOnly)
import Data.Map.Strict qualified as Map
import NeatInterpolation (text)
import Test.Hspec (Spec, describe, it, shouldBe)

import Day07

exampleText :: Text
exampleText =
  [text|
  $ cd /
  $ ls
  dir a
  14848514 b.txt
  8504156 c.dat
  dir d
  $ cd a
  $ ls
  dir e
  29116 f
  2557 g
  62596 h.lst
  $ cd e
  $ ls
  584 i
  $ cd ..
  $ cd ..
  $ cd d
  $ ls
  4060174 j
  8033020 d.log
  5626152 d.ext
  7214296 k
  |]

exampleInput :: [Line]
exampleInput =
  [ ChangeDir "/"
  , ListDir
  , DirEntry "a"
  , FileEntry 14848514 "b.txt"
  , FileEntry 8504156 "c.dat"
  , DirEntry "d"
  , ChangeDir "a"
  , ListDir
  , DirEntry "e"
  , FileEntry 29116 "f"
  , FileEntry 2557 "g"
  , FileEntry 62596 "h.lst"
  , ChangeDir "e"
  , ListDir
  , FileEntry 584 "i"
  , ChangeDir ".."
  , ChangeDir ".."
  , ChangeDir "d"
  , ListDir
  , FileEntry 4060174 "j"
  , FileEntry 8033020 "d.log"
  , FileEntry 5626152 "d.ext"
  , FileEntry 7214296 "k"
  ]

exampleFileSystem :: FileSystem
exampleFileSystem =
  Map.fromList
    [
      ( "a"
      , Dir 94853 $
          Map.fromList
            [
              ( "e"
              , Dir 584 $
                  Map.fromList
                    [ ("i", File 584)
                    ]
              )
            , ("f", File 29116)
            , ("g", File 2557)
            , ("h.lst", File 62596)
            ]
      )
    , ("b.txt", File 14848514)
    , ("c.dat", File 8504156)
    ,
      ( "d"
      , Dir 24933642 $
          Map.fromList
            [ ("j", File 4060174)
            , ("d.log", File 8033020)
            , ("d.ext", File 5626152)
            , ("k", File 7214296)
            ]
      )
    ]

spec :: Spec
spec = do
  describe "parser" $ do
    it "parses the example" $ do
      parseOnly parser exampleText `shouldBe` Right exampleInput

  describe "toTree" $ do
    it "handles the example" $ do
      calculateSizes (toTree exampleInput) `shouldBe` exampleFileSystem

  describe "part 1" $ do
    it "handles the example" $ do
      part1 exampleInput `shouldBe` 95437

  describe "part 2" $ do
    it "handles the example" $ do
      part2 exampleInput `shouldBe` 24933642
