module Main where

import Data.Text qualified as Text

import Day01 qualified
import Exercise (Exercise (..))
import Runner (runSolution)

exercises :: [Exercise]
exercises = [Day01.exercise]

main :: IO ()
main =
  forM_ exercises $ \(Exercise{..}) -> do
    putTextLn $ Text.intercalate " " exerciseName
    runSolution exerciseSolution