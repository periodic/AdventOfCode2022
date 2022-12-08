module Main where

import Criterion.Types (Report (..), SampleAnalysis (..))
import Data.Text qualified as Text
import Options.Applicative (
  Parser,
  auto,
  execParser,
  help,
  info,
  long,
  metavar,
  option,
  short,
  str,
  strArgument,
  switch,
 )
import Statistics.Types (Estimate (..))
import Text.Printf (printf)

import Day01 qualified
import Day02 qualified
import Day03 qualified
import Day04 qualified
import Day05 qualified
import Day06 qualified
import Day07 qualified
import Exercise (Exercise (..), Solution (..))
import Runner (ParseResult (ParseResult), SolutionResult (..), Timing (..), readInput, runSolution)

exercises :: [Exercise]
exercises =
  [ Day01.exercise
  , Day02.exercise
  , Day03.exercise
  , Day04.exercise
  , Day05.exercise
  , Day06.exercise
  , Day07.exercise
  ]

data Filter = Filter
  { exerciseFilter :: Int
  , solutionFilter :: Maybe Text
  }
  deriving (Show)

data Arguments = Arguments
  { inputFile :: FilePath
  , filters :: Maybe Filter
  , benchmarking :: Bool
  }
  deriving (Show)

argsParser :: Parser Arguments
argsParser =
  Arguments
    <$> inputFile
    <*> optional solutionFilter
    <*> benchmark
 where
  inputFile = strArgument (metavar "INPUT_FILE")
  solutionFilter =
    Filter
      <$> option auto (long "exercise" <> short 'e' <> help "Exercise number")
      <*> optional (option str (long "solution" <> short 's' <> help "Solutions prefix"))
  benchmark = switch (long "benchmark" <> short 'b' <> help "Enable detailed benchmarking")

main :: IO ()
main = do
  Arguments{..} <- execParser (info argsParser mempty)

  let exercises' =
        case filters of
          Nothing -> exercises
          Just (Filter{..}) -> filter ((== exerciseFilter) . exerciseNum) exercises

  forM_ exercises' $ \(Exercise{..}) -> do
    printExercise exerciseNum
    ParseResult input parseTiming <- readInput benchmarking inputFile exerciseParser
    printParseResult parseTiming

    let solutions' =
          case filters >>= solutionFilter of
            Nothing -> exerciseSolutions
            Just prefix -> filter ((prefix `Text.isPrefixOf`) . solutionName) exerciseSolutions

    forM_ solutions' $ \solution -> do
      solutionResult <- runSolution benchmarking input solution
      printSolutionResult (solutionName solution) solutionResult

printExercise :: Int -> IO ()
printExercise n =
  putStrLn $ replicate 10 '=' <> "Day " <> show n <> replicate 10 '='

printParseResult :: Timing -> IO ()
printParseResult timing =
  putTextLn $ "Parsing\n  " <> showTiming timing

printSolutionResult :: Text -> SolutionResult -> IO ()
printSolutionResult solutionName SolutionResult{..} = do
  putTextLn $ solutionName <> "\n  " <> solutionOutput <> "\n  " <> showTiming solutionTiming

showTiming :: Timing -> Text
showTiming (SimpleTiming seconds) = toText @String $ printf "time = %0.1fμs" (seconds * 1000)
showTiming (DetailedTiming report) =
  let Report{reportAnalysis = SampleAnalysis{anMean, anStdDev}} = report
   in toText @String $ printf "mean = %0.1fμs, stdDev = %0.1fμs" (estPoint anMean * 1000000) (estPoint anStdDev * 1000000)
