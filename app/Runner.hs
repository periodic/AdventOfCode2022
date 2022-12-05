module Runner (SolutionResult (..), ParseResult (..), Timing (..), runSolution, readInput) where

import Criterion (benchmarkWith', whnfAppIO)
import Criterion.Main.Options (defaultConfig)
import Criterion.Types (Config (..), Report, Verbosity (..))
import Data.Attoparsec.Text (Parser, parseOnly)
import Data.Text.IO (readFile)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

import Exercise (Solution (..))

data Timing
  = SimpleTiming Double -- Seconds
  | DetailedTiming Report

data SolutionResult = SolutionResult
  { solutionOutput :: Text
  , solutionTiming :: Timing
  }

data ParseResult a = ParseResult
  { parseOutput :: a
  , parseTiming :: Timing
  }

runSolution :: Bool -> a -> Solution a -> IO SolutionResult
runSolution detailed input Solution{..} = do
  uncurry SolutionResult <$> time detailed (pure . show . solutionExec) input

readInput :: Bool -> FilePath -> Parser a -> IO (ParseResult a)
readInput detailed path parser = do
  contents <- Data.Text.IO.readFile path
  uncurry ParseResult <$> time detailed (doParsing parser) contents

doParsing :: Parser a -> Text -> IO a
doParsing parser contents =
  case parseOnly parser contents of
    Left err -> do
      printf "Failed to parse input: %s\n" err
      exitFailure
    Right input ->
      return input

time :: Bool -> (a -> IO b) -> a -> IO (b, Timing)
time True work input = do
  timing <- benchmarkWith' (defaultConfig{verbosity = Quiet}) $ whnfAppIO work input
  output <- work input
  pure (output, DetailedTiming timing)
time False work input = do
  start <- getCPUTime
  result <- work input
  end <- result `seq` getCPUTime
  let diff = fromIntegral (end - start) / (10 ^ 9) :: Double
  pure (result, SimpleTiming diff)
