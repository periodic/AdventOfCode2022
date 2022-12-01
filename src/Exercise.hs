module Exercise (Exercise (..), Solution (..)) where

import Data.Attoparsec.Text ( Parser )

data Solution a = Solution
  { solutionName :: Text
  , solutionExec :: a -> Int
  }

data Exercise where
  Exercise ::
    { exerciseNum :: Int
    , exerciseParser :: Parser a
    , exerciseSolutions :: [Solution a]
    } ->
    Exercise
