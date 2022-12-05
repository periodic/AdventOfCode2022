module Exercise (Exercise (..), Solution (..)) where

import Data.Attoparsec.Text (Parser)

data Solution a where
  Solution ::
    Show b =>
    { solutionName :: Text
    , solutionExec :: a -> b
    } ->
    Solution a

data Exercise where
  Exercise ::
    { exerciseNum :: Int
    , exerciseParser :: Parser a
    , exerciseSolutions :: [Solution a]
    } ->
    Exercise
