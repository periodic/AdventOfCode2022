
module Exercise (Exercise (..), Solution (..)) where

import qualified Data.Attoparsec.Text as Attoparsec

data Solution a = Solution
  { solutionParser :: Attoparsec.Parser a
  , solutionPart1 :: a -> Int
  , solutionPart2 :: a -> Int
  }

data Exercise where
  Exercise :: { exerciseName :: [Text]
    , exerciseSolution :: Solution a
    } -> Exercise
