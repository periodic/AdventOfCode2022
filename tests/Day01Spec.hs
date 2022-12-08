module Day01Spec where

import Data.Attoparsec.Text (parseOnly)
import Data.Text qualified as T
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (NonEmptyList (..), Positive (Positive), property)

import Day01 (parser)

render :: [[Int]] -> Text
render = T.intercalate "\n\n" . map (T.intercalate "\n" . map show)

fromNonEmpty :: NonEmptyList a -> [a]
fromNonEmpty (NonEmpty xs) = xs

fromPositive :: Positive a -> a
fromPositive (Positive n) = n

spec :: Spec
spec =
    describe "parser" $ do
        it "handles generated input" $
            property $ \(NonEmpty input) ->
                let buckets = map (map fromPositive . fromNonEmpty) input
                 in parseOnly parser (render buckets) == Right buckets
