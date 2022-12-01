module Utils.Constraints (solve) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

solve :: (Ord a, Ord b) => Map a (Set b) -> Maybe (Map a b)
solve =
  fmap M.fromList . solveList . M.toList

solveList :: (Ord a, Ord b) => [(a, Set b)] -> Maybe [(a, b)]
solveList options =
  case options of
    [] -> Just mempty
    ((key, values) : remainingOptions) -> getFirst . mconcat . map First $ do
      value <- S.toList values
      let filteredOptions = L.map (\(k, opts) -> (k, S.delete value opts)) remainingOptions
      let subSolution = solveList filteredOptions
      return $ ((key, value) :) <$> subSolution