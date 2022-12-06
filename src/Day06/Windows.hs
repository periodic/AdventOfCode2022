module Day06.Windows where

import Data.List (nub)
import Data.Text qualified as T

findUniqueWindow :: Int -> Text -> Int
findUniqueWindow n =
  maybe (error "No subsequence found") fst . find ((== n) . length . nub . toString . snd) . zip [n..] . windows
  where 
    windows :: Text -> [Text]
    windows xs
      | T.length xs < n = [xs]
      | otherwise = T.take n xs : windows (T.drop 1 xs)

part1 :: Text -> Int
part1 = findUniqueWindow 4

part2 :: Text -> Int
part2 = findUniqueWindow 14