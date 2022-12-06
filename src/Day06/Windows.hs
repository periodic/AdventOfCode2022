module Day06.Windows where

import qualified Data.Sequence as Seq
import Data.List (nub)

findUniqueWindow :: Eq a => Int -> [a] -> Int
findUniqueWindow n =
  maybe (error "No subsequence found") fst . find ((== n) . length . nub . toList . snd) . zip [n..] . windows . fromList
  where 
    windows :: Seq a -> [Seq a]
    windows xs
      | length xs < n = [xs]
      | otherwise = Seq.take n xs : windows (Seq.drop 1 xs)

part1 :: String -> Int
part1 = findUniqueWindow 4

part2 :: String -> Int
part2 = findUniqueWindow 14