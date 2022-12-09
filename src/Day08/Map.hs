module Day08.Map where

import Data.List (maximum)
import Data.Map.Strict qualified as Map

type Input = [((Int, Int), Int)]
type Coord = (Int, Int)
type Grid = Map Coord

inputToMap :: Input -> Grid Int
inputToMap = Map.fromList

addCoord :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addCoord (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

computeVisibility :: Grid Int -> Grid Bool
computeVisibility forest =
  markDown . markUp . markLeft . markRight $ emptyVisMap
 where
  (minX, minY) = fst . fromMaybe (error "Forest is empty") $ Map.lookupMin forest
  (maxX, maxY) = fst . fromMaybe (error "Forest is empty") $ Map.lookupMax forest

  emptyVisMap = Map.fromList . map (,False) $ [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]]

  markDown visMap =
    foldr (markVisible (-1) (0, 1) . (,minY)) visMap [minX .. maxX]
  markUp visMap =
    foldr (markVisible (-1) (0, -1) . (,maxY)) visMap [minX .. maxX]
  markLeft visMap =
    foldr (markVisible (-1) (-1, 0) . (maxX,)) visMap [minY .. maxY]
  markRight visMap =
    foldr (markVisible (-1) (1, 0) . (minX,)) visMap [minY .. maxY]

  markVisible :: Int -> Coord -> Coord -> Grid Bool -> Grid Bool
  markVisible prevVal dir coord visMap =
    case Map.lookup coord forest of
      Nothing -> visMap
      Just treeVal ->
        if treeVal > prevVal
          then markVisible treeVal dir (addCoord coord dir) $ Map.insert coord True visMap
          else markVisible prevVal dir (addCoord coord dir) visMap

part1 :: Input -> Int
part1 = length . filter id . map snd . Map.toList . computeVisibility . inputToMap

scoreCoord :: Grid Int -> Coord -> Int
scoreCoord forest coord@(x, y) =
  scoreLeft * scoreRight * scoreDown * scoreUp
 where
  (minX, minY) = fst . fromMaybe (error "Forest is empty") $ Map.lookupMin forest
  (maxX, maxY) = fst . fromMaybe (error "Forest is empty") $ Map.lookupMax forest

  treeVal =
    fromMaybe (error $ "Forest is missing a tree at " <> show coord) $
      Map.lookup coord forest

  scoreLeft =
    viewDist $ reverse [(x', y) | x' <- [minX .. (x - 1)]]
  scoreRight =
    viewDist $ [(x', y) | x' <- [(x + 1) .. maxX]]
  scoreDown =
    viewDist $ reverse [(x, y') | y' <- [minY .. (y - 1)]]
  scoreUp =
    viewDist $ [(x, y') | y' <- [(y + 1) .. maxY]]

  viewDist =
    length . takeUntil (>= treeVal) . mapMaybe (`Map.lookup` forest)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs)
  | p x = [x]
  | otherwise = x : takeUntil p xs

part2 :: Input -> Int
part2 input =
  let forest = inputToMap input
   in maximum . map (scoreCoord forest . fst) . Map.toList $ forest
