module Day12 where

import Data.Attoparsec.Text (Parser, letter)

import Data.Map.Strict qualified as Map
import Data.PQueue.Min qualified as Queue
import Exercise (Exercise (..), Solution (..))
import Utils.ParserHelpers (parseGrid)
import Data.List (minimum)

exercise :: Exercise
exercise =
  Exercise
    { exerciseNum = 12
    , exerciseParser = parser
    , exerciseSolutions =
        [ Solution "Part 1" part1
        , Solution "Part 2" part2
        ]
    }

type Coord = (Int, Int)

newtype Height = Height Int
  deriving (Show, Eq, Ord, Enum, Num, Integral, Real)

data Input = Input
  { grid :: Map Coord Height
  , start :: Coord
  , end :: Coord
  }
  deriving (Show, Eq)

parser :: Parser Input
parser =
  parseGrid (some letter) >>= processValues
 where
  -- Just using ord because it doesn't seem to care which
  charToHeight = Height . ord

  processValues :: [(Coord, Char)] -> Parser Input
  processValues rawData =
    let (s, rawData') = findAndReplace 'S' 'a' rawData
        (e, rawData'') = findAndReplace 'E' 'z' rawData'
        grid = Map.map charToHeight . Map.fromList $ rawData''
     in case (s, e) of
          (Just start, Just end) -> pure $ Input{grid, start, end}
          (Nothing, _) -> fail "Start is missing"
          (_, Nothing) -> fail "Start is missing"

  findAndReplace :: Char -> Char -> [(Coord, Char)] -> (Maybe Coord, [(Coord, Char)])
  findAndReplace _ _ [] = (Nothing, [])
  findAndReplace target replacement ((currCoord, currVal) : rest)
    | target == currVal = (Just currCoord, (currCoord, replacement) : rest)
    | otherwise = ((currCoord, currVal) :) <$> findAndReplace target replacement rest


type Steps = MaybeT (State (Map Coord Int, Queue.MinQueue Coord))

computeStepsTo :: Coord -> Map Coord Height -> Map Coord Int
computeStepsTo end grid =
  -- flood (Map.singleton end 0) (Queue.singleton end)
  fst . executingState (Map.singleton end 0, Queue.singleton end). runMaybeT $ floodSt
  where
    getDists = fst <$> get

    addToQueue c =
      modify . second $ Queue.insert c

    setDist c n =
      modify . first $ Map.insert c n

    getNext = do
      (dists, queue) <- get
      (c, queue') <- hoistMaybe $ Queue.minView queue
      put (dists, queue')
      pure c

    floodSt = do
      curr@(x,y) <- getNext
      let adjacent = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
      forM_ adjacent $ \prevStep -> processCoord curr prevStep <|> pass
      floodSt

    processCoord currStep nextStep = do
      currVal <- hoistMaybe $ Map.lookup currStep grid
      nextVal <- hoistMaybe $ Map.lookup nextStep grid
      guard $ currVal <= nextVal + 1 -- Can make the step, note names are reversed because we are going backwards

      dists <- getDists
      currSteps <- hoistMaybe $ Map.lookup currStep dists
      case Map.lookup nextStep dists of
        Nothing -> do
          setDist nextStep (currSteps + 1)
          addToQueue nextStep
        Just steps -> do
          guard $ steps > currSteps + 1
          setDist nextStep (currSteps + 1)
          addToQueue nextStep

part1 :: Input -> Int
part1 Input{..} = 
  fromMaybe (error $ "Start is not in the grid? " <> show start) . Map.lookup start $ computeStepsTo end grid

part2 :: Input -> Int
part2 Input{..} = 
  let stepMap = computeStepsTo end grid
      starts = Map.keys . Map.filter (== Height (ord 'a')) $ grid
      steps = mapMaybe (`Map.lookup` stepMap) starts
  in minimum steps
