module Day09 where

import Control.Monad.Writer (MonadWriter (tell), WriterT, execWriterT)
import Data.Attoparsec.Text (Parser, char, decimal, endOfInput, endOfLine, sepBy1, skipSpace)
import Data.Set qualified as Set
import Linear.V2 (V2 (..))

import Exercise (Exercise (..), Solution (..))

exercise :: Exercise
exercise =
  Exercise
    { exerciseNum = 9
    , exerciseParser = parser
    , exerciseSolutions =
        [ Solution "Part 1" part1
        , Solution "Part 2" part2
        ]
    }

data Instruction
  = R Int
  | L Int
  | U Int
  | D Int
  deriving stock (Show, Eq)

type Input = [Instruction]

parser :: Parser Input
parser =
  (instruction `sepBy1` endOfLine) <* skipSpace <* endOfInput
 where
  instruction =
    R <$> (char 'R' *> skipSpace *> decimal)
      <|> L <$> (char 'L' *> skipSpace *> decimal)
      <|> U <$> (char 'U' *> skipSpace *> decimal)
      <|> D <$> (char 'D' *> skipSpace *> decimal)

type Coord = V2 Int

type Rope = [Coord]

type Sim = WriterT (Set Coord) (State Rope)

runInstruction :: Instruction -> Sim ()
runInstruction (R n) = void . replicateM n . move $ V2 1 0
runInstruction (L n) = void . replicateM n . move $ V2 (-1) 0
runInstruction (U n) = void . replicateM n . move $ V2 0 1
runInstruction (D n) = void . replicateM n . move $ V2 0 (-1)

move :: Coord -> Sim ()
move dir = do
  rope <- get
  case rope of
    [] -> error "The rope is empty"
    (front : rest) -> do
      let rope' = pullRope (front + dir) rest
          end : _ = reverse rope'
      tell $ Set.singleton end
      put rope'

pullRope :: Coord -> Rope -> Rope
pullRope front back = do
  case back of
    -- Nothing to pull on.
    [] -> [front]
    -- Move the next knot
    (next : rest) ->
      let dp@(V2 dx dy) = front - next
      in
        if abs dx <= 1 && abs dy <= 1
          -- Abort if this isn't moving.
          then front : back
          else
            -- Move and propagate.
            front : pullRope (next + signum dp) rest

origin :: V2 Int
origin = V2 0 0

runSim :: Rope -> Input -> (Set Coord, Rope)
runSim rope =
  usingState rope . execWriterT . addStart . mapM runInstruction
 where
  addStart = (>> tell (Set.singleton origin))

part1 :: Input -> Int
part1 = Set.size . fst . runSim (fromList [origin, origin])

part2 :: Input -> Int
part2 = Set.size . fst . runSim (fromList $ replicate 10 origin)
