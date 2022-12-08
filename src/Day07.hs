module Day07 where

import Data.Attoparsec.Text (Parser, endOfInput, skipSpace, sepBy, endOfLine, string, takeTill, decimal)
import Data.Char (isSpace)
import Data.Map.Strict qualified as Map

import Exercise (Exercise(..), Solution (..))
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ((<|))
import Data.List (minimum)


exercise :: Exercise
exercise = Exercise {
  exerciseNum = 7,
  exerciseParser = parser,
  exerciseSolutions =
    [ Solution "Part 1" part1
    , Solution "Part 2" part2
    ]
}

data Line
  = ChangeDir Text
  | ListDir
  | DirEntry Text
  | FileEntry Int Text
  deriving (Show, Eq)

type Input = [Line]

parser :: Parser Input
parser = 
  (line `sepBy` endOfLine) <* skipSpace <* endOfInput
  where
    line = changeDir <|> listDir <|> directoryEntry <|> fileEntry
    changeDir = ChangeDir <$> (string "$ cd " *> takeTill isSpace)
    listDir = ListDir <$ string "$ ls"
    directoryEntry = DirEntry <$> (string "dir " *> takeTill isSpace)
    fileEntry = FileEntry <$> decimal <* skipSpace <*> takeTill isSpace

data Entry
  = Dir Int (Map Text Entry)
  | File Int
  deriving (Show, Eq)

type FileSystem = Map Text Entry

type FileSystemBuilder = State (NonEmpty (Text, Map Text Entry))

toTree :: Input -> FileSystem
toTree =
  snd . NE.head . executingState (NE.singleton ("/", Map.empty)) . (>> handleCommand (ChangeDir "/")) . handleCommands
  where
    handleCommands :: Input -> FileSystemBuilder ()
    handleCommands [] = pass
    handleCommands (x:xs) = do
      handleCommand x
      handleCommands xs
    handleCommand :: Line -> FileSystemBuilder ()
    handleCommand (ChangeDir "..") =
      unwindStack
    handleCommand (ChangeDir "/") = do
      ((currName, _) :| _) <- get
      if currName == "/"
        then pass
        else unwindStack >> handleCommand (ChangeDir "/")
    handleCommand (ChangeDir name) = do
      modify $ \stack@((_, currContents):|_) -> 
        case Map.lookup name currContents of
          Just (Dir _ newContents) ->
            (name, newContents) <| stack
          Just (File _) ->
            error $ "Attempted to recurse into a file: " <> name
          Nothing -> 
            error $ "Attempted to recurse into non-existent directory: " <> name
    handleCommand ListDir =
      pass
    handleCommand (DirEntry name) =
      insertCurrent name (Dir 0 Map.empty)
    handleCommand (FileEntry size name) =
      insertCurrent name (File size)

    unwindStack :: FileSystemBuilder ()
    unwindStack =
      modify $ \case
        (currName, currContents):|(parentName, parentContents):rest ->
          let parent' = Map.insert currName (Dir 0 currContents) parentContents
          in (parentName, parent') :| rest
        _ -> error "Attempted to go up from the root directory"

    insertCurrent :: Text -> Entry -> FileSystemBuilder ()
    insertCurrent name entry = do
      modify $ \((currName, currContents) :| rest) ->
        (currName, Map.insert name entry currContents) :| rest

calculateSizes :: FileSystem -> FileSystem
calculateSizes = Map.map calculateSize
  where
    calculateSize file@(File _) = file
    calculateSize (Dir _ entries) =
      let sized = Map.map calculateSize entries
          totalSize = sum $ Map.map toSize sized
      in Dir totalSize sized

toSize :: Entry -> Int
toSize (File size) = size
toSize (Dir size _) = size

fsToList :: FileSystem -> [Entry]
fsToList fs = do
  entry <- map snd . Map.toList $ fs
  case entry of
    file@(File{}) -> pure file
    dir@(Dir _ contents) -> dir : fsToList contents

isDir :: Entry -> Bool
isDir (File{}) = False
isDir (Dir{}) = True

part1 :: Input -> Int
part1 = sum . filter (<= 100000) . map toSize . filter isDir . fsToList . calculateSizes . toTree 

part2 :: Input -> Int
part2 input = 
  let filesystem = calculateSizes . toTree $ input
      totalSize = sum $ Map.map toSize filesystem
      requiredSize = totalSize - (70000000 - 30000000)
  in minimum . filter (>= requiredSize) . map toSize . filter isDir . fsToList $ filesystem
