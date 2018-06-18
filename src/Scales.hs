module Scales where

import Data.Char (toUpper)
import Data.List (elemIndex, findIndices, intercalate)
import Data.List.Split (splitOn)
import System.IO (FilePath)

data Note = C | Db | D | Eb | E | F | Gb | G | Ab | A | Bb | B deriving (Enum, Eq, Show)
type Key = Note
type Step = Int
--  A scale has a name and a list of notes
data Scale = Scale String [Note] deriving Show
data KeyedScale = KeyedScale String Key [Note]
data ScaleSteps = ScaleSteps String [Step] deriving Show

instance Show KeyedScale where
  show (KeyedScale name key notes) = prettyKey ++ " " ++ name ++ ": " ++ (unwords prettyNotes)
    where sharpened = map toSharp notes
          shouldSharpen = useSharp notes
          prettyKey = if shouldSharpen then toSharp key else show key
          prettyNotes = if shouldSharpen then sharpened else map show notes

toSharp :: Note -> String
toSharp Db = "C#"
toSharp Eb = "D#"
toSharp Gb = "F#"
toSharp Ab = "G#"
toSharp Bb = "A#"
toSharp note = show note

useSharp :: [Note] -> Bool
useSharp notes = length (findIndices (== 'A') notesStr) > 1
                 || length (findIndices (== 'B') notesStr) > 1
                 || length (findIndices (== 'C') notesStr) > 1
                 || length (findIndices (== 'D') notesStr) > 1
                 || length (findIndices (== 'E') notesStr) > 1
                 || length (findIndices (== 'F') notesStr) > 1
                 || length (findIndices (== 'G') notesStr) > 1
  where notesStr = unwords $ map show notes

sharpen :: Note -> Note
sharpen C  = Db
sharpen Db = D
sharpen D  = Eb
sharpen Eb = E
sharpen E  = F
sharpen F  = Gb
sharpen Gb = G
sharpen G  = Ab
sharpen Ab = A
sharpen A  = Bb
sharpen Bb = B
sharpen B  = C

flatten :: Note -> Note
flatten Db = C
flatten D  = Db
flatten Eb = D
flatten E  = Eb
flatten F  = E
flatten Gb = F
flatten G  = Gb
flatten Ab = G
flatten A  = Ab
flatten Bb = A
flatten B  = Bb
flatten C = B

readNote :: String -> Either String Note
readNote str = case (map toUpper str) of
                 "C"  -> Right C
                 "C#" -> Right Db
                 "DB" -> Right Db
                 "D"  -> Right D
                 "D#" -> Right Eb
                 "EB" -> Right Eb
                 "E"  -> Right E
                 "F"  -> Right F
                 "F#" -> Right Gb
                 "GB" -> Right Gb
                 "G"  -> Right G
                 "G#" -> Right Ab
                 "AB" -> Right Ab
                 "A"  -> Right A
                 "A#" -> Right Bb
                 "BB" -> Right Bb
                 "B"  -> Right B
                 str  -> Left ("Invalid note: " ++ str)

notes :: [Note]
notes = [C .. B]

notesInScale :: [Note] -> KeyedScale -> Bool
notesInScale notes (KeyedScale _ _ scale) = and $ map (`elem` scale) notes

stepsToIntervalsFromRoot :: [Step] -> [Int]
stepsToIntervalsFromRoot steps = reverse $ drop 1 $ scanr (+) 0 $ reverse steps

deserializeScaleSteps :: String -> ScaleSteps
deserializeScaleSteps str = ScaleSteps name notes
  where name  = last parts
        notes = map read (init parts)
        parts = splitOn "|" str

loadScaleStepsFromFile :: FilePath -> IO [ScaleSteps]
loadScaleStepsFromFile filename = do
  rawData <- readFile filename
  return (map deserializeScaleSteps (lines rawData))

serializeScaleSteps :: ScaleSteps -> String
serializeScaleSteps (ScaleSteps name notes) = intercalate "|" ((map show notes) ++ [name])

saveScaleStepsToFile :: FilePath -> [ScaleSteps] -> IO ()
saveScaleStepsToFile filename scales = writeFile filename (serialize scales)
  where serialize scaleSteps = intercalate "\n" $ map serializeScaleSteps scaleSteps

genScale :: ScaleSteps -> Key -> KeyedScale
genScale (ScaleSteps name steps) key = KeyedScale name key (map (shiftedScale !!) intervalsFromRoot)
  where shiftedScale = drop offset $ cycle notes
        intervalsFromRoot = stepsToIntervalsFromRoot steps
        offset = case (elemIndex key notes) of
          Nothing    -> 0
          Just index -> index

stringToNotes :: String -> Either String [Note]
stringToNotes str = traverse readNote (words str)

findScales :: [Note] -> [KeyedScale] -> [KeyedScale]
findScales notes scales = filter (notesInScale notes) scales
