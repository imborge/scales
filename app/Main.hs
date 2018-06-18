module Main where

import Control.Exception (handle, displayException)
import Control.Monad (forever)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.IO.Error (IOError, isEOFError)

import Scales

readHandler :: IOError -> IO ()
readHandler ex
  | isEOFError ex = exitFailure
  | otherwise = do
      putStrLn $ displayException ex
      exitFailure

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  handle readHandler $ do
    scaleSteps <- loadScaleStepsFromFile "data/scales.data"
    let scales = pure genScale <*> scaleSteps <*> notes
    putStrLn "Scales by Børge André Jensen <borgeajensen@gmail.com>"
    putStrLn "Enter a set of notes to find a scale e.g. Gb A C#, or 'q' to quit"
    forever $ do
      putStr "> "
      input <- getLine
      case input of
        "q" -> exitSuccess
        _   -> do
          putStrLn "Scales containing your notes:"
          traverse (\x ->
                      case x of
                        Left error -> putStrLn error
                        Right scale -> putStrLn $ show scale) (sequence matchingScales)
            where matchingScales = pure findScales <*> stringToNotes input <*> pure scales
