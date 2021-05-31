module Main where

--------------------------------------------------------------------------------
import Track
import Track.Bezier
import Editor.Waypoint
import Editor.TrackState
import Input
import Game
import Editor
import Editor.Cache
import SF
import Vec
import Types
import Util

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Prelude hiding ((.), id)
import Control.Exception
import Text.Read
import Data.Maybe
import System.Environment
import Debug.Trace
--------------------------------------------------------------------------------

main :: IO ()
main = do
  editorInit <- readTrackSaveData
  runSF $ runMode $ editor editorInit game

readTrackSaveData :: IO (TrackSaveData, FilePath)
readTrackSaveData = do 
  args <- getArgs
  print args
  case args of
    [filename] -> do
      let filepath = "tracks/" ++ filename
      saveData <- read <$> readFile filepath
      return (saveData, filepath)
    _ -> return (TrackSaveData [] [], "/dev/null")
      
      

-- readTrack :: IO (Track, TrackState)
-- readTrack = do
--   let 
--     def = TrackSaveData [(zeroVec,100),(Vec 0 600,100)] []
-- 
--     h :: SomeException -> IO TrackSaveData
--     h _ = return def
-- 
--   saveData <- (fromMaybe def . readMaybe <$> readFile "track") `catch` h
-- 
--   return (fromSaveData saveData, trackStateFromSaveData saveData)
  
runSF :: (Input ~> Output) -> IO ()
runSF sf
  = playIO
    (InWindow "hsrace test" (1716,1397) (800,600))
    black
    120
    ([], emptyInput, (Output blank Nothing, sf))
    (\(_,_,(Output pic _,_)) -> return pic)
    (\e w -> return (registerEvent e w))
    (glossUpdate $ \dt input old -> do
        (output, new) <- updateSF dt input old
        processOutput output
        return (output, new))


processOutput :: Output -> IO ()
processOutput (Output _ (Just (FileOutput filename content)))
  = writeFile filename content
processOutput _ = return ()
