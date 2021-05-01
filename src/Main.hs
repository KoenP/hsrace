module Main where

--------------------------------------------------------------------------------
import Track
import Track.Bezier
import Editor.TrackState
import Input
import Game
import Editor
import Editor.Cache
import SF
import Vec
import Types

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Prelude hiding ((.), id)
import Control.Exception
import Text.Read
import Data.Maybe
--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Read track.
  -- (track, trackState) <- readTrack
  -- let sf = 
  -- let sf = runMode (game (editor trackState game) track)
  -- runSF sf
  runSF $ runMode $ editor emptyCache game

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
    (\(_,_,(out@(Output pic _),_)) -> processOutput out >> return pic)
    (\e w -> return (registerEvent e w))
    (glossUpdate updateSF)

processOutput :: Output -> IO ()
processOutput (Output _ (Just (FileOutput filename content)))
  = writeFile filename content
processOutput _ = return ()
