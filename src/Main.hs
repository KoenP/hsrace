module Main where

--------------------------------------------------------------------------------
import Track
import Track.Road
import Input
import Game
import Editor
import SF
import Types

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Prelude hiding ((.), id)
--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Read track.
  saveData <- read <$> readFile "track"
  let road = fromWaypoints saveData
  
  let sf = runMode (game (editor game) (Track road undefined))
  runSF sf
  
runSF :: (Input ~> Output) -> IO ()
runSF sf
  = playIO
    (InWindow "hsrace test" (1716,1397) (800,600))
    black
    60
    ([], emptyInput, (Output blank Nothing, sf))
    (\(_,_,(out@(Output pic _),_)) -> processOutput out >> return pic)
    (\e w -> return (registerEvent e w))
    (glossUpdate updateSF)

processOutput :: Output -> IO ()
processOutput (Output _ (Just (FileOutput filename content)))
  = writeFile filename content
processOutput _ = return ()
