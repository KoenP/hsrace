module Main where

--------------------------------------------------------------------------------

import Track
import RenderTrack
import Vec
import Angle
import Input
import Game
import Editor
import State
import SF

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Data.Function hiding ((.))
import Data.List
import Data.Coerce
import Data.Array
import Control.Lens
import Debug.Trace
import Prelude hiding ((.), id)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Read track.
  saveData <- read <$> readFile "track"
  -- let editorState0 = initializeEditorState saveData
  -- let (Layout track _)       = fromSaveData saveData
  let track = fromWaypoints saveData
  
  -- keymap <- constructKeyMap "keybindings.cfg"

  let game' mode track = game mode track (mkCollisionGrid 50 (map _tsShape track))
  let sf = runMode (game' (editor game') track)
  -- let sf = runMode (fix (game' . editor))
  runSF sf
  
  -- runSF editor
  -- runSF (game )
  -- playIO
  --   (InWindow "hsrace test" (1716,1397) (800,600))
  --   black
  --   60
  --   ([], emptyInput, Left (initialGameState layout, editorState0))
  --   (return . render)
  --   (\e w -> return (registerEvent e w))
  --   (glossUpdate update)

-- game :: m -> m
-- editor :: m -> m
-- game (editor (game (editor ....)))
-- game (fix ())

type ProgramState = Either (GameState, EditorState) EditorState

runSF :: (Input ~> Picture) -> IO ()
runSF sf
  = playIO
    (InWindow "hsrace test" (1716,1397) (800,600))
    black
    60
    ([], emptyInput, (blank, sf))
    (\(_,_,(pic,_)) -> return pic)
    (\e w -> return (registerEvent e w))
    (glossUpdate updateSF)

-- prog :: Input ~> Picture
-- prog = 

-- switchMode :: ProgramState -> ProgramState
-- switchMode (Left  (_,es))
--   = Right es
-- switchMode (Right es    )
--   = Left (initialGameState (editorStateExtractLayout es) , es)
-- 
-- update :: Double -> Input -> ProgramState -> IO ProgramState
-- update dt input state | keyTriggered Mode input = return (switchMode state)
-- update dt input (Left  (gs,es)) = return (Left (updateWorld dt input gs, es))
-- update dt input (Right es     ) =
--   let
--     saveToFile = editorStateSaveToFile es
--     waypoints  = editorStateExtractWaypoints es
--     es'        = updateEditor dt input es
--   in
--     if   saveToFile
--     then writeFile "track" (show waypoints) >> return (Right es')
--     else return (Right es')

-- update :: Double -> Input -> ProgramState -> IO ProgramState
-- update dt input (Left gameState   ) = return (updateWorld dt input gameState)
-- update dt input (Right editorState)
--   | Right editorState' <- updateEditor dt input editorState
--   = let saveToFile         = view es_saveToFile editorState'
--         waypoints          = revRev $ view (es_trackState.ts_waypointsR) editorState'
--     in if saveToFile
--        then writeFile "track" (show waypoints)
--             >> return (Right $ set es_saveToFile False editorState')
--        else return (Right editorState')
-- update dt input (Right editorState) = return (updateEditor dt input editorState)
--   -- TODO bad structure

-- render :: GlossState ProgramState -> Picture
-- render (_, _, Left  (gs,_)) = renderGameState gs
-- render (_, _, Right es)     = uncurry renderEditor es
