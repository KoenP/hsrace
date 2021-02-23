module Editor where

--------------------------------------------------------------------------------
import Vec
import Input
import Track
import State

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Control.Lens
import Data.List
import qualified Data.Set as Set
import Data.Set (Set)
--------------------------------------------------------------------------------


data EditorInput = EditorInput
  { _input_pointer :: Vec Window
  , _input_dir :: Vec World
  }
makeLenses 'EditorInput

updateEditor :: Double -> Input -> EditorState -> ProgramState
updateEditor _ (Input _ keysTriggered _) _
  | Mode `Set.member` keysTriggered
  = Left emptyGameState
updateEditor _ (Input keysDown _ mouseMovement) (EditorState viewPort0 curTrack0 pointerPos0)
  = Right (EditorState viewPort1 curTrack1 pointerPos1)
  where
    -- Viewport
    dv        = 10 *^ direction keysDown
    drot      = 0
    dzoom     = 0
    viewPort1 = updateViewPort dv drot dzoom viewPort0

  
    curTrack1   = curTrack0
    pointerPos1 = pointerPos0 ^+^ (0.1 *^ ignoreCoordinateSystem mouseMovement)

