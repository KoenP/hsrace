module Editor where

--------------------------------------------------------------------------------
import Vec
import Angle
import Input
import Track
import RenderTrack
import State
import Polar

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Control.Lens
import qualified Data.Set as Set
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
updateEditor _ (Input keysDown keysTriggered mouseMovement) (EditorState viewPort0 curTrack0 pointerPos0)
  = Right (EditorState viewPort1 curTrack1 pointerPos1)
  where
    -- Viewport
    dv        = 10 *^ direction keysDown
    drot      = 0
    dzoom     = 0
    viewPort1 = updateViewPort dv drot dzoom viewPort0
    pointerPos1 = pointerPos0 ^+^ (0.1 *^ ignoreCoordinateSystem mouseMovement)
  
    curTrack1 =
      if Accelerating `Set.member` keysTriggered
      then windowCoordsToWorldCoords viewPort1 pointerPos1 : curTrack0
      else curTrack0

renderEditorState :: EditorState -> Picture
renderEditorState (EditorState viewPort waypointsR mousePos@(Vec ptrX ptrY)) = 
  let
    world = renderTrack
          $ fromWaypoints
          $ windowCoordsToWorldCoords viewPort mousePos : waypointsR
    pointer = translate (realToFrac ptrX) (realToFrac ptrY) $ color white $ circle 4
  in
    pictures [applyViewPort viewPort world, pointer]

extractTrackDescription :: [(Polar, Vec World, Radians Double)] -> TrackDescription
extractTrackDescription = TrackDescription . reverse . map (\(p,_,_) -> p)

-- addWaypoint :: [(Polar, Vec World, Radians Double)]
--             -> Vec World
--             -> [(Polar, Vec World, Radians Double)]
-- addWaypoint []    (Vec _ y) = [(Polar y 0 , Vec 0 y, 0)]
-- addWaypoint ((Polar rad theta, lastWaypoint, netRot):state) mousePos
--   = (_ , _ , _) : (Polar rad (newNetRot - theta) , lastWaypoint , netRot) : state
--   where
--     (dist, newNetRot) = newTrackWaypoint lastWaypoint netRot mousePos
-- 
-- newTrackWaypoint :: Vec World
--                  -> Radians Double
--                  -> Vec World
--                  -> (Double, Radians Double)
-- newTrackWaypoint lastWaypoint netRotation nextWaypoint
--   = (dist, theta)
--   where
--     dv = nextWaypoint ^-^ lastWaypoint
--     dist = norm dv
--     Polar _ theta = cartesianToPolar (rotVec (-netRotation) dv)


--------------------------------------------------------------------------------
