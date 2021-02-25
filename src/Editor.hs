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
updateEditor
  _
  (Input keysDown keysTriggered mouseMovement)
  (EditorState viewPort0 trackState0 pointerPos0)
  = Right (EditorState viewPort1 trackState1 pointerPos1)
  where
    -- Viewport
    dv        = 10 *^ direction keysDown
    drot      = 0
    dzoom     = 0
    viewPort1 = updateViewPort dv drot dzoom viewPort0
    pointerPos1 = pointerPos0 ^+^ (0.1 *^ ignoreCoordinateSystem mouseMovement)
  
    trackState1
      | Accelerating `Set.member` keysTriggered
      = updateTrackState trackState0 (windowCoordsToWorldCoords viewPort1 pointerPos1)
      | otherwise
      = trackState0

updateTrackState :: EditorTrackState -> Vec World -> EditorTrackState
updateTrackState (ETS cache waypoints) newWaypoint
  | length waypoints == 2 = ETS [last $ fromWaypoints (newWaypoint : waypoints)] (newWaypoint : waypoints)
  | length waypoints < 3  = ETS []                        (newWaypoint : waypoints)
  | otherwise             = ETS (newCacheSegment : cache) (newWaypoint : waypoints)
  where
    newCacheSegment = fromWaypoints (newWaypoint : take 3 waypoints) !! 1

renderEditorState :: EditorState -> Picture
renderEditorState (EditorState viewPort (ETS cache waypointsR) mousePos@(Vec ptrX ptrY)) = 
  let
    finalSegments = fromWaypoints
                  $ windowCoordsToWorldCoords viewPort mousePos : takeAtMost 3 waypointsR
    renderedTrack = renderTrack (finalSegments ++ cache)
    pointer = translate (realToFrac ptrX) (realToFrac ptrY) $ color white $ circle 4
  in
    pictures [applyViewPort viewPort renderedTrack, pointer]

takeAtMost :: Int -> [a] -> [a]
takeAtMost 0 _      = []
takeAtMost _ []     = []
takeAtMost n (x:xs) = x : takeAtMost (n-1) xs

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
