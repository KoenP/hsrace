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
import Data.Bifunctor
--------------------------------------------------------------------------------

data EditorInput = EditorInput
  { _input_pointer :: Vec Window
  , _input_dir :: Vec World
  }
makeLenses 'EditorInput

updateEditor :: Double -> Input -> EditorState -> ProgramState
updateEditor _ (Input _ keysTriggered _) state
  | Mode `Set.member` keysTriggered
  = Left $ initialGameState (view es_trackState state)
updateEditor
  _
  input
  (EditorState viewPort0 trackState0 pointerPos0 curWidth0 _)
  = Right (EditorState viewPort1 trackState1 pointerPos1 curWidth1 writeToFile)
  where
    adjustingWidth = keyDown EditorAdjustWidth input
    
    -- Viewport
    mouseMovement = _input_mouseMovement input
    dv            = 10 *^ direction input
    drot          = 0
    dzoom         = 0
    viewPort1     = updateViewPort dv drot dzoom viewPort0
    pointerPos1   | not adjustingWidth = pointerPos0 ^+^ (0.1 *^ ignoreCoordinateSystem mouseMovement)
                  | otherwise = pointerPos0

    curWidth1 | adjustingWidth = let (Vec _ y) = mouseMovement in curWidth0 + y
              | otherwise      = curWidth0
  
    trackState1
      | keyTriggered EditorPlaceTrack input
      = updateTrackState trackState0 (windowCoordsToWorldCoords viewPort1 pointerPos1) curWidth1
      | otherwise
      = trackState0

    writeToFile = keyTriggered EditorSave input

-- TODO pile of crap atm
updateTrackState :: TrackState -> Vec World -> Double -> TrackState
updateTrackState trackState newPos curWidth = TS (fromWaypoints' (revRev waypointsR)) (bimap revList revList $ trackCorners (revRev waypointsR)) (waypointsR `revSnoc` (newPos, curWidth))
  where
    waypointsR = view ts_waypointsR trackState
  -- | length waypoints == 2 = ETS [last $ fromWaypoints' (map (,200) (newWaypoint : waypoints))] (newWaypoint : waypoints)
  -- | length waypoints < 3  = ETS []                        (newWaypoint : waypoints)
  -- | otherwise             = ETS (newCacheSegment : cache) (newWaypoint : waypoints)
  -- where
  --   newCacheSegment = fromWaypoints' (map (,200) (newWaypoint : take 3 waypoints)) !! 1

renderEditorState :: EditorState -> Picture
renderEditorState (EditorState viewPort trackState mousePos@(Vec ptrX ptrY) curWidth _) = 
  let
    -- finalSegments = fromWaypoints'
    --               $ map (,200) (windowCoordsToWorldCoords viewPort mousePos : take 3 waypointsR)
    -- renderedTrack = renderTrack (finalSegments ++ cache)
    waypointsR = view ts_waypointsR trackState
    virtualWaypoint = windowCoordsToWorldCoords viewPort mousePos
    renderedTrack = renderTrack $ fromWaypoints'
      $ revRev (waypointsR `revSnoc` (virtualWaypoint, curWidth))
    pointer = translate (realToFrac ptrX) (realToFrac ptrY) $ color white $ circle 4

    (leftCorners, rightCorners) = trackCorners
      $ revRev $ waypointsR `revSnoc` (virtualWaypoint, curWidth)
    cornerCircles = pictures
                  $  [translate (realToFrac x) (realToFrac y) $ color red   $ circle 10 | Vec x y <- leftCorners]
                  ++ [translate (realToFrac x) (realToFrac y) $ color green $ circle 10 | Vec x y <- rightCorners]

      -- pictures [ translate (realToFrac x) (realToFrac y) $ color white $ circle 10 | Vec x y <- corners]
  in
    pictures [applyViewPort viewPort renderedTrack, pointer, applyViewPort viewPort cornerCircles]

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
