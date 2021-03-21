module Editor ( EditorState(..)
              , OverlayState(..)
              , updateEditor
              , renderEditor
              , editorStateExtractLayout
              , editorStateExtractWaypoints
              , editorStateSaveToFile 
              , initializeEditorState 
              , editor
              ) where

--------------------------------------------------------------------------------
import Input
import Editor.LayoutState
import Editor.Render
import Editor.Overlay
import Editor.TrackState
import Track
import SF
import Vec
import Util

import Graphics.Gloss

import Control.Lens
import Prelude hiding ((.), id)
--------------------------------------------------------------------------------

editor :: Input ~> Picture
editor = proc input -> do
  let mouseMovement@(Vec _ mouseDy) = _input_mouseMovement input
  cursorPos <- cumsum -< 0.1 *^ mouseMovement
  viewPort  <- viewPortSF -< input

  let dWidth | keyDown EditorAdjust input = mouseDy
             | otherwise                  = zeroVec
  curWidth <- stateful 100 (\_ v s -> (v + s) `max` 0) -< dWidth
  placeWaypoint <- risingEdge -< keyDown EditorCommit input
  let placeWaypointEvent
        = sample placeWaypoint (windowCoordsToWorldCoords viewPort cursorPos, curWidth)

  trackState
    <- updateOnJust emptyEditorTrackState (\ts (pos,width)
                                          ->  addWaypoint ts pos width)
    -< placeWaypointEvent

  -- waypointsR <- updateOnJust revEmpty revSnoc -< placeWaypointEvent
  -- trackCornersRR <- 
  returnA -< renderEditor (OverlayState cursorPos viewPort undefined)
                          (LayoutState trackState (PlacingTrack curWidth))

viewPortSF :: Input ~> ViewPort
viewPortSF = proc input -> do
  position <- cumsum -< 10 *^ direction input
  returnA -< ViewPort position 0 1

type EditorState = (OverlayState, LayoutState)

updateEditor :: Double
             -> Input
             -> EditorState
             -> EditorState
updateEditor dt input (os0,ls0) =
  let os1 = updateOverlay input os0
  in (os1 , updateLayoutState dt input os1 ls0)

editorStateExtractLayout :: EditorState -> Layout
editorStateExtractLayout (_, LayoutState { _ls_trackState = ts })
  = extractLayout ts

editorStateExtractWaypoints :: EditorState -> [Waypoint]
editorStateExtractWaypoints (_, LayoutState { _ls_trackState = ts })
  = revRev (view ts_waypointsR ts)

editorStateSaveToFile :: EditorState -> Bool
editorStateSaveToFile (OverlayState { _os_saveToFile = saveToFile }, _)
  = saveToFile

initializeEditorState :: LayoutSaveData -> EditorState
initializeEditorState saveData = (initialOverlayState, initializeLayoutState saveData)

-- update

--   (EditorState viewPort1 trackState1 pointerPos1 writeToFile mode1)
--   where
--     -- Viewport
--     adjusting     = keyDown EditorAdjustWidth input
--     mouseMovement = _input_mouseMovement input
--     viewPort1     = updateEditorViewPort input viewPort0
--     pointerPos1   | not adjusting = pointerPos0 ^+^ (0.1 *^ ignoreCoordinateSystem mouseMovement)
--                   | otherwise     = pointerPos0
--     pointerWorldPos = windowCoordsToWorldCoords viewPort1 pointerPos1
--     writeToFile = keyTriggered EditorSave input

-- updatePlacingTrackMode :: Input -> EditorState -> EditorState
-- updatePlacingTrackMode
--   input@Input { _input_mouseMovement = mouseMovement
--               }
--   EditorState { _es_trackState       = trackState0
--               , _es_mode             = PlacingTrack curWidth0
--               , _es_pointerPos       = pointerPos0
--               , _es_viewPort         = viewPort0
--               } =
--   let
--     adjustingWidth = keyDown EditorAdjustWidth input
--     curWidth1 | adjustingWidth = let (Vec _ y) = mouseMovement in (curWidth0 + y) `max` 0
--               | otherwise      = curWidth0
--     viewPort1     = updateEditorViewPort input viewPort0
--     adjusting     = keyDown EditorAdjustWidth input
--     pointerPos1   | not adjusting = updateCursor mouseMovement pointerPos0
--                   | otherwise     = pointerPos0
--     trackState1
--       | keyTriggered EditorCommit input
--       = addWaypoint trackState0 (windowCoordsToWorldCoords viewPort1 pointerPos1) curWidth1
--       | otherwise
--       = trackState0
-- 
--     saveToFile = keyTriggered EditorSave input
--   in
--     EditorState viewPort1 trackState1 pointerPos1 saveToFile (PlacingTrack curWidth1)
-- 
-- updatePlacingPillarMode :: Input -> EditorState -> EditorState
-- updatePlacingPillarMode
--   input@Input { _input_mouseMovement = mouseMovement
--               }
--   EditorState { _es_trackState       = trackState0
--               , _es_mode             = PlacingPillar radius0
--               , _es_pointerPos       = pointerPos0
--               , _es_viewPort         = viewPort0
--               } =
--   let
--     -- TODO get rid of code duplication
--     adjustingWidth = keyDown EditorAdjustWidth input
--     radius1 | adjustingWidth = let (Vec _ y) = mouseMovement in (radius0 + y) `max` 0
--             | otherwise      = radius0
--     viewPort1     = updateEditorViewPort input viewPort0
--     adjusting     = keyDown EditorAdjustWidth input
--     pointerPos1   | not adjusting = updateCursor mouseMovement pointerPos0
--                   | otherwise     = pointerPos0
--     trackState1
--       | keyTriggered EditorCommit input
--       = addPillar trackState0 (windowCoordsToWorldCoords viewPort1 pointerPos1 , radius1)
--       | otherwise
--       = trackState0
--     saveToFile = keyTriggered EditorSave input
--   in
--     EditorState viewPort1 trackState1 pointerPos1 saveToFile (PlacingPillar radius1)




--------------------------------------------------------------------------------
