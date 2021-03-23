module Editor.LayoutState where

------------------------------------------------------------------------------------------
import Vec
import Editor.TrackState
import Input
import State
import Track
import Editor.Overlay
import Util

import Control.Lens
------------------------------------------------------------------------------------------

data EditingMode = PlacingTrack  { _em_pt_curWidth :: Double }
                 | PlacingPillar { _em_pt_pillarRadius :: Double }
  deriving Show 

data LayoutState = LayoutState
  { _ls_trackState :: TrackState
  , _ls_mode       :: EditingMode
  }
  deriving Show
makeLenses 'LayoutState


-- TODO was dit aan het refactoren
-- TODO andere mode

updateLayoutState :: Double -> Input -> OverlayState -> LayoutState -> LayoutState
-- updateLayoutState _ input _ es
--   | keyTriggered EditorNextSubMode input = nextEditorMode es
updateLayoutState
  _
  input@Input  { _input_mouseMovement = mouseMovement}
  OverlayState { _os_viewPort         = viewPort
               , _os_pointerPos       = pointerPos
               }
  LayoutState  { _ls_trackState       = trackState0
               , _ls_mode             = PlacingTrack curWidth0
               } = 
  let
    adjusting = keyDown EditorAdjust input
    curWidth1 | adjusting = let (Vec _ y) = mouseMovement in (curWidth0 + y) `max` 0
              | otherwise      = curWidth0
    trackState1
      | keyTriggered EditorCommit input
      = addWaypoint trackState0 (windowCoordsToWorldCoords viewPort pointerPos) curWidth1
      | otherwise
      = trackState0

    saveToFile = keyTriggered EditorSave input
  in
    LayoutState trackState1 (PlacingTrack curWidth1)

initializeLayoutState :: LayoutSaveData -> LayoutState
initializeLayoutState saveData
  = undefined
  -- = LayoutState (trackStateFromSaveData saveData) placingTrackMode

emptyLayoutState :: LayoutState
emptyLayoutState
  = LayoutState emptyEditorTrackState (PlacingTrack 150)

-- placingTrackMode, placingPillarMode :: EditingMode
-- placingTrackMode = PlacingTrack 120
-- placingPillarMode = PlacingPillar 20

-- nextEditorMode :: LayoutState -> LayoutState
-- nextEditorMode = over ls_mode $ \case
--   PlacingTrack {}  -> placingPillarMode
--   PlacingPillar {} -> placingTrackMode
