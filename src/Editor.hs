module Editor where

--------------------------------------------------------------------------------
import Input
import Editor.Render
import Editor.TrackState
import Editor.GUI
import Track
import SF
import Vec
import Types
import Util

import Graphics.Gloss

import Control.Lens
import Prelude hiding ((.), id)
--------------------------------------------------------------------------------

type TrackEditCommand = (Maybe Waypoint, Maybe Pillar)

editor :: TrackState -> Game -> ProgMode
editor ts0 = editor' (editorSF ts0)

editor' :: (Input ~> (Output, Track))
        -> Game
        -> ProgMode
editor' edSF switchToF = Mode $ proc input -> do
  ((out, track), edSF') <- inspect edSF -< input
  changeMode_ <- sampleOnRisingEdge -< (keyDown ChangeMode input, switchToF (editor' edSF' switchToF) track)
  returnA -< (changeMode_, out)

editorSF :: TrackState -> (Input ~> (Output, Track))
editorSF ts0 = proc input -> do
  gui_ <- gui -< input
  nextSubMode <- risingEdge -< keyDown EditorNextSubMode input
  clearTrackState <- risingEdge -< keyDown EditorClear input

  -- out <- runMode placingTrackMode -< (input, gui_)
  rec
    dTrackState <- delay ts0 -< trackState
    (editCommands, pic) <- cycleSwitch placingTrackMode placingPillarMode
                        -< ((input, gui_, dTrackState), nextSubMode)
    trackState <- trackStateSF ts0 -< (editCommands, clearTrackState)

  -- Save current track, if requested.
  writeFile
    <- sampleOnRisingEdge
    -< (keyDown EditorSave input, FileOutput "track" (show (extractSaveData trackState)))

  -- Finalize outputs.
  let pic' = _gui_overlay gui_ pic

  let output = Output pic' writeFile

  returnA -< (output, extractTrack trackState)

placingPillarMode :: (Input, GUI, TrackState) ~> (TrackEditCommand, Picture)
placingPillarMode = proc (input, GUI _ placePillarPos _ _, trackState) -> do
  let
    Vec _ mouseDy = _input_mouseMovement input
    adjusting = keyDown EditorAdjust input
    dRadius | adjusting = mouseDy
            | otherwise = zeroVec
  curRadius <- stateful 30 (\_ v s -> (v + s) `max` 0) -< dRadius

  placePillar <- risingEdge -< keyDown EditorCommit input
  let placePillarEvent = sample placePillar (placePillarPos, curRadius)

  let pic = renderPlacingPillarMode placePillarPos trackState curRadius

  returnA -< ((Nothing, placePillarEvent), pic)

placingTrackMode :: (Input, GUI, TrackState) ~> (TrackEditCommand, Picture)
placingTrackMode = proc (input, gui, trackState) -> do
  -- Change track width.
  let
    Vec _ mouseDy      = _input_mouseMovement input
    adjusting          = keyDown EditorAdjust input
    dWidth | adjusting = mouseDy
           | otherwise = zeroVec
  curWidth <- stateful 100 (\_ v s -> (v + s) `max` 0) -< dWidth

  -- Update the track state with a new waypoint when the user clicks.
  placeWaypoint <- risingEdge -< keyDown EditorCommit input
  let
    placeTrackPos      = _gui_cursorWorldPos gui
    placeWaypointEvent = sample placeWaypoint (placeTrackPos, curWidth)

  let pic = renderPlacingTrackMode placeTrackPos trackState curWidth

  returnA -< ((placeWaypointEvent, Nothing), pic)

trackStateUpdate :: TrackState -> TrackEditCommand -> TrackState
trackStateUpdate ts (Just (pos,width), _          ) = addWaypoint ts pos width
trackStateUpdate ts (Nothing         , Just pillar) = addPillar ts pillar
trackStateUpdate ts (Nothing         , Nothing)     = ts

trackStateSF :: TrackState -> ((TrackEditCommand, Bool) ~> TrackState)
trackStateSF ts0 = second (arr $ \b -> if b then Just emptyEditorTrackState else Nothing)
  >>> stateWithReset ts0 (const (flip trackStateUpdate))
  

