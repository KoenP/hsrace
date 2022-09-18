module Editor where

--------------------------------------------------------------------------------
import Input
    ( keyDown,
      keyTriggered,
      GameKey(EditorCloseLoop, ChangeMode, Space, EditorSave, RMB, LMB,
              EditorDelete),
      Input )
import Editor.Pillar ( pillars )
import Editor.GUI ( gui, GUI(_gui_overlay, _gui_cursorWorldPos) )
import Editor.Lap ( lapBoundary, crossesBoundary )
import Editor.Waypoint
    ( nodeRadius, WaypointID, WaypointsAction(..) )
import Editor.Cache
    ( fromWaypoints, readCache, waypointCache, Cache )
import Track
    ( GameTrack(GameTrack),
      Pillar,
      TrackSaveData(TrackSaveData),
      checkOnRoad )
import SF
    ( returnA,
      inspect,
      runMode,
      sample,
      sampleOnRisingEdge,
      Mode(Mode),
      type (~>) )
import Vec ( (<->), Vec, World )
import Types ( FileOutput(FileOutput), Game, Output(..), ProgMode )
import Grid ( closestNearby, Grid )

import Graphics.Gloss ( pictures )

import Prelude hiding ((.), id)
import Control.Monad ( guard )
import Data.Maybe ( fromJust, isJust )
--------------------------------------------------------------------------------

-- type TrackEditCommand = (Maybe Waypoint, Maybe Pillar)
type EditorInitialization = (TrackSaveData, FilePath)

editor :: EditorInitialization -> Game -> ProgMode
editor init game = editor' (editorSF init) game

editor' :: (Input ~> (Output, Cache, [Pillar]))
        -> Game
        -> ProgMode
editor' edSF switchToF = Mode $ proc input -> do
  ((out, cache, pillars), edSF') <- inspect edSF -< input
  let (_, road, roadPic) = readCache cache
  let roadCheck = checkOnRoad road
  let pic = roadPic
  let lapBoundaryInfo = lapBoundary cache
  let checkLapBoundary 
        | Just lb <- lapBoundaryInfo = (`crossesBoundary` lb)
        | otherwise                  = const 0
  changeMode_ <- sampleOnRisingEdge
    -< ( keyDown ChangeMode input
       , switchToF
           (editor' edSF' switchToF)
           (GameTrack roadCheck pillars pic checkLapBoundary)
       )
  returnA -< (changeMode_, out)

editorSF :: EditorInitialization -> (Input ~> (Output, Cache, [Pillar]))
editorSF (TrackSaveData waypoints0 pillars0, trackFilePath) = proc input -> do
  gui_ <- gui -< input
  let cursor = _gui_cursorWorldPos gui_
  let dragging = keyDown LMB input

  (cache, waypointsPic) <- waypointCache (fromWaypoints waypoints0) -< wpsInputs gui_ input
  let (waypoints_, _, roadPic) = readCache cache
  let waypointIsHighlighted = False -- TODO

  (pillars, pillarsPic) <- pillars pillars0
    -< (cursor,
        dragging && not waypointIsHighlighted,
        keyTriggered EditorDelete input,
        keyTriggered Space input)

  -- Save current track, if requested.
  let saveKeyTriggered = keyTriggered EditorSave input
  let saveData = TrackSaveData waypoints_ pillars
  let writeFile = sample saveKeyTriggered $ FileOutput trackFilePath (show saveData)

  -- Finalize outputs.
  let pic' = _gui_overlay gui_ (pictures [roadPic, waypointsPic, pillarsPic])
  let output = Output pic' writeFile

  returnA -< (output, cache, pillars)

--------------------------------------------------------------------------------

highlightedWaypoint :: (Vec World, Bool, Grid World WaypointID) ~> Maybe WaypointID
highlightedWaypoint = runMode notDraggingMode
  where
    -- While not dragging, try to figure out which is the closest waypoint
    -- on every tick.
    notDraggingMode = Mode $ proc (cursorPos, tryingToDrag, grid) -> do
      let
        highlightedWaypointID = do
          (pos, nearestWaypointID) <- closestNearby grid cursorPos
          guard (pos <-> cursorPos <= nodeRadius)
          return nearestWaypointID
        dragging = tryingToDrag && isJust highlightedWaypointID

      returnA -<
        (sample dragging (draggingMode $ fromJust highlightedWaypointID)
        , highlightedWaypointID
        )

    -- While dragging, the currently highlighted waypoint remains highlighted.
    draggingMode id = Mode $ proc (_, tryingToDrag, _) -> do
      returnA -< (sample (not tryingToDrag) notDraggingMode, Just id)

--------------------------------------------------------------------------------
-- | Translate user inputs to inputs to the waypoints signal function.
wpsInputs :: GUI -> Input -> (Vec World, WaypointsAction)
wpsInputs gui input = (_gui_cursorWorldPos gui, action)
  where
    action | keyTriggered RMB input             = PlaceNewWaypoint
           | keyDown LMB input                  = DragWaypoint
           | keyTriggered EditorDelete input    = DeleteWaypoint
           | keyTriggered EditorCloseLoop input = CloseLoop
           | otherwise                          = NoWaypointAction
