module Editor where

--------------------------------------------------------------------------------
import Input
import Editor.Render
import Editor.Pillar
import Editor.GUI
import Editor.Waypoint
import Editor.Cache
import Editor.Nodes
import Track
import Track.Road
import SF
import Vec
import Types
import Util
import Grid

import Graphics.Gloss

import Prelude hiding ((.), id)
import Control.Monad
import Data.Functor
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Tuple
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
  let checkpoints = undefined
  changeMode_ <- sampleOnRisingEdge
    -< ( keyDown ChangeMode input
       , switchToF (editor' edSF' switchToF) (GameTrack roadCheck pillars pic checkpoints)
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
    action | keyTriggered RMB input          = PlaceNewWaypoint
           | keyDown LMB input               = DragWaypoint
           | keyTriggered EditorDelete input = DeleteWaypoint
           | otherwise                       = NoWaypointAction
