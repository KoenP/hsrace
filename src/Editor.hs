module Editor where

--------------------------------------------------------------------------------
import Input
import Editor.Render
import Editor.Cache
import Editor.GUI
import Editor.Waypoint
import Editor.Cache
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
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
--------------------------------------------------------------------------------

-- type TrackEditCommand = (Maybe Waypoint, Maybe Pillar)

editor :: Cache -> Game -> ProgMode
editor cache0 = editor' (editorSF cache0)

editor' :: (Input ~> (Output, Cache))
        -> Game
        -> ProgMode
editor' edSF switchToF = Mode $ proc input -> do
  ((out, cache), edSF') <- inspect edSF -< input
  let (_, road, pic) = readCache cache
  let roadCheck = checkOnRoad road
  changeMode_ <- sampleOnRisingEdge
    -< ( keyDown ChangeMode input
       , switchToF (editor' edSF' switchToF) roadCheck pic
       )
  returnA -< (changeMode_, out)

editorSF :: Cache -> (Input ~> (Output, Cache))
editorSF cache0 = proc input -> do
  gui_ <- gui -< input
  nextSubMode <- risingEdge -< keyDown EditorNextSubMode input
  -- clearTrackState <- risingEdge -< keyDown EditorClear input

  (cache, waypointsPic) <- waypoints cache0
    -< ( _gui_cursorWorldPos gui_
       , keyDown LMB input
       , keyTriggered RMB input
       )
  let (_,_,roadPic) = readCache cache

  -- Save current track, if requested.
  -- writeFile
  --   <- sampleOnRisingEdge
  --   -< ( keyDown EditorSave input
  --      , FileOutput "track" (show (extractSaveData trackState))
  --      )

  -- Finalize outputs.
  let pic' = _gui_overlay gui_ (pictures [roadPic, waypointsPic])

  let output = Output pic' Nothing -- writeFile

  returnA -< (output, cache)

-- placingPillarMode :: (Input, GUI, TrackState) ~> (TrackEditCommand, Picture)
-- placingPillarMode = proc (input, GUI _ placePillarPos _ _, trackState) -> do
--   let
--     Vec _ mouseDy = error "fix this in Editor" -- _input_mouseMovement input
--     adjusting = keyDown EditorAdjust input
--     dRadius | adjusting = mouseDy
--             | otherwise = zeroVec
--   curRadius <- stateful 30 (\_ v s -> (v + s) `max` 0) -< dRadius
-- 
--   placePillar <- risingEdge -< keyDown EditorCommit input
--   let placePillarEvent = sample placePillar (placePillarPos, curRadius)
-- 
--   let pic = renderPlacingPillarMode placePillarPos trackState curRadius
-- 
--   returnA -< ((Nothing, placePillarEvent), pic)
-- 
-- placingTrackMode :: (Input, GUI, TrackState) ~> (TrackEditCommand, Picture)
-- placingTrackMode = proc (input, gui, trackState) -> do
--   -- Change track width.
--   let
--     Vec _ mouseDy      = error "fix this in Editor" -- _input_mouseMovement input
--     adjusting          = keyDown EditorAdjust input
--     dWidth | adjusting = mouseDy
--            | otherwise = zeroVec
--   curWidth <- stateful 100 (\_ v s -> (v + s) `max` 0) -< dWidth
-- 
--   -- Update the track state with a new waypoint when the user clicks.
--   placeWaypoint <- risingEdge -< keyDown EditorCommit input
--   let
--     placeTrackPos      = _gui_cursorWorldPos gui
--     placeWaypointEvent = sample placeWaypoint (placeTrackPos, curWidth)
-- 
--   let pic = renderPlacingTrackMode placeTrackPos trackState curWidth
-- 
--   returnA -< ((placeWaypointEvent, Nothing), pic)
-- 
-- trackStateUpdate :: TrackState -> TrackEditCommand -> TrackState
-- trackStateUpdate ts (Just (pos,width), _          ) = addWaypoint ts pos width
-- trackStateUpdate ts (Nothing         , Just pillar) = addPillar ts pillar
-- trackStateUpdate ts (Nothing         , Nothing)     = ts
-- 
-- trackStateSF :: TrackState -> ((TrackEditCommand, Bool) ~> TrackState)
-- trackStateSF ts0 = second (arr $ \b -> if b then Just emptyEditorTrackState else Nothing)
--   >>> stateWithReset ts0 (const (flip trackStateUpdate))
  

--------------------------------------------------------------------------------

highlightedWaypoint :: (Vec World, Bool, Grid World WaypointID) ~> Maybe WaypointID
highlightedWaypoint = runMode notDraggingMode
  where
    notDraggingMode = Mode $ proc (cursorPos, tryingToDrag, grid) -> do
      let
        nearestWaypointID = closestNearby grid cursorPos
        dragging = tryingToDrag && isJust nearestWaypointID

      returnA -< (sample dragging (draggingMode (fromJust nearestWaypointID)), nearestWaypointID)

    draggingMode id = Mode $ proc (_, tryingToDrag, _) -> do
      returnA -< (sample (not tryingToDrag) notDraggingMode, Just id)


waypoints :: Cache -> (Vec World, Bool, Bool) ~> (Cache, Picture)
waypoints cache0 =
  let
    gridCellSize = 100  
  in
    proc (cursorPos, dragging, addNew) -> do
  
      nextID <- WaypointID ^<< stateful' 0 (+) -< boolToInt addNew
      stoppedDragging <- risingEdge -< not dragging
      let updateGrid = stoppedDragging || addNew

      rec
        lastWaypoint_ <- delay Nothing -< lastWaypoint

        let 
          newWaypoint :: Waypoint
          newWaypoint = case lastWaypoint_ of
            Nothing -> Waypoint cursorPos (Vec 0 (-120), Vec 0 120)
            Just (_, (Waypoint anchor (_, offset2), _)) ->
              let
                dir = normalize $ cursorPos ^-^ (anchor ^+^ offset2)
                len = norm offset2
                offset1' = (- len) *^ dir
                offset2' = len *^ dir
              in
                Waypoint cursorPos (offset1', offset2')
                      
          newWaypointEvent = sample addNew (nextID, ((newWaypoint, blank), waypoint newWaypoint))

        grid_ <- delay (mkGrid gridCellSize []) -< grid
        nearestWaypointID <- delay Nothing <<< highlightedWaypoint -< (cursorPos, dragging, grid_)
        waypointMap
          <- sparseUpdater Map.empty
          -< ([], maybeToList newWaypointEvent, maybeToList nearestWaypointID `zip` [(cursorPos, dragging)])
        
        let lastWaypoint = Map.lookupMax waypointMap
        let newGrid = mkGrid gridCellSize [(id,vec) | (id,(wp,_)) <- Map.toList waypointMap, vec <- wpVecsAbsolute wp]


        -- TODO: grid doesn't work properly yet
        -- does not get correctly updated after dragging a control point
        -- we need to ignore the grid while dragging
        grid <- setter (mkGrid gridCellSize []) -< sample updateGrid newGrid

      let wpPics = map snd $ Map.elems waypointMap

      cache <- cacheSF cache0
        -< ((\(x,((y,_),_)) -> (x,y)) <$> newWaypointEvent
           , do { guard dragging
                ; id <- nearestWaypointID
                ; (wp,_) <- id `Map.lookup` waypointMap
                ; return (id,wp)
                }
           )
  
      returnA -< (cache, pictures wpPics) -- (map fst (Map.elems waypointMap), pictures (renderGrid grid : wpPics))
