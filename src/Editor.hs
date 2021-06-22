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
  changeMode_ <- sampleOnRisingEdge
    -< ( keyDown ChangeMode input
       , switchToF (editor' edSF' switchToF) (GameTrack roadCheck pillars pic)
       )
  returnA -< (changeMode_, out)

editorSF :: EditorInitialization -> (Input ~> (Output, Cache, [Pillar]))
editorSF (TrackSaveData waypoints0 pillars0, trackFilePath) = proc input -> do
  gui_ <- gui -< input
  nextSubMode <- risingEdge -< keyDown EditorNextSubMode input
  -- clearTrackState <- risingEdge -< keyDown EditorClear input
  let cursor = _gui_cursorWorldPos gui_
  let dragging = keyDown LMB input

  -- (cache, waypointsPic, waypointIsHighlighted) <- waypoints (fromWaypoints waypoints0)
  --   -< ( cursor
  --      , dragging
  --      , keyTriggered RMB input
  --      )
  (cache, waypointsPic) <- waypointCache (fromWaypoints waypoints0) -< wpsInputs gui_ input
  let (waypoints_, _, roadPic) = readCache cache
  let waypointIsHighlighted = False -- TODO

  (pillars, pillarsPic) <- pillars pillars0
    -< (cursor, dragging && not waypointIsHighlighted, keyTriggered Space input)

  -- Save current track, if requested.
  let saveKeyTriggered = keyTriggered EditorSave input
  let saveData = TrackSaveData waypoints_ pillars
  let writeFile = sample saveKeyTriggered $ FileOutput trackFilePath (show saveData)
    -- <- sampleOnRisingEdge
    -- -< ( traceResult $ saveKeyDown
    --    , 
    --    )

  -- Finalize outputs.
  let pic' = _gui_overlay gui_ (pictures [roadPic, waypointsPic, pillarsPic])
  let output = Output pic' writeFile

  returnA -< (output, cache, pillars)

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
    action | keyTriggered RMB input = PlaceNewWaypoint
           | keyDown LMB input      = DragWaypoint
           | otherwise              = NoWaypointAction
  
-- wps :: Cache -> ((Vec World, WaypointsAction) ~> (Cache, Picture))
-- wps cache0 = runMode (notDraggingMode cache0)
  -- where
    -- notDraggingMode ::  Cache -> Mode (Vec World, WaypointsAction) (Cache, Picture)
    -- notDraggingMode cache0 =
      -- let
        -- grid0 = cacheToGrid cache0
      -- in
        -- Mode $ proc (cursorPos, action) -> do
          -- -- Add new waypoint.
          -- let
            -- newCache = guard (action == PlaceNewWaypoint)
                       -- $> appendWaypoint cursorPos cache0
            -- newGrid = fmap cacheToGrid newCache
          -- cache <- setter cache0 -< newCache
          -- grid <- setter grid0 -< newGrid
-- 
          -- -- Start dragging if the user starts holding down the mouse button
          -- -- while the mouse is within range of a node.
          -- let
            -- highlightedWaypoint = do
              -- (pos, nearestWaypointID) <- closestNearby grid cursorPos
              -- guard (pos <-> cursorPos <= nodeRadius)
              -- return (pos, nearestWaypointID)
            -- highlightedWaypointID = fmap snd highlightedWaypoint
             --  
          -- startDragging <- risingEdge -< action == DragWaypoint
          -- let
            -- startDraggingEvent = guard startDragging >> highlightedWaypoint
              -- <&> \(pos,id) -> draggingMode (pos ^-^ cursorPos) id cache
-- 
          -- -- Render waypoints.
          -- let pic = renderWaypoints (maybeToList highlightedWaypointID) cache
-- 
          -- returnA -< (startDraggingEvent, (cache, pic))
-- 
    -- -- Drag a waypoint node around. The behavior depends strongly on
    -- -- which type of node is being dragged, see the `edit` function.
    -- -- We stop dragging when the drag button is released.
    -- draggingMode :: Vec World -> (WaypointID, WaypointComponent) -> Cache -> Mode (Vec World, WaypointsAction) (Cache, Picture)
    -- draggingMode offset (id,component) cache0 =
      -- let
        -- waypoint0 = fromJust (lookupWaypoint id cache0)
       --  
        -- -- This function describes how to update the waypoint as one
        -- -- of its components is being dragged.
        -- edit :: Vec World -> Waypoint -> Waypoint
        -- edit
          -- -- Moving the anchor is straightforward, we just overwrite the anchor coordinates.
          -- | Anchor <- component
          -- = \cursorPos (Waypoint _ offsets width) -> Waypoint (cursorPos ^+^ offset) offsets width
-- 
          -- -- When a control point point is moved, we mirror its
          -- -- rotation around the anchor in the other control point.
          -- | ControlPoint i <- component
          -- = \cursorPos (Waypoint anchor cpOffsets width) ->
              -- let
                -- cpIdentities           = if i == 1 then \x -> x else swap
                -- (cp1Offset, cp2Offset) = cpIdentities cpOffsets
                -- newCp1Absolute         = cursorPos ^+^ offset
                -- newCp1Offset           = newCp1Absolute ^-^ anchor
                -- theta                  = newCp1Offset `signedInternalAngle` cp1Offset
                -- newCp2Offset           = (-theta) `rotVec` cp2Offset
              -- in
                -- Waypoint anchor (cpIdentities (newCp1Offset, newCp2Offset)) width
-- 
          -- -- We drag a whisker alongside an imaginary "rail"
          -- -- perpendicular to the the heading of the road in the
          -- -- waypoint.
          -- | Whisker i <- component
          -- = \cursorPos (Waypoint anchor (offset1, offset2) _) ->
              -- let
                -- rail = normalize (perp offset1)
                -- width = abs ((cursorPos ^+^ offset ^-^ anchor) `scalarProjectionOnto` rail)
              -- in
                -- Waypoint anchor (offset1, offset2) width
-- 
        -- -- Update the cache based on cursor movement.
        -- -- We update the entry for the dragged waypoint, as well as
        -- -- for the next waypoint in the cache, if it exists, to update
        -- -- the cached road and picture.
        -- step :: Vec World -> (Cache, Waypoint) -> (Cache, Waypoint)
        -- step cursorPos (old, waypoint) =
          -- let newWp = edit cursorPos waypoint
              -- cache' = writeWaypoint id newWp old
          -- in case Map.lookupGT id cache' of Just (id', (newNextWp,_,_)) -> (writeWaypoint id' newNextWp cache', newWp)
                                            -- Nothing -> (cache', newWp)
      -- in
        -- Mode $ proc (cursorPos, action) -> do
          -- (cache, _) <- stateful' (cache0, waypoint0) step -< cursorPos
          -- let stopDraggingEvent = sample (action == NoWaypointAction) (notDraggingMode cache)
          -- returnA -< (stopDraggingEvent, (cache, renderWaypoints [(id,component)] cache))
     --  
-- 
    -- renderWaypoints :: [(WaypointID, WaypointComponent)] -> Cache -> Picture
    -- renderWaypoints highlightedIDs cache = pictures
      -- $  [renderWaypoint wp True (highlighting component)
         -- | (id,component) <- highlightedIDs, wp <- maybeToList (lookupWaypoint id cache)
         -- ]
      -- ++ [ renderWaypoint wp False unhighlighted
         -- | wp <- cacheWaypoints (mapDeleteMany (map fst highlightedIDs) cache)
         -- ]

--------------------------------------------------------------------------------


-- | Keep track of a set of waypoints while avoiding re-rendering at every step.
--   The boolean return values indicates whether a waypoint node is in range
--   for dragging.
-- waypoints :: Cache -> (Vec World, Bool, Bool) ~> (Cache, Picture, Bool)
-- waypoints cache0 =
--   let
--     gridCellSize = 100  
--     nextID0 = case Map.lookupMax cache0 of
--       Nothing               -> 0
--       Just (WaypointID k,_) -> k + 1
-- 
--     wps0 = Map.fromList [ (k, ((wp, renderWaypoint wp False unhighlighted), waypoint wp))
--                         | (k, (wp,_,_)) <- Map.toList cache0
--                         ]
--   in
--     proc (cursorPos, dragging, addNew) -> do
--   
--       nextID <- WaypointID ^<< stateful' nextID0 (+) -< boolToInt addNew
--       stoppedDragging <- risingEdge -< not dragging
--       let updateGrid = stoppedDragging || addNew
-- 
--       rec
--         lastWaypoint_ <- delay Nothing -< lastWaypoint
-- 
--         -- Compute where to place a new waypoint, if needed.
--         let 
--           newWaypoint :: Waypoint
--           newWaypoint = case lastWaypoint_ of
--             Nothing -> Waypoint cursorPos (Vec 0 (-120), Vec 0 120) defaultWaypointWidth
--             Just (_, (Waypoint anchor (_, offset2) width, _)) ->
--               let
--                 dir = normalize $ cursorPos ^-^ (anchor ^+^ offset2)
--                 len = norm offset2
--                 offset1' = (- len) *^ dir
--                 offset2' = len *^ dir
--               in
--                 Waypoint cursorPos (offset1', offset2') width
--                       
--           newWaypointEvent = sample addNew (nextID, ((newWaypoint, blank), waypoint newWaypoint))
-- 
--         -- Keep track of a grid of all waypoint nodes.
--         dGrid <- delay (mkGrid gridCellSize []) -< grid
--         nearestWaypointID <- delay Nothing <<< highlightedWaypoint
--           -< (cursorPos, dragging, dGrid)
--         waypointMap <- sparseUpdater wps0
--           -< ( []
--              , maybeToList newWaypointEvent
--              , maybeToList nearestWaypointID `zip` [(cursorPos, dragging)]
--              )
--         let lastWaypoint = Map.lookupMax waypointMap
--         let newGrid = mkGrid gridCellSize [(id,vec) | (id,(wp,_)) <- Map.toList waypointMap, vec <- wpVecsAbsolute wp]
--         grid <- setter (mkGrid gridCellSize []) -< sample updateGrid newGrid
-- 
--       let wpPics = map snd $ Map.elems waypointMap
-- 
--       cache <- cacheSF cache0
--         -< ((\(x,((y,_),_)) -> (x,y)) <$> newWaypointEvent
--            , do { guard dragging
--                 ; id <- nearestWaypointID
--                 ; (wp,_) <- id `Map.lookup` waypointMap
--                 ; return (id,wp)
--                 }
--            )
--       returnA -< (cache, pictures wpPics, traceShow nearestWaypointID (isJust nearestWaypointID)) -- (map fst (Map.elems waypointMap), pictures (renderGrid grid : wpPics))
