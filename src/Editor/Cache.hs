module Editor.Cache where

-- Data structure to avoid recomputing the whole road every frame.

--------------------------------------------------------------------------------
import SF
import Track.Road
import Track.Bezier
import Editor.Waypoint
import Track
import Util
import Grid
import Vec

import Graphics.Gloss

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Tuple
--------------------------------------------------------------------------------

-- | For each waypoint ID, we store the waypoint and the polygons and picture
--   for the piece of the road leading back to the previous waypoint.
type Cache = Map WaypointID (Waypoint, Road, Picture)
type CacheGrid = Grid World (WaypointID, WaypointComponent)

----------------------------------------
-- Construct cache.
----------------------------------------
emptyCache :: Cache
emptyCache = Map.empty

fromWaypoints :: [Waypoint] -> Cache
fromWaypoints waypoints = composeMany
  (reverse [writeWaypoint id wp | (id,wp) <- map WaypointID [0..] `zip` waypoints])
  emptyCache

waypointCache :: Cache -> ((Vec World, WaypointsAction) ~> (Cache, Picture))
waypointCache cache0 = runMode (notDraggingMode cache0)
  where
    notDraggingMode ::  Cache -> Mode (Vec World, WaypointsAction) (Cache, Picture)
    notDraggingMode cache0 =
      let
        grid0 = cacheToGrid cache0
      in
        Mode $ proc (cursorPos, action) -> do
          rec
            dGrid <- delay grid0 -< grid

            -- Start dragging if the user starts holding down the mouse button
            -- while the mouse is within range of a node.
            let
              highlightedWaypoint = do
                (pos, nearestWaypointID) <- closestNearby dGrid cursorPos
                guard (pos <-> cursorPos <= nodeRadius)
                return (pos, nearestWaypointID)
              highlightedWaypointID = fmap snd highlightedWaypoint
        
            -- Add or delete waypoints.
            dCache <- delay cache0 -< cache
            let
              newCache = performCardinalityAction action highlightedWaypointID cursorPos dCache
              newGrid  = fmap cacheToGrid newCache
            cache <- setter cache0 -< newCache
            grid  <- setter grid0 -< newGrid

              
          startDragging <- risingEdge -< action == DragWaypoint
          let
            startDraggingEvent = guard startDragging >> highlightedWaypoint
              <&> \(pos,id) -> draggingMode (pos ^-^ cursorPos) id cache

          -- Render waypoints.
          let pic = renderWaypoints (maybeToList highlightedWaypointID) cache

          returnA -< (startDraggingEvent, (cache, pic))

    -- Drag a waypoint node around. The behavior depends strongly on
    -- which type of node is being dragged, see the `edit` function.
    -- We stop dragging when the drag button is released.
    draggingMode :: Vec World -> (WaypointID, WaypointComponent) -> Cache
                 -> Mode (Vec World, WaypointsAction) (Cache, Picture)
    draggingMode offset (id,component) cache0 =
      let
        waypoint0 = fromJust (lookupWaypoint id cache0)
        
        -- This function describes how to update the waypoint as one
        -- of its components is being dragged.
        edit :: Vec World -> Waypoint -> Waypoint
        edit
          -- Moving the anchor is straightforward, we just overwrite the anchor coordinates.
          | Anchor <- component
          = \cursorPos (Waypoint _ offsets width) -> Waypoint (cursorPos ^+^ offset) offsets width

          -- When a control point point is moved, we mirror its
          -- rotation around the anchor in the other control point.
          | ControlPoint i <- component
          = \cursorPos (Waypoint anchor cpOffsets width) ->
              let
                cpIdentities           = if i == 1 then \x -> x else swap
                (cp1Offset, cp2Offset) = cpIdentities cpOffsets
                newCp1Absolute         = cursorPos ^+^ offset
                newCp1Offset           = newCp1Absolute ^-^ anchor
                theta                  = newCp1Offset `signedInternalAngle` cp1Offset
                newCp2Offset           = (-theta) `rotVec` cp2Offset
              in
                Waypoint anchor (cpIdentities (newCp1Offset, newCp2Offset)) width

          -- We drag a whisker alongside an imaginary "rail"
          -- perpendicular to the the heading of the road in the
          -- waypoint.
          | Whisker _ <- component
          = \cursorPos (Waypoint anchor (offset1, offset2) _) ->
              let
                rail = normalize (perp offset1)
                width = abs ((cursorPos ^+^ offset ^-^ anchor) `scalarProjectionOnto` rail)
              in
                Waypoint anchor (offset1, offset2) width

        -- Update the cache based on cursor movement.
        -- We update the entry for the dragged waypoint, as well as
        -- for the next waypoint in the cache, if it exists, to update
        -- the cached road and picture.
        step :: Vec World -> (Cache, Waypoint) -> (Cache, Waypoint)
        step cursorPos (old, waypoint) =
          let newWp = edit cursorPos waypoint
              cache' = writeWaypoint id newWp old
          in case Map.lookupGT id cache' of Just (id', (newNextWp,_,_)) -> (writeWaypoint id' newNextWp cache', newWp)
                                            Nothing -> (cache', newWp)
      in
        Mode $ proc (cursorPos, action) -> do
          (cache, _) <- stateful' (cache0, waypoint0) step -< cursorPos
          let stopDraggingEvent = sample (action == NoWaypointAction) (notDraggingMode cache)
          returnA -< (stopDraggingEvent, (cache, renderWaypoints [(id,component)] cache))
      

    renderWaypoints :: [(WaypointID, WaypointComponent)] -> Cache -> Picture
    renderWaypoints highlightedIDs cache = pictures
      $  [renderWaypoint wp True (highlighting component)
         | (id,component) <- highlightedIDs, wp <- maybeToList (lookupWaypoint id cache)
         ]
      ++ [ renderWaypoint wp False unhighlighted
         | wp <- cacheWaypoints (mapDeleteMany (map fst highlightedIDs) cache)
         ]

-- | Perform actions that change the number of waypoints (insertions, deletions, appends, ...).
performCardinalityAction :: WaypointsAction -> Maybe (WaypointID, WaypointComponent) -> Vec World -> Cache -> Maybe Cache
performCardinalityAction PlaceNewWaypoint Nothing          cursor old = Just $ appendWaypoint cursor old
performCardinalityAction PlaceNewWaypoint (Just (id,_))    _      old = Just $ insertWaypointBefore id old

performCardinalityAction DeleteWaypoint   maybeHighlighted _      old = do
  (id, component) <- maybeHighlighted
  guard (component == Anchor)
  return (deleteWaypoint id old)
performCardinalityAction _                _                _      _   = Nothing
 
----------------------------------------
-- Update cache.
----------------------------------------
-- | Add a new waypoint to the end of the road, calculating reasonable defaults
--   for offsets and width by looking at the last waypoint already in the cache.
appendWaypoint :: Vec World -> Cache -> Cache
appendWaypoint position cache
  | Just (id, Waypoint anchor (_, offset2) width) <- lookupLastWaypoint cache
  = let
      dir      = normalize $ position ^-^ (anchor ^+^ offset2)
      len      = norm offset2
      offset1' = (- len) *^ dir
      offset2' = len *^ dir
      waypoint = Waypoint position (offset1', offset2') width
    in
      writeWaypoint (nextWaypointID id) waypoint cache
  | otherwise
  = Map.fromList [(WaypointID 0, (Waypoint position (Vec 0 (-100), Vec 0 100) defaultWaypointWidth, [], blank))]

deleteWaypoint :: WaypointID -> Cache -> Cache
deleteWaypoint id old =
  let withoutId = id `Map.delete` old
  in case id `Map.lookupGT` old of
    Just (nextId, (wp,_,_)) -> writeWaypoint nextId wp withoutId
    Nothing                 -> withoutId

insertWaypointBefore :: WaypointID -> Cache -> Cache
insertWaypointBefore idOfNext old = fromMaybe old $ do
  (wpNext, _, _) <- idOfNext `Map.lookup` old
  (wpPrevId, (wpPrev, _, _)) <- idOfNext `Map.lookupLT` old
  let id            = betweenWaypointIDs wpPrevId idOfNext
  let bezier        = waypointsToBezier wpPrev wpNext
  let curve         = cubicCurve  bezier
  let curveDeriv    = cubicCurveDerivative bezier
  let anchor        = curve 0.5
  let controlPoints = (20 *^ normalize (neg (curveDeriv 0.5)), 20 *^ normalize (curveDeriv 0.5))
  let wp            = Waypoint anchor controlPoints ((width wpPrev + width wpNext) / 2)
  return $ refreshWaypoint idOfNext (writeWaypoint id wp old)


refreshWaypoint :: WaypointID -> Cache -> Cache
refreshWaypoint id old | Just (wp,_,_) <- id `Map.lookup` old = writeWaypoint id wp old
                       | otherwise                           = error "tried to refresh nonexistant waypoint"
    
-- | Insert or overwrite waypoint with the given ID, updating the
--   cached road and picture *for that waypoint only*.
writeWaypoint :: WaypointID -> Waypoint -> Cache -> Cache
writeWaypoint id wp old
  | Just (_, (wp', _, _)) <- Map.lookupLT id old
  = let roadSegment = mkRoadSegment wp' wp
    in Map.insert id (wp, roadSegment, renderRoadSegment roadSegment) old
  | otherwise
  = Map.insert id (wp, [], blank) old

mkRoadSegment :: Waypoint -> Waypoint -> RoadSegment
mkRoadSegment wp1 wp2
  = bezierToRoadSegment (width wp1) (width wp2) (waypointsToBezier wp1 wp2)

----------------------------------------
-- Read from cache.
----------------------------------------
readCache :: Cache -> ([Waypoint], Road, Picture)
readCache cache = let (wps, segs, pics) = unzip3 (Map.elems cache)
                  in (wps, concat segs, pictures pics)
      
cacheToGrid :: Cache -> Grid World (WaypointID, WaypointComponent)
cacheToGrid cache = mkGrid 100
  [ ((id, componentID), pos)
  | (id, (wp,_,_)) <- Map.toList cache
  , (componentID, pos) <- wpVecsWithComponents wp
  ]

lookupWaypoint :: WaypointID -> Cache -> Maybe Waypoint
lookupWaypoint id cache = fst3 <$> Map.lookup id cache

lookupLastWaypoint :: Cache -> Maybe (WaypointID, Waypoint)
lookupLastWaypoint = fmap (\(id,(wp,_,_)) -> (id,wp)) . Map.lookupMax

cacheWaypoints :: Cache -> [Waypoint]
cacheWaypoints = fst3 . readCache
