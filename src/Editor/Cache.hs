module Editor.Cache where

-- Data structure to avoid recomputing the whole road every frame.

--------------------------------------------------------------------------------
import SF
import Editor.Waypoint
import Track
import Util
import Grid
import Vec

import Graphics.Gloss

import Prelude hiding ((.), id)
import Control.Applicative
import qualified Data.Map as Map
import Data.Map (Map)
--------------------------------------------------------------------------------

-- | For each waypoint ID, we store the waypoint and the polygons and picture
--   for the piece of the road leading back to the previous waypoint.
type Cache = Map WaypointID (Waypoint, Road, Picture)

----------------------------------------
-- Construct cache.
----------------------------------------
emptyCache :: Cache
emptyCache = Map.empty

fromWaypoints :: [Waypoint] -> Cache
fromWaypoints waypoints = composeMany
  (reverse [writeWaypoint id wp | (id,wp) <- map WaypointID [0..] `zip` waypoints])
  emptyCache


cacheSF :: Cache -> (Maybe (WaypointID, Waypoint), Maybe (WaypointID, Waypoint)) ~> Cache
cacheSF cache0 = uncurry (<|>) ^>> stateful' cache0 step
  where
    step :: Maybe (WaypointID, Waypoint) -> Cache -> Cache
    step Nothing        old = old
    step (Just (id,wp)) old =
      let cache' = writeWaypoint id wp old
      in case Map.lookupGT id cache' of Just (id', (wp',_,_)) -> writeWaypoint id' wp' cache'
                                        Nothing -> cache'

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
      writeWaypoint (succ id) waypoint cache

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
