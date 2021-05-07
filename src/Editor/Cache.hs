module Editor.Cache where

-- Data structure to avoid recomputing the whole road every frame.

--------------------------------------------------------------------------------
import SF
import Editor.Waypoint
import Track
import Util

import Graphics.Gloss

import Control.Applicative
import qualified Data.Map as Map
import Data.Map (Map)
--------------------------------------------------------------------------------

-- | For each waypoint ID, we store the waypoint and the polygons and picture
--   for the piece of the road leading back to the previous waypoint.
type Cache = Map WaypointID (Waypoint, Road, Picture)

emptyCache :: Cache
emptyCache = Map.empty

cacheSF :: Cache -> (Maybe (WaypointID, Waypoint), Maybe (WaypointID, Waypoint)) ~> Cache
cacheSF cache0 = uncurry (<|>) ^>> stateful' cache0 step
  where
    step :: Maybe (WaypointID, Waypoint) -> Cache -> Cache
    step Nothing        old = old
    step (Just (id,wp)) old =
      let cache' = writeWaypoint id wp old
      in case Map.lookupGT id cache' of Just (id', (wp',_,_)) -> writeWaypoint id' wp' cache'
                                        Nothing -> cache'

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

readCache :: Cache -> ([Waypoint], Road, Picture)
readCache cache = let (wps, segs, pics) = unzip3 (Map.elems cache)
                  in (wps, concat segs, pictures pics)
      
fromWaypoints :: [Waypoint] -> Cache
fromWaypoints waypoints = composeMany
  (reverse [writeWaypoint id wp | (id,wp) <- map WaypointID [0..] `zip` waypoints])
  emptyCache
