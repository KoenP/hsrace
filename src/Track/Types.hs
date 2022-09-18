module Track.Types where

--------------------------------------------------------------------------------
import Vec

import Graphics.Gloss (Picture)
--------------------------------------------------------------------------------

type Anchors w = (Vec w, Vec w)
type ControlPoints w = (Vec w, Vec w)
data CubicBezier w = CubicBezier (Anchors w) (ControlPoints w)

-- | Waypoints control the shape of the road.
--   Each waypoint consists of an anchor, which the road passes through,
--   as well as the offsets relative to the anchor (so not absolute
--   positions) of two control points, which determine the curvature of the road.
--   The first control point in a waypoint is the second controller of
--   the road shape before the waypoint, the second control point in a
--   waypoint is the first controller of the road shape after the
--   waypoint.
data Waypoint = Waypoint
  { anchor :: Vec World
  , controlPointsOffsets :: ControlPoints World
  , width :: Double
  }
  deriving (Show, Read)

-- | At the smallest scale, a road consists of quadrilaterals which
--   are used for overlap detection and rendering.
type RoadQuad    = [Vec World]

-- | A road is a list of road segments.
type Road        = [RoadQuad]

-- | A road segment is a section of road between two waypoints.
type RoadSegment = Road

type Pillar = Vec World

-- | Everything the game module needs to know about the track.
data GameTrack = GameTrack
  { _gt_onRoad :: Vec World -> Bool
  , _gt_pillars :: [Pillar]
  , _gt_pic :: Picture
  , _gt_crossesLapBoundary :: (Vec World, Vec World) -> Int
  -- , _gt_checkpoints :: [Vec World -> Bool]
  }

-- | The data needed to reconstruct the track.
data TrackSaveData = TrackSaveData
  { _tsd_waypoints :: [Waypoint]
  , _tsd_pillars :: [Pillar]
  }
  deriving (Read, Show)
