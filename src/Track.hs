module Track
  ( module Track
  , Road
  , RoadSegment
  ) where

--------------------------------------------------------------------------------
import Vec
import Track.Road

import Control.Lens
import Debug.Trace
--------------------------------------------------------------------------------

type Pillar = (Vec World, Double)

pillarPushOut :: Pillar -> Vec World -> Maybe (Vec World)
pillarPushOut (center, radius) vec
  | delta <- vec ^-^ center, dist <- norm delta, dist <= radius
  = Just $ center ^+^ (radius / dist) *^ delta
  | otherwise
  = Nothing

-- TODO rename and move to own module
data Track = Track { _lo_road    :: Road
                   , _lo_pillars :: [Pillar]
                   }
  deriving (Read, Show)
makeLenses ''Track

-- data TrackSaveData
--   = TrackSaveData { _lsd_waypoints :: [Waypoint]
--                   , _lsd_pillars   :: [Pillar]
--                   }
--   deriving (Read, Show)
-- makeLenses ''TrackSaveData
-- 
-- fromSaveData :: TrackSaveData -> Track
-- fromSaveData (TrackSaveData waypoints pillars) = Track (fromWaypoints waypoints) pillars
