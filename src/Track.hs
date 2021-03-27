module Track
  ( module Track
  , Waypoint
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


-- TODO rename and move to own module
data Track = Track { _lo_road    :: Road
                   , _lo_pillars :: [Pillar]
                   }
  deriving (Read, Show)
makeLenses ''Track

data TrackSaveData
  = TrackSaveData { _lsd_waypoints :: [Waypoint]
                  , _lsd_pillars   :: [Pillar]
                  }
  deriving (Read, Show)
makeLenses ''TrackSaveData

--------------------------------------------------------------------------------
-- NEW TRACK CONSTRUCTION
--------------------------------------------------------------------------------
fromSaveData :: TrackSaveData -> Track
fromSaveData (TrackSaveData waypoints pillars) = Track (fromWaypoints waypoints) pillars
