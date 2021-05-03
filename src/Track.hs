module Track
  ( module Track
  , Road
  , RoadSegment
  ) where

--------------------------------------------------------------------------------
import Vec
import Track.Road

import Graphics.Gloss (Picture)

import Control.Lens
import Debug.Trace
--------------------------------------------------------------------------------

type Pillar = Vec World

pillarRadius :: Double
pillarRadius = 60

pillarPushOut :: Pillar -> Vec World -> Maybe (Vec World)
pillarPushOut center vec
  | delta <- vec ^-^ center, dist <- norm delta, dist <= pillarRadius
  = Just $ center ^+^ (pillarRadius / dist) *^ delta
  | otherwise
  = Nothing

-- | Everything the game module needs to know about the track.
data GameTrack = GameTrack
  { _gt_onRoad :: Vec World -> Bool
  , _gt_pillars :: [Pillar]
  , _gt_pic :: Picture
  }
