module Track
  ( module Track
  , module Track.Road
  , module Track.Render
  , module Track.Types
  ) where

--------------------------------------------------------------------------------
import Vec
import Track.Road
import Track.Render
import Track.Types

import Graphics.Gloss (Picture)

import Control.Lens
import Debug.Trace
--------------------------------------------------------------------------------

pillarRadius :: Double
pillarRadius = 60

pillarPushOut :: Pillar -> Vec World -> Maybe (Vec World)
pillarPushOut center vec
  | delta <- vec ^-^ center, dist <- norm delta, dist <= pillarRadius
  = Just $ center ^+^ (pillarRadius / dist) *^ delta
  | otherwise
  = Nothing


