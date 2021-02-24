module Waypoints where

import Vec
import Angle
import Polar

type Waypoints = [Vec World]

thetas :: Waypoints -> [Radians Double]
thetas (wp1:wp2:wps) = 0 : zipWith3 turnAngle (wp1:wp2:wps) (wp2:wps) wps ++ [0]
thetas wps = map (const 0) wps

-- | Compute the turn angle of three consecutive waypoints.
turnAngle :: Vec w -> Vec w -> Vec w -> Radians Double 
turnAngle u v w = let Polar _ theta_wv = cartesianToPolar (rotVec (-theta_vu) (w ^-^ v))
                  in theta_wv
  where
    Polar _ theta_vu = cartesianToPolar (v ^-^ u)


-- changeOfBasis :: (Vec World, Vec World) -> Vec World -> Vec World
-- changeOfBasis (Vec x1 y1, Vec x2 y2) (Vec x y) = Vec a b
--   where
--     a = _
--     b = x * y1 - x1 * y 
