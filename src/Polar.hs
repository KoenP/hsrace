module Polar where

--------------------------------------------------------------------------------
import Vec
import Angle
--------------------------------------------------------------------------------

  
data Polar = Polar {_pRad :: Double, _pTheta :: Angle } deriving (Show, Eq)

-- Transforms polar coordinates to a cartesian system where an angle of
-- 0 radians corresponds to a vector pointing straight up (x component equal
-- to 0), and a positive angle indicates clockwise rotation.
polarToCartesian :: Polar -> Vec w
polarToCartesian (Polar r theta) = Vec (r * rsin theta) (r * rcos theta)

-- Transforms cartesian coordinates to a polar representation where
-- an angle of 0 radians corresponds to a vector pointing straight up
-- and a positive angle indicates clockwise rotation.
cartesianToPolar :: Vec w -> Polar
cartesianToPolar v = Polar (norm v) (vecAngle v)
