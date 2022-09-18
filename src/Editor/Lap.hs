module Editor.Lap where
  
--------------------------------------------------------------------------------
import Vec
import Editor.Cache ( Cache )
import Track ( Waypoint(controlPointsOffsets), wpVecsAbsolute )

import qualified Data.Map as Map
import Data.Functor ( (<&>) )
--------------------------------------------------------------------------------


-- | Get the endpoints of the line segment that marks the end of a
-- lap/beginning of a new lap.
data LapBoundary = LapBoundary
  {
    -- | Line segment defining the boundary of a lap.
    _lb_segment :: (Vec World, Vec World)

    -- | Normal vector perpendicular to the boundary segment, pointing
    -- in the intended direction of travel.
  , _lb_dir     :: Vec World
  }
lapBoundary :: Cache -> Maybe LapBoundary
lapBoundary cache = Map.lookupMax cache <&> \(_,(wp,_,_)) ->
  let [_,_,_,u,v] = wpVecsAbsolute wp
      (_, cpo2)   = controlPointsOffsets wp
  in LapBoundary (u,v) (normalize cpo2)

-- | Returns 0 if the given segment does not cross the boundary;
-- returns 1 if the segment crosses the boundary in the direction of
-- travel; returns -1 if the segment crosses the boundary against the
-- direction of travel.
crossesBoundary :: (Vec World, Vec World) -> LapBoundary -> Int
crossesBoundary (sb,se) (LapBoundary (bb,be) bdir)
  | segmentsIntersect (sb,se) (bb,be)
  = floor $ signum ((se ^-^ sb) `scalarProjectionOnto` bdir)
  | otherwise = 0 
