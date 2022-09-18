module Lap where

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
