module Track.Road where

--------------------------------------------------------------------------------
import Vec
import Angle
import Util
import Track.Polygon

import Control.Lens
--------------------------------------------------------------------------------

-- | Road segments are polygons where the vertices are listed in clockwise order.
newtype RoadSegment = RoadSegment { _tsShape :: Polygon } deriving (Show, Eq, Read)
makeLenses ''RoadSegment

type Road = [RoadSegment]
type Waypoint = (Vec World, Double)

fromWaypoints :: [Waypoint] -> Road
fromWaypoints = roadFromCorners . roadCorners

roadFromCorners :: ([Vec World] , [Vec World]) -> Road
roadFromCorners (l1:l2:l , r1:r2:r)
  = RoadSegment [l1,l2,r2,r1] : roadFromCorners (l2:l , r2:r)
roadFromCorners _ = []

roadCorners :: [Waypoint] -> ([Vec World], [Vec World])
roadCorners waypoints = unzip $ zipWith3 waypointCorners waypoints (0 : hdgs) (nag hdgs)
  where
    hdgs = headings waypoints

headings :: [Waypoint] -> [Angle]
headings waypoints = zipWith (\(v1,_) (v2,_) -> vecAngle (v2 ^-^ v1)) waypoints (tail waypoints)
  
waypointCorners :: Waypoint -> Angle -> Angle -> (Vec World, Vec World)
waypointCorners (v,rad) headingBefore headingAfter
  = (relocate (Vec (-rad) hHalved) , relocate (Vec rad (-hHalved)))
  where
    hHalved         = rad / rtan alpha
    turnAngle       = headingAfter - headingBefore
    alpha           = (pi - turnAngle) / 2
    relocate        = (^+^ v) . rotVec headingBefore
