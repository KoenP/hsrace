module Track where

--------------------------------------------------------------------------------
import Vec
import Angle
import Polar

import Control.Lens (makeLenses, over, set, view)
import Data.Function (on)
import Data.Coerce (coerce)
import Data.List
import Debug.Trace
--------------------------------------------------------------------------------

newtype TrackDescription = TrackDescription { _tdPs :: [Polar] } deriving (Show)
newtype TrackSegment = TrackSegment { _tsShape :: [Vec World] } deriving (Show, Eq)
type Track = [TrackSegment]

-- Zipper for track data structure.
-- The "current" track segment, if there is one, is implicitly the head
-- of the forward pointing list.
data TrackZipper = TrackZipper { _tz_back    :: [TrackSegment]
                               , _tz_forward :: [TrackSegment]
                               }
  deriving Show
makeLenses ''Polar
makeLenses ''TrackDescription
makeLenses ''TrackSegment
makeLenses ''TrackZipper

--------------------------------------------------------------------------------
-- NEW TRACK CONSTRUCTION
--------------------------------------------------------------------------------
type Waypoint = (Vec World, Double)

testje :: [Waypoint]
testje = [(zeroVec, 30), (Vec 0 200, 40), (Vec 200 400, 60), (Vec 400 400, 10), (Vec 400 0, 30)]
-- testje = [(zeroVec, 50), (Vec 0 300, 100), (Vec 0 500, 100), (Vec 50 600, 100)]

fromWaypoints' :: [Waypoint] -> Track
fromWaypoints' = trackFromCorners . trackCorners

trackFromCorners :: ([Vec World] , [Vec World]) -> Track
trackFromCorners (l1:l2:l , r1:r2:r)
  = TrackSegment [l1,l2,r2,r1] : trackFromCorners (l2:l , r2:r)
trackFromCorners _ = []

trackCorners :: [Waypoint] -> ([Vec World], [Vec World])
trackCorners waypoints = unzip $ zipWith3 waypointCorners waypoints (0 : hdgs) (nag hdgs)
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

v = Vec 0 200
rad = 30
before = 0
after = pi/4
ta = after - before
alph = pi - ta / 2


-- waypointCorners :: (Vec World, Double) -> Angle -> Angle -> (Vec World, Vec World)
-- waypointCorners (v,rad) headingBefore headingAfter = (v ^-^ dv , v ^+^ dv)
--   where
--     turnAngle = headingAfter - headingBefore
--     alpha     = - (pi - turnAngle) / 2
--     dv        = rotVec headingBefore $ rad *^ Vec (rsin alpha) (rcos alpha)

-- | Repeat the last thing mentioned endlessly.
nag :: [a] -> [a]
nag []      = []
nag [x]    = repeat x
nag (x:xs) = x : nag xs
    

--------------------------------------------------------------------------------
-- TRACK CONSTRUCTION
--------------------------------------------------------------------------------
fromWaypoints :: [Vec World] -> Track
fromWaypoints waypoints
  = zipWith6 mkSeg waypoints rotations (repeat 300) lengths thetas (tail thetas)
  where
    thetas  = turnAngles waypoints
    (lengths, rotations) = unzip $ segmentLengthsAndRotations waypoints
    mkSeg :: Vec World -> Angle -> Double -> Double -> Angle -> Angle -> TrackSegment
    mkSeg offset rotation width length theta1 theta2
      = transformTrackSegment (\v -> rotVec rotation v ^+^ offset)
      $ constructTrackSegment width length theta1 theta2
    
segmentLengthsAndRotations :: [Vec World] -> [(Double, Angle)]
segmentLengthsAndRotations [] = []
segmentLengthsAndRotations waypoints
  = zipWith
    (\u v -> let dv = v ^-^ u in (norm dv, vecAngle dv))
    waypoints
    (tail waypoints)

turnAngles :: [Vec World] -> [Radians Double]
turnAngles (wp1:wp2:wps) = 0 : zipWith3 turnAngle (wp1:wp2:wps) (wp2:wps) wps ++ [0]
turnAngles wps = map (const 0) wps

-- | Compute the turn angle of three consecutive waypoints.
turnAngle :: Vec w -> Vec w -> Vec w -> Radians Double 
turnAngle u v w = let Polar _ theta_wv = cartesianToPolar (rotVec (-theta_vu) (w ^-^ v))
                  in theta_wv
  where
    Polar _ theta_vu = cartesianToPolar (v ^-^ u)


-- Constructs a track segment given the width and length of the segment,
-- as well as the angles with which it connects to its previous and its next segment.
-- The track segment is interpreted as starting at (0,0) going straight up; any rotation
-- or translation needed to put in in the right place of the track is not handled
-- by this function.
constructTrackSegment :: Double -> Double -> Radians Double -> Radians Double -> TrackSegment
constructTrackSegment width length theta1 theta2 =
  TrackSegment [ Vec (-x) (-diff1)
               , Vec (-x) (length+diff2)
               , Vec   x  (length-diff2)
               , Vec   x  diff1
               ]
  where
    x     = width / 2
    diff1 = width / 2 * tan (coerce theta1 / 2)
    diff2 = width / 2 * tan (coerce theta2 / 2)

zipperFromTrack :: Track -> TrackZipper
zipperFromTrack t = TrackZipper [] t

-- TODO: move collision stuff to its own module or something

--------------------------------------------------------------------------------
-- COLLISION DETECTION
--------------------------------------------------------------------------------

-- A list of vertices describing a polygon.
-- Unchecked assumption: the vertices are listed in order,
-- either clockwise or counterclockwise.
type Polygon = [Vec World]

-- A list of vertices describing a convex polygon.
-- Unchecked assumption: convexity of the shape.
-- Unchecked assumption: the vertices are listed in order,
-- either clockwise or counterclockwise.
type ConvexPolygon = Polygon

-- Two vectors define a line segment.
data LineSegment = LineSegment (Vec World) (Vec World)
  deriving (Eq, Show, Read, Ord)

-- Given a point pt and a convex polygon pg, finds the point on pg closest to pt.
closestPointOnConvexPolygon :: Vec World -> ConvexPolygon -> Vec World
closestPointOnConvexPolygon pt pg
  | pointInConvexPolygon pt pg = pt
  | otherwise
    = minimumByDstToPt . map (closestPointOnLineSegment pt) . polygonEdges
    $ pg
  where
    minimumByDstToPt = minimumBy $ (compare `on` norm . (^-^ pt))

-- Checks whether a given point lies within a given convex polygon.
pointInConvexPolygon :: Vec World -> ConvexPolygon -> Bool
pointInConvexPolygon pt pg = 
  all (\onto -> checkProj (scalarProjectionOnto pt onto) (convexPolygonShadow pg onto)) ontos
  where
    checkProj x (l,h) = x > l && x < h
    ontos             = map (perp . normalize) $ polygonEdgeDeltas pg

-- Given a point pt and a line segment from a to b, computes the point on
-- the line segment closest to pt.
closestPointOnLineSegment :: Vec World -> LineSegment -> Vec World
closestPointOnLineSegment p (LineSegment a b)
  | t < 0     = a
  | t > 1     = b
  | otherwise = p'
  where
    p' = (a^*(1-t)) ^+^ (b^*t)
    t  = - (v `dot` u) / (v `dot` v)
    v  = b ^-^ a
    u  = a ^-^ p

-- Computes the list of the vector differences between subsequent vertices of a polygon.
polygonEdgeDeltas :: Polygon -> [Vec World]
polygonEdgeDeltas pg = zipWith (^-^) (tail $ cycle pg) pg

-- Returns the edges of a polygon as a list of line segments.
polygonEdges :: Polygon -> [LineSegment]
polygonEdges pg = zipWith LineSegment pg (tail $ cycle pg)

-- Projects a convex shape onto a projection axis, returning the edges of its ``shadow''
-- as scalars.
-- Useful for separating axis theorem based collision detection.
convexPolygonShadow :: ConvexPolygon -> Vec World -> (Double, Double)
convexPolygonShadow shape onto = (minimum projection, maximum projection)
  where projection = map (`scalarProjectionOnto` onto) shape

testShape :: [Vec World]
testShape = [Vec 0 0, Vec 0 1, Vec 2 1, Vec 1 1]


--------------------------------------------------------------------------------
-- Probably junk
--------------------------------------------------------------------------------

constructTrack :: TrackDescription -> Track
constructTrack desc = zipWith3
  f
  (_tdPs desc)
  (0 : map (_pTheta) (_tdPs desc))
  (zip (trackWaypoints desc) (trackSegmentRotations desc))
  where
    f :: Polar -> Radians Double -> (Vec World, Radians Double) -> TrackSegment
    f (Polar r theta2) theta1 (offset, totalRotation) =
     transformTrackSegment
     (\v -> rotVec totalRotation v ^+^ offset)
     (constructTrackSegment 300 r theta1 theta2)

testTrack :: Track
testTrack = fromWaypoints' testTrackWaypoints
                            
testTrackWaypoints :: [Waypoint]
testTrackWaypoints = [ (zeroVec,w) , (Vec 0 1000,w) , (Vec 1000 1000,w/2), (Vec 1300 600, w), (Vec 1900 1000, w) {-, Vec 2000 2000-} ]
  where w = 200

trackWaypoints :: TrackDescription -> [Vec World]
trackWaypoints (TrackDescription ps) = map fst $ scanl
  (\(offset, orientation) polar ->
      let v = polarToCartesian (set pTheta orientation polar)
      in (offset ^+^ v, orientation + view pTheta polar))
  (zeroVec, 0)
  ps

trackSegmentRotations :: TrackDescription -> [Radians Double]
trackSegmentRotations (TrackDescription ps) = scanl (+) 0 (map _pTheta ps)

transformTrackSegment :: (Vec World -> Vec World) -> TrackSegment -> TrackSegment
transformTrackSegment f = over tsShape (map f)
