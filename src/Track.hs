module Track where

--------------------------------------------------------------------------------
import Vec
import Angle

import Control.Lens (makeLenses, over, set, view)
import Data.Function (on)
import Data.Coerce (coerce)
import Data.List
--------------------------------------------------------------------------------

data Polar = Polar {_pRad :: Double, _pTheta :: Radians Double } deriving (Show, Eq)
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


testTrackD = TrackDescription $ [Polar 100 0.5]
--TrackDescription $ map (Polar 100) [pi / 2, -pi / 3, pi / 2, 0]
  where len = 100

testTrack :: Track
testTrack = constructTrack testTrackD

trackWaypoints :: TrackDescription -> [Vec World]
trackWaypoints (TrackDescription ps) = map fst $ scanl
  (\(offset, orientation) polar ->
      let v = polarToCartesian (set pTheta orientation polar)
      in (offset ^+^ v, orientation + view pTheta polar))
  (zeroVec, 0)
  ps

trackSegmentRotations :: TrackDescription -> [Radians Double]
trackSegmentRotations (TrackDescription ps) = scanl (+) 0 (map _pTheta ps)

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

transformTrackSegment :: (Vec World -> Vec World) -> TrackSegment -> TrackSegment
transformTrackSegment f = over tsShape (map f)

zipperFromTrack :: Track -> TrackZipper
zipperFromTrack t = TrackZipper [] t

-- Transforms polar coordinates to a cartesian system where an angle of
-- 0 radians corresponds to a vector pointing straight up (x component equal
-- to 0), and a positive angle indicates clockwise rotation.
polarToCartesian :: Polar -> Vec World
polarToCartesian (Polar r theta) = Vec (r * rsin theta) (r * rcos theta)
-- TODO: move collision stuff to its own module or something

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
