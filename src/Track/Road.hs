module Track.Road where

--------------------------------------------------------------------------------
import Vec
import Track.Bezier
import Track.Polygon
import Track.Types
import Util

import Control.Applicative
import Control.Category ((>>>))
import Data.Bifunctor
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
--------------------------------------------------------------------------------

-- TODO: kinda bad naming
data WaypointComponent = Anchor | ControlPoint Int | Whisker Int
  deriving Eq

wpComponents :: [WaypointComponent]
wpComponents = [Anchor, ControlPoint 1, ControlPoint 2, Whisker 1, Whisker 2]

roadWidth, sampleDensity :: Double
roadWidth = 250
sampleDensity = 0.01

-- | Construct part of a road that follows a bezier curve.
--   The width of the road is linearly interpolated between the two
--   values provided as parameters.
--   sampleDensity = samples / length
bezierToRoadSegment :: Double -> Double -> CubicBezier World -> RoadSegment
bezierToRoadSegment width1 width2 bezier =
  let
    curve         = cubicCurve bezier
    (_,length)    = iterativelyApproximateCurveLength 0.01 curve
    nSamples      = ceiling (sampleDensity * length)
    samplePoints  = map (/fromIntegral nSamples) [0..fromIntegral nSamples]
    samples       = map curve samplePoints
    derivs        = map (cubicCurveDerivative bezier) samplePoints
    normDerivs    = map normalize derivs
    widths        = map (lerp width1 width2) samplePoints
    crossBars     = [ (v ^+^ offset , v ^-^ offset)
                    | (dv,v,width) <- zip3 normDerivs samples widths
                    , let offset = width *^ perp dv
                    ]
  in
    zipWith mkRoadQuad crossBars (tail crossBars)

-- | Given two crossbars, which may or may not intersect, make a road quad.
--   If they don't intersect, we construct the polygon v1-w1-w2-v2
--   Otherwise, we construct the polygon v1-w1-v2-w2
mkRoadQuad :: (Vec w, Vec w) -> (Vec w, Vec w) -> [Vec w]
mkRoadQuad (v1,v2) (w1,w2)
  -- Intersecting case
  | v1 <-> v2 > v1 <-> v2 = [v1, w1, v2, w2]
  | otherwise             = [v1, w1, w2, v2]


checkOnRoad :: Road -> (Vec World -> Bool)
checkOnRoad road =
  let grid = roadCollisionGrid road
  in \pos -> any (pointInPolygon pos)
             $ collisionGridCoords pos `multiMapLookup` grid

roadCollisionGrid :: Road -> Map (Int,Int) [RoadQuad]
roadCollisionGrid road
  = multiMapFromList
  [ (ix, quad)
  | quad <- road
  , ix <- quad |>
      quadBoundingBox >>> bimap' collisionGridCoords >>> uncurry coordRange
  ]

quadBoundingBox :: RoadQuad -> (Vec World, Vec World)
quadBoundingBox []       = error "quadBoundingBox: RoadQuad should not be empty"
quadBoundingBox (vx:vxs) = foldl' minmax (vx,vx) vxs
  where minmax (Vec xl yl, Vec xh yh) (Vec x y)
          = (Vec (min xl x) (min yl y), Vec (max xh x) (max yh y))

collisionGridCellSize :: Double
collisionGridCellSize = 64

collisionGridCoords :: Vec World -> (Int,Int)
collisionGridCoords (Vec x y)
  = (floor (x / collisionGridCellSize), floor (y / collisionGridCellSize))

coordRange :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
coordRange (xl,yl) (xh,yh) = liftA2 (,) [xl..xh] [yl..yh]

-- | Return the absolute positions of the anchor and control points
--   in a waypoint.
wpVecsAbsolute :: Waypoint -> [Vec World]
wpVecsAbsolute wp@(Waypoint anchor (cp1,cp2) _ )
  = [anchor, cp1 ^+^ anchor, cp2 ^+^ anchor, wc1, wc2]
  where
    (wc1, wc2) = widthControllerPositions wp

wpVecsWithComponents :: Waypoint -> [(WaypointComponent, Vec World)]
wpVecsWithComponents wp = wpComponents `zip` wpVecsAbsolute wp

widthControllerPositions :: Waypoint -> (Vec World, Vec World)
widthControllerPositions (Waypoint anchor (cpoffset,_) width) = 
  let wcoffset = width *^ perp (normalize cpoffset)
  in (anchor ^-^ wcoffset, anchor ^+^ wcoffset)
    

-- | Compute the absolute positions of the control points in a waypoint.
controlPointsAbsolute :: Waypoint -> (Vec World, Vec World)
controlPointsAbsolute (Waypoint anchor (e1,e2) _ ) = (anchor ^+^ e1, anchor ^+^ e2)

flipWaypointControlPoints :: Waypoint -> Waypoint
flipWaypointControlPoints (Waypoint anchor (e1,e2) width)
  = Waypoint anchor (e2,e1) width

waypointsToBezier :: Waypoint -> Waypoint -> CubicBezier World
waypointsToBezier (Waypoint anchor1 (_, offset1) _) (Waypoint anchor2 (offset2, _) _)
  = CubicBezier (anchor1, anchor2) (anchor1 ^+^ offset1, anchor2 ^+^ offset2)
