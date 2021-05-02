module Track.Road where

--------------------------------------------------------------------------------
import Vec
import Track.Bezier
import Track.Polygon
import Util

import Control.Applicative
import Control.Category ((>>>))
import Data.Bifunctor
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
--------------------------------------------------------------------------------

-- | At the smallest scale, a road consists of quadrilaterals which
--   are used for overlap detection and rendering.
type RoadQuad    = [Vec World]

-- | A road is a list of road segments.
type Road        = [RoadQuad]

-- | A road segment is a section of road between two waypoints.
type RoadSegment = Road

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
quadBoundingBox (vx:vxs)= foldl' minmax (vx,vx) vxs
  where minmax (Vec xl yl, Vec xh yh) (Vec x y)
          = (Vec (min xl x) (min yl y), Vec (max xh x) (max yh y))

collisionGridCellSize :: Double
collisionGridCellSize = 64

collisionGridCoords :: Vec World -> (Int,Int)
collisionGridCoords (Vec x y)
  = (floor (x / collisionGridCellSize), floor (y / collisionGridCellSize))

coordRange :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
coordRange (xl,yl) (xh,yh) = liftA2 (,) [xl..xh] [yl..yh]
