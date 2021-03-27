-- All sorts of functions on concave polygons.

module Track.Polygon where

--------------------------------------------------------------------------------
import Vec

import Data.List
import Data.Function
--------------------------------------------------------------------------------

-- A list of vertices describing a polygon.
-- Unchecked assumption: convexity of the shape.
-- Unchecked assumption: the vertices are listed in order,
-- either clockwise or counterclockwise.
type Polygon = [Vec World]
  
-- Two vectors define a line segment.
data LineSegment = LS (Vec World) (Vec World)
  deriving (Eq, Show, Read, Ord)

-- Given a point pt and a convex polygon pg, finds the point on pg closest to pt.
closestPointOnPolygon :: Vec World -> Polygon -> Vec World
closestPointOnPolygon pt pg
  | pointInPolygon pt pg = pt
  | otherwise
    = minimumByDstToPt . map (closestPointOnLineSegment pt) . polygonEdges
    $ pg
  where
    minimumByDstToPt = minimumBy (compare `on` norm . (^-^ pt))

-- Checks whether a given point lies within a given convex polygon.
pointInPolygon :: Vec World -> Polygon -> Bool
pointInPolygon pt pg
  = all (\onto -> checkProj (scalarProjectionOnto pt onto) (convexPolygonShadow pg onto)) ontos
  where
    checkProj x (l,h) = x >= l && x <= h
    ontos             = polygonProjectionAxes pg

-- Given a point pt and a line segment from a to b, computes the point on
-- the line segment closest to pt.
closestPointOnLineSegment :: Vec World -> LineSegment -> Vec World
closestPointOnLineSegment p (LS a b)
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
polygonEdges pg = zipWith LS pg (tail $ cycle pg)

-- Projects a convex shape onto a projection axis, returning the edges of its ``shadow''
-- as scalars.
-- Useful for separating axis theorem based collision detection.
convexPolygonShadow :: Polygon -> Vec World -> (Double, Double)
convexPolygonShadow shape onto = (minimum projection, maximum projection)
  where projection = map (`scalarProjectionOnto` onto) shape

polygonProjectionAxes :: Polygon -> [Vec World]
polygonProjectionAxes = map (perp . normalize) . polygonEdgeDeltas

-- | Check whether two polygons overlap.
polygonPolygonOverlap :: Polygon -> Polygon -> Bool
polygonPolygonOverlap shape1 shape2 = and projections
  where
    projections
      = [ projectionOverlap shadow1 shadow2
        | axis <- projectionAxes
        , let shadow1 = convexPolygonShadow shape1 axis 
        , let shadow2 = convexPolygonShadow shape2 axis 
        ]
    projectionAxes = polygonProjectionAxes shape1 ++ polygonProjectionAxes shape2

-- | Check whether two ranges overlap.
projectionOverlap :: (Double,Double) -> (Double,Double) -> Bool
projectionOverlap (l1 , h1) (l2 , h2)
  | l1 <= l2 = l2 <= h1
  | l2 <= l1 = l1 <= h2
  -- l1 <= l2 && l2 <= h1 || l1 <= h2 && h2 <= h1

pickPointInPolygon :: Polygon -> Vec World
pickPointInPolygon (p1:p2:p3:_) = sumV [p1,p2,p3] ^/ 3

testShape :: [Vec World]
testShape = [Vec 0 0, Vec 0 1, Vec 2 1, Vec 1 1]

