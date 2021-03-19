module Track where

--------------------------------------------------------------------------------
import Vec
import Angle
import Polar
import Util

import Control.Lens (makeLenses, over, set, view)
import Data.Array
import Data.Function (on)
import Data.Coerce (coerce)
import Data.List
import Data.Bifunctor
import Debug.Trace
--------------------------------------------------------------------------------

newtype TrackDescription = TrackDescription { _tdPs :: [Polar] } deriving (Show)
-- | Track segments are polygons where the vertices are listed in clockwise order.
newtype TrackSegment     = TrackSegment { _tsShape :: [Vec World] } deriving (Show, Eq, Read)
type Track               = [TrackSegment]
type Pillar              = (Vec World , Double)


-- TODO rename and move to own module
data Layout              = Layout { _lo_track :: Track , _lo_pillars :: [Pillar] }
  deriving (Read, Show)

type Waypoint = (Vec World, Double)

data LayoutSaveData
  = LayoutSaveData { _lsd_waypoints :: [Waypoint]
                   , _lsd_pillars   :: [Pillar]
                   }
  deriving (Read, Show)

makeLenses ''Polar
makeLenses ''TrackDescription
makeLenses ''TrackSegment
makeLenses ''Layout
makeLenses ''LayoutSaveData

--------------------------------------------------------------------------------
-- NEW TRACK CONSTRUCTION
--------------------------------------------------------------------------------
fromSaveData :: LayoutSaveData -> Layout
fromSaveData (LayoutSaveData waypoints pillars) = Layout (fromWaypoints waypoints) pillars
  
testje :: [Waypoint]
testje = [(zeroVec, 30), (Vec 0 200, 40), (Vec 200 400, 60), (Vec 400 400, 10), (Vec 400 0, 30)]
-- testje = [(zeroVec, 50), (Vec 0 300, 100), (Vec 0 500, 100), (Vec 50 600, 100)]

fromWaypoints :: [Waypoint] -> Track
fromWaypoints = trackFromCorners . trackCorners

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

--------------------------------------------------------------------------------
-- COLLISION DETECTION: FINE GRAINED
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
data LineSegment = LS (Vec World) (Vec World)
  deriving (Eq, Show, Read, Ord)

-- Given a point pt and a convex polygon pg, finds the point on pg closest to pt.
closestPointOnConvexPolygon :: Vec World -> ConvexPolygon -> Vec World
closestPointOnConvexPolygon pt pg
  | pointInConvexPolygon pt pg = pt
  | otherwise
    = minimumByDstToPt . map (closestPointOnLineSegment pt) . polygonEdges
    $ pg
  where
    minimumByDstToPt = minimumBy (compare `on` norm . (^-^ pt))

-- Checks whether a given point lies within a given convex polygon.
pointInConvexPolygon :: Vec World -> ConvexPolygon -> Bool
pointInConvexPolygon pt pg
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
convexPolygonShadow :: ConvexPolygon -> Vec World -> (Double, Double)
convexPolygonShadow shape onto = (minimum projection, maximum projection)
  where projection = map (`scalarProjectionOnto` onto) shape

polygonProjectionAxes :: ConvexPolygon -> [Vec World]
polygonProjectionAxes = map (perp . normalize) . polygonEdgeDeltas

-- | Check whether two polygons overlap.
polygonPolygonOverlap :: ConvexPolygon -> ConvexPolygon -> Bool
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

pickPointInConvexPolygon :: ConvexPolygon -> Vec World
pickPointInConvexPolygon (p1:p2:p3:_) = sumV [p1,p2,p3] ^/ 3

testShape :: [Vec World]
testShape = [Vec 0 0, Vec 0 1, Vec 2 1, Vec 1 1]

--------------------------------------------------------------------------------
-- COLLISION DETECTION: COARSE PHASE
--------------------------------------------------------------------------------
data CollisionGrid = CollisionGrid 
  { _cg_grid       :: !(Array (Int,Int) [Polygon])
  , _cg_bottomLeft :: !(Vec World)
  , _cg_cellSize   :: !Double
  }

collisionGridLookup :: CollisionGrid -> Vec World -> [Polygon]
collisionGridLookup (CollisionGrid grid bottomLeft cellSize) v
  = let Vec x y = (v ^-^ bottomLeft) ^/ cellSize
        ix      = (floor x , floor y)
        
    in if inRange (bounds grid) ix then grid ! ix else [] -- TODO there's still an error here with the empty grid...

mkCollisionGrid :: Double -> [Polygon] -> CollisionGrid
mkCollisionGrid cellSize []
  = CollisionGrid (listArray ((0,0),(0,0)) []) zeroVec cellSize
mkCollisionGrid cellSize collidables =
  let
    (xs , ys) = unzipVecs (concat collidables)
    bottomLeft = Vec (minimum xs) (minimum ys)
    gridData = concatMap (scanPolygon cellSize bottomLeft) collidables
    (is,js) = unzip (map fst gridData)
    lowerBound = (minimum is , minimum js)
    upperBound = (maximum is , maximum js)
    grid = accumArray (flip (:)) [] (lowerBound , upperBound) gridData
  in
    trace "test" $ CollisionGrid grid bottomLeft cellSize
  
polygonSnapToGrid :: ConvexPolygon -> ConvexPolygon                                      
polygonSnapToGrid pg = let p = pickPointInConvexPolygon pg
                       in map (`snapAwayFrom` p) pg

scanPolygon :: Double -> Vec World -> Polygon -> [((Int,Int) , Polygon)]
scanPolygon cellSize bottomLeft originals =
  let
    transformed         = [(v ^-^ bottomLeft) ^/ cellSize | v <- polygonSnapToGrid originals]
    (leftXs , rightXs)  = bimap (edgeXs onLeft) (edgeXs onRight) $ leftAndRightEdges transformed
    (Vec _ bottomY , _) = boundingBox transformed
  in
    concat [ map (\x -> ((x,y),originals)) [xleft-2..xright+2]
           | (y,xleft,xright) <- zip3 [floor bottomY - 1..] leftXs rightXs
           ]


-- pg = [Vec 0 4 , Vec 2 8 , Vec 5 4 , Vec 2 2]
-- pg = [zeroVec , Vec 5 0 , Vec 5 5 , Vec 0 5]
-- pg = [zeroVec , Vec 100 0 , Vec 100 100 , Vec 0 100]
testpg = [ Vec {_x = -100.0, _y = -0.30231962245787836}              --  (-100,0)
         , Vec {_x = -102.79765414100132, _y = 462.3915254699674}    --  (-100,500)
         , Vec {_x = 98.39765414100131, _y = 265.30847453003446}     --  (100,260)
         , Vec {_x = 100.0, _y = 0.30231962245787836}                --  (100,0)
         ]
-- (edgel,edger) = leftAndRightEdges testpg
-- 
-- (l,r) = (segmentConnections edgel, segmentConnections edger)


segmentConnections :: [LineSegment] -> [Vec World]
segmentConnections (LS v1 v2 : rest) = v1 : v2 : [v2' | LS _ v2' <- rest]

leftAndRightEdges :: ConvexPolygon -> ([LineSegment] , [LineSegment])
leftAndRightEdges vs
  = leftAndRight
  $ bimap sortYAscending sortYAscending
  $ foldr cats ([],[])
  $ zipWith op vs (tail (cycle vs))
  where
    op v1@(Vec _ y1) v2@(Vec _ y2)
      -- Ascending
      | y2 > y1   = ([] , [LS v1 v2]) -- lowest y value first
      -- Descending
      | y1 > y2   = ([LS v2 v1] , []) -- lowest y value first
      -- Horizontal
      | otherwise = ([] , [])

    cats :: ([a],[a]) -> ([a],[a]) -> ([a],[a])
    cats (as,bs) (cs,ds) = (as++cs , bs++ds)
    
    sortYAscending :: [LineSegment] -> [LineSegment]
    sortYAscending = sortOn (\(LS (Vec _ y) _) -> y)

    leftAndRight :: ([LineSegment],[LineSegment]) -> ([LineSegment],[LineSegment])
    leftAndRight (ls1,ls2) =
      let
        (Vec low1 _ , Vec high1 _) = boundingBox (segmentConnections ls1)
        (Vec low2 _ , Vec high2 _) = boundingBox (segmentConnections ls2)
      in
        if low1 < low2 || high1 < high2
        then (ls1,ls2)
        else (ls2,ls1)

segmentXs :: (Double -> Int) -> LineSegment -> [Int]
segmentXs xRound (LS (Vec x1 y1) (Vec x2 y2)) = xs
  where invSlope = (x2 - x1) / (y2 - y1)
        xs       = take (2 + round (y2 - y1)) $ map xRound $ iterate (+invSlope) x1

edgeXs :: (Double -> Int) -> [LineSegment] -> [Int]
edgeXs xRound segments =
  let (xs0:xss) = map (segmentXs xRound) segments
  in xs0 ++ concatMap tail xss

onLeft, onRight :: Double -> Int
onLeft  = floor
onRight = ceiling


-- -- | Segment can't be horizontal!
-- xsOnLeft :: LineSegment -> [Int]
-- xsOnLeft (LS (Vec x1 y1) (Vec x2 y2)) = xs
--   where invSlope = (x2 - x1) / (y2 - y1)
--         xs = map floor $ iterate (+invSlope) x1
--         ys = takeWhile (< ceiling y2) $ iterate (+1) (floor y1)
-- 
-- xsOnRight :: LineSegment -> [Int]
-- xsOnRight (LS (Vec x1 y1) (Vec x2 y2)) = xs
--   where invSlope = (x2 - x1) / (y2 - y1)
--         xs = map ceiling $ iterate (+invSlope) x1
--         ys = takeWhile (< ceiling y2) $ iterate (+1) (floor y1)

-- leftSlopes curY (LS (Vec x0 y0) (Vec x1 y1) : sortedEdges)
--   | y0 == y1  = _
--   | otherwise = 

-- scan y sortedEdges dxl dxh xl xh = 

lsLowestYFirst :: LineSegment -> LineSegment
lsLowestYFirst (LS v1@(Vec _ y1) v2@(Vec _ y2))
  | y1 <= y2  = LS v1 v2
  | otherwise = LS v2 v1



-- from http://www.sunshine2k.de/coding/java/TriangleRasterization/TriangleRasterization.html
-- | Vec x1 y1 should be the top of the triangle!
-- fillBottomFlatTriangle :: Double -> Vec World -> Triangle -> [(Int,Int)]
-- fillBottomFlatTriangle cellSize (Vec blx bly) (Vec x1 y1,Vec x2 y2,Vec x3 y3)
--   | y1 > y2 && y1 > y3 && y2 == y3
--   = let
--       invslope1 = (x2 - x1) / (y2 - y1)
--       invslope2 = (x3 - x1) / (y3 - y1)
--       ySampleBegin = floor ((y2 - bly) / cellSize)
--       ySamples = [ySampleBegin, ySampleBegin +  .. ceiling ((y1 - bly) / cellSize)]
--     in
--       _
--   | otherwise
--   = error "not a flat-bottomed triangle!"
