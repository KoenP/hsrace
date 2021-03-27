-- Data structure for the coarse phase of collision detection.

module Track.CollisionGrid where

--------------------------------------------------------------------------------
import Track.Polygon
import Vec

import Data.Array
import Data.Bifunctor
import Data.List
import Debug.Trace
--------------------------------------------------------------------------------

data CollisionGrid e  = CollisionGrid 
  { _cg_grid       :: !(Array (Int,Int) [e])
  , _cg_bottomLeft :: !(Vec World)
  , _cg_cellSize   :: !Double
  }

collisionGridLookup :: CollisionGrid e -> Vec World -> [e]
collisionGridLookup (CollisionGrid grid bottomLeft cellSize) v
  = let Vec x y = (v ^-^ bottomLeft) ^/ cellSize
        ix      = (floor x , floor y)
        
    in if inRange (bounds grid) ix then grid ! ix else [] -- TODO there's still an error here with the empty grid...

mkCollisionGrid :: Double -> [Polygon] -> CollisionGrid Polygon
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
    CollisionGrid grid bottomLeft cellSize

-- mkPillarGrid :: Double -> [Pillar] -> CollisionGrid Pillar
-- mkPillarGrid cellSize pillars =
--   let
--     (xs, ys)   = unzipVecs [pos ^-^ Vec rad rad | (pos,rad) <- pillars]
--     bottomLeft = Vec (minimum xs) (minimum ys)
--     grid       = accumArray (flip (:)) [] _ _
--   in
--     CollisionGrid grid bottomLeft cellSize

-- circleIndices :: Double -> Vec World -> (Vec World, Double) -> [((Int,Int), Circle)]
-- circleIndices  cellSize bottomLeft (pos,rad) =
--   (pos ^-^ bottomLeft) ^/ cellSize
  
polygonSnapToGrid :: Polygon -> Polygon                                      
polygonSnapToGrid pg = let p = pickPointInPolygon pg
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

segmentConnections :: [LineSegment] -> [Vec World]
segmentConnections (LS v1 v2 : rest) = v1 : v2 : [v2' | LS _ v2' <- rest]

leftAndRightEdges :: Polygon -> ([LineSegment] , [LineSegment])
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

lsLowestYFirst :: LineSegment -> LineSegment
lsLowestYFirst (LS v1@(Vec _ y1) v2@(Vec _ y2))
  | y1 <= y2  = LS v1 v2
  | otherwise = LS v2 v1
