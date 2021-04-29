module Track.Bezier where

--------------------------------------------------------------------------------
import Angle
import Vec
import SF
import Util
import Input
import Types
import Track.Render
import Editor.GUI
import Grid

import Graphics.Gloss
  
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (id, (.))
import Data.Function (on)
--------------------------------------------------------------------------------

-- type SegmentID = Rational
-- data SegmentComponent = Anchor | Endpoint1 | Endpoint2
-- type SegmentComponentID = (SegmentID, SegmentComponent)

type Anchors w = (Vec w, Vec w)
type ControlPoints w = (Vec w, Vec w)
data CubicBezier w = CubicBezier (Anchors w) (ControlPoints w)

type TrackSegment = [Vec World]

type PathConfig = [(Vec World, Vec World)]

data Waypoint = Waypoint { anchor :: Vec World, controlPointsOffsets :: ControlPoints World }
  deriving Show
newtype WaypointID = WaypointID Int
  deriving (Show, Eq, Ord)

wpVecsAbsolute :: Waypoint -> [Vec World]
wpVecsAbsolute (Waypoint anchor (cp1,cp2)) = [anchor, cp1 ^+^ anchor, cp2 ^+^ anchor]

bezierTest :: Picture
bezierTest = renderBezier 50 $ CubicBezier (zeroVec , Vec 300 0) (Vec 100 100 , Vec 200 (-100))

controlPointsAbsolute :: Waypoint -> (Vec World, Vec World)
controlPointsAbsolute (Waypoint anchor (e1,e2)) = (anchor ^+^ e1, anchor ^+^ e2)

flipWaypointControlPoints :: Waypoint -> Waypoint
flipWaypointControlPoints (Waypoint anchor (e1,e2)) = Waypoint anchor (e2,e1)

waypointsToBezier :: Waypoint -> Waypoint -> CubicBezier World
waypointsToBezier (Waypoint anchor1 (_, offset1)) (Waypoint anchor2 (offset2, _))
  = CubicBezier (anchor1, anchor2) (anchor1 ^+^ offset1, anchor2 ^+^ offset2)
    

controlNodeRadius :: Double
controlNodeRadius = 15

cellSize = 50

gridIndex :: Vec w -> (Int,Int)
gridIndex (Vec x y) = (floor (x / cellSize), floor (y / cellSize))

-- -- type SegmentMap = Map SegmentID (Segment, )
-- type SpaceGrid = Map (Int,Int) [SegmentComponentID]
-- 
-- spaceGrid :: SegmentMap -> SpaceGrid
-- spaceGrid = Map.toList >>> concatMap convert >>> multiMapFromList
--   where
--     convert :: (SegmentID, Segment) -> [((Int,Int), SegmentComponentID)]
--     convert (id, Segment anchor (p1,p2))
--       = [ (ix, (id,sid))
--         | (sid, v) <- [(Anchor,anchor), (Endpoint1,p1), (Endpoint2,p2)]
--         , ix <- index v
--         ]
--       -- map index [anchor, p1, p2] `zip` map (id,) [Anchor, Endpoint1, Endpoint2]
-- 
--     index :: Vec w -> [(Int,Int)]
--     index v =
--       let
--         delta           = Vec cellSize cellSize
--         (xl, yl)        = gridIndex $ v ^-^ delta
--         (xh, yh)        = gridIndex $ v ^+^ delta
--       in
--         (,) <$> [xl..xh] <*> [yl..yh]
-- 
-- bezierControlNodes :: (Vec World, Bool) ~> [Segment]
-- bezierControlNodes = runMode (noControlNodeSelected Map.empty)
--   where
--     noControlNodeSelected :: SegmentMap -> Mode (Vec World, Bool) [Segment]
--     noControlNodeSelected segmentMap =
--       let
--         segments = Map.elems segmentMap
--       in
--         Mode $ proc (cursorWorldPos, dragging) -> do
--           select <- risingEdge -< dragging
-- 
--           let
--             grid                              = spaceGrid segmentMap
-- 
--             -- IDs for control nodes that are close enough that we need to check whether they should
--             -- be highlighted.
--             controlNodeIDsToCheck             = concat . maybeToList $ gridIndex cursorWorldPos `Map.lookup` grid
--           
--             -- 
--             selectedNodeID = guard select >> listToMaybe
--               [ id
--               | (id, pos) <- controlNodeIDsToCheck `zip` map (foo segmentMap) controlNodeIDsToCheck
--               , pos <-> cursorWorldPos < controlNodeRadius
--               ]
--           
--             event = fmap (controlNodeSelected segmentMap) selectedNodeID
--           returnA -< (event, segments)
-- 
--     controlNodeSelected :: Map SegmentID Segment -> SegmentComponentID -> Mode (Vec World, Bool) [Segment]
--     controlNodeSelected segmentMap (segmentID, segmentComponent)
--       = Mode $ proc (cursorWorldPos, dragging) -> do
-- 
--           returnA -< undefined

waypoint :: Double -> Waypoint -> ((Vec World, Bool) ~> (Waypoint, Picture))
waypoint radius seg0 = runMode (notSelectedMode seg0)
  where
    notSelectedMode  :: Waypoint -> Mode (Vec World, Bool) (Waypoint, Picture)
    notSelectedMode seg@(Waypoint anchor _) = Mode $ proc (cursorWorldPos, selecting) -> do
      let
        (e1,e2)   = controlPointsAbsolute seg
        offsets   = [v ^-^ cursorWorldPos | v <- [e1, e2, anchor]]
        inRange   = any ((<= radius) . norm) offsets
        (_, nextMode, highlighted)  = minimumBy
            (compare `on` (norm . fst3))
            [ (offset, f offset, id)
            | (offset, f, id) <- zip3 offsets
                                      [ dragControlPointMode False seg
                                      , dragControlPointMode True  (flipWaypointControlPoints seg) 
                                      , dragAnchorMode seg
                                      ]
                                      [1,2,0] -- this is just horrible
            ]

      selectingRisingEdge <- risingEdge -< selecting
      event <- sampleOnRisingEdge -< (selectingRisingEdge && inRange, nextMode)

      returnA -< (event, (seg, render seg (guard inRange >> Just highlighted)))

    dragAnchorMode :: Waypoint -> Vec World -> Mode (Vec World, Bool) (Waypoint, Picture)
    dragAnchorMode seg0 offset = Mode $ proc (cursorWorldPos, selecting) -> do
      let anchor = cursorWorldPos ^+^ offset
      let seg    = seg0 { anchor = anchor }
      event <- sampleOnRisingEdge -< (not selecting, notSelectedMode seg)
      returnA -< (event, (seg, render seg (Just 0)))

    dragControlPointMode :: Bool -> Waypoint -> Vec World -> Mode (Vec World, Bool) (Waypoint, Picture)
    dragControlPointMode flip wp@(Waypoint anchor (e1init,e2init)) offset = Mode $ proc (cursorWorldPos, selecting) -> do
      let
        e1Absolute = cursorWorldPos ^+^ offset
      e1Absolute_ <- delay (e1init ^+^ anchor) -< e1Absolute
      let
        e1  = e1Absolute  ^-^ anchor
        e1_ = e1Absolute_ ^-^ anchor
        dTheta = e1 `signedInternalAngle` e1_
      theta <- cumsum -< (-dTheta)
      let
        e2  =  theta `rotVec` e2init

      let segment | flip      = Waypoint anchor (e2,e1)
                  | otherwise = Waypoint anchor (e1,e2)
      event <- sampleOnRisingEdge -< (not selecting, notSelectedMode segment)
      returnA -< (event, (segment, render segment (Just (1 + boolToInt flip))))

    -- TODO this is a very quick hack (:
    render :: Waypoint -> Maybe Int -> Picture
    render wp Nothing = let [anchor,c1,c2] = wpVecsAbsolute wp
                        in pictures [ linePic [c1,c2]
                                    , translatePic anchor (color red (circlePic controlNodeRadius))
                                    , translatePic c1 (color green (circlePic controlNodeRadius))
                                    , translatePic c2 (color green (circlePic controlNodeRadius))
                                    ]
    render wp (Just n) =
      let [anchor,c1,c2] = wpVecsAbsolute wp
      in pictures $ linePic [c1,c2] : zipWith ($)
         [ \f -> translatePic anchor (color red (f controlNodeRadius))
         , \f -> translatePic c1 (color green (f controlNodeRadius))
         , \f -> translatePic c2 (color green (f controlNodeRadius))
         ]
         (replicate n circlePic ++ [circleSolidPic] ++ repeat circlePic)


highlightedWaypoint :: (Vec World, Bool, Grid World WaypointID) ~> Maybe WaypointID
highlightedWaypoint = runMode notDraggingMode
  where
    notDraggingMode = Mode $ proc (cursorPos, tryingToDrag, grid) -> do
      let
        nearestWaypointID = closestNearby grid cursorPos
        dragging = tryingToDrag && isJust nearestWaypointID

      returnA -< (sample dragging (draggingMode (fromJust nearestWaypointID)), nearestWaypointID)

    draggingMode id = Mode $ proc (_, tryingToDrag, _) -> do
      returnA -< (sample (not tryingToDrag) notDraggingMode, Just id)


-- | For each waypoint ID, we store the waypoint and the polygons and picture
--   for the piece of the road leading back to the previous waypoint.
type Cache = Map WaypointID (Waypoint, [TrackSegment], Picture)

cacheSF :: (Maybe (WaypointID, Waypoint), Maybe (WaypointID, Waypoint)) ~> Cache
cacheSF = stateful' Map.empty step 
  where
    step :: (Maybe (WaypointID, Waypoint), Maybe (WaypointID, Waypoint)) -> Cache -> Cache

    -- Case: no updates
    step (Nothing, Nothing) old = old

    -- Case: insert new waypoint in an empty cache.
    step (Just (id, wp), Nothing) old
      | Map.null old
      = Map.insert id (wp, [], blank) old

    -- Case: insert new waypoint at the end of a nonempty cache.
    step (Just (id, wp), Nothing) old
      = writeWaypoint id wp old


    -- Case: edit existing waypoint.
    step (Nothing, Just (id, wp)) old
      = let
          cache' = writeWaypoint id wp old
        in
          case Map.lookupGT id cache' of Just (id', (wp',_,_)) -> writeWaypoint id' wp' cache'
                                         Nothing -> cache'

    step _ _ = error "cache: unhandled case"

    -- TODO "update waypoint" functie

    -- Insert or overwrite waypoint with the given ID, updating the
    -- cached road and picture *for that waypoint only*.
    writeWaypoint :: WaypointID -> Waypoint -> Cache -> Cache
    writeWaypoint id wp old
      | Just (_, (wp', _, _)) <- Map.lookupLT id old
      = let roadPart = mkRoadPart wp' wp
        in Map.insert id (wp, roadPart, renderRoadPart roadPart) old
      | otherwise
      = Map.insert id (wp, [], blank) old


    -- TODO magic number, should depend on length of segment
    mkRoadPart :: Waypoint -> Waypoint -> [TrackSegment]
    mkRoadPart wp1 wp2 = bezierToRoadPart (waypointsToBezier wp1 wp2)

readCache :: Cache -> ([Waypoint], [TrackSegment], Picture)
readCache cache = let (wps, segs, pics) = unzip3 (Map.elems cache)
                  in (wps, concat segs, pictures pics)
      
waypoints :: (Vec World, Bool, Bool) ~> (Cache, Picture)
waypoints =
  let
    gridCellSize = 100  
  in
    proc (cursorPos, dragging, addNew) -> do
  
      nextID <- WaypointID ^<< stateful' 0 (+) -< boolToInt addNew
      let 
        newWaypoint = Waypoint cursorPos (Vec 0 (-50), Vec 0 50)
        newWaypointEvent = sample addNew (nextID, ((newWaypoint, blank), waypoint controlNodeRadius newWaypoint))

      stoppedDragging <- risingEdge -< not dragging
      let updateGrid = stoppedDragging || addNew

      rec
        grid_ <- delay (mkGrid gridCellSize []) -< grid
        nearestWaypointID <- delay Nothing <<< highlightedWaypoint -< (cursorPos, dragging, grid_)
        waypointMap
          <- sparseUpdater Map.empty
          -< ([], maybeToList newWaypointEvent, maybeToList nearestWaypointID `zip` [(cursorPos, dragging)])

        let newGrid = mkGrid gridCellSize [(id,vec) | (id,(wp,_)) <- Map.toList waypointMap, vec <- wpVecsAbsolute wp]


        -- TODO: grid doesn't work properly yet
        -- does not get correctly updated after dragging a control point
        -- we need to ignore the grid while dragging
        grid <- setter (mkGrid gridCellSize []) -< sample updateGrid newGrid

      let wpPics = map snd $ Map.elems waypointMap

      cache <- cacheSF -< ((\(x,((y,_),_)) -> (x,y)) <$> newWaypointEvent
                        , do {guard dragging; id <- nearestWaypointID; (wp,_) <- id `Map.lookup` waypointMap
                             ; return (id,wp)}
                        )
  
      returnA -< (cache, pictures wpPics) -- (map fst (Map.elems waypointMap), pictures (renderGrid grid : wpPics))

bezierEdit :: Input ~> Output
bezierEdit = proc input -> do
  GUI _ cursorWorldPos _ overlay <- gui -< input
  newWaypoint <- risingEdge -< keyDown EditorAdjust input
  let dragging = keyDown EditorCommit input
  ((_,_,roadPic),waypointsPic) <- first (arr readCache) ^<< waypoints -< (cursorWorldPos, dragging, newWaypoint)
  
  returnA -< Output (overlay $ pictures [roadPic, waypointsPic]) Nothing

quadraticCurve :: Vec w -> Vec w -> Vec w -> (Double -> Vec w)
quadraticCurve v1 v2 v3 =
  -- v1 ^+^ (2*t)*^(v2^-^ v1) ^+^ (t*t)*^(v3 ^-^ 2*^v2 ^+^ v1)
  let lerp1 = lerp v1 v2
      lerp2 = lerp v2 v3
  in \t -> lerp (lerp1 t) (lerp2 t) t

cubicCurve :: CubicBezier w -> (Double -> Vec w)
cubicCurve (CubicBezier (e1,e2) (c1,c2)) =
  let
    curve1 = quadraticCurve e1 c1 c2
    curve2 = quadraticCurve c1 c2 e2
  in
    \t -> lerp (curve1 t) (curve2 t) t

cubicCurveDerivative :: CubicBezier w -> (Double -> Vec w)
cubicCurveDerivative (CubicBezier (e1,e2) (c1,c2)) t
  =   e1 ^* ((-3)*(1-t)**2)
  ^+^ c1 ^* (9*t*t - 12*t + 3)
  ^+^ c2 ^* (3 * (2 - 3*t) * t)
  ^+^ e2 ^* (3*t*t)

roadWidth = 100

sampleDensity = 0.01

-- | Construct part of a road that follows a bezier curve.
--   sampleDensity = samples / length
bezierToRoadPart :: CubicBezier World -> [TrackSegment]
bezierToRoadPart bezier =
  let
    curve         = cubicCurve bezier
    (_,length)    = iterativelyApproximateCurveLength 0.01 curve
    nSamples      = ceiling (sampleDensity * length)
    samplePoints  = map (/fromIntegral nSamples) [0..fromIntegral nSamples]
    samples       = map curve samplePoints
    derivs        = map (cubicCurveDerivative bezier) samplePoints
    normDerivs    = map normalize derivs
    crossBars     = [ (v ^+^ offset , v ^-^ offset)
                    | (dv,v) <- normDerivs `zip` samples
                    , let offset = roadWidth *^ perp dv
                    ]
  in
    zipWith mkTrackSegment crossBars (tail crossBars)

-- | Given two crossbars, which may or may not intersect, make a track segment.
--   If they don't intersect, we construct the polygon v1-w1-w2-v2
--   Otherwise, we construct the polygon v1-w1-v2-w2
mkTrackSegment :: (Vec w, Vec w) -> (Vec w, Vec w) -> [Vec w]
mkTrackSegment (v1,v2) (w1,w2)
  -- Intersecting case
  | v1 <-> v2 > v1 <-> v2 = [v1, w1, v2, w2]
  | otherwise             = [v1, w1, w2, v2]

renderRoadPart :: [TrackSegment] -> Picture
renderRoadPart = pictures . zipWith color (cycle [blue, dim blue]) . map polygonPic

renderBezier :: Int -> CubicBezier World -> Picture
renderBezier nSamples bezier =
  let
    curve         = cubicCurve bezier
    samplePoints  = map (/fromIntegral nSamples) [0..fromIntegral nSamples]
    samples       = map curve samplePoints
    -- path          = linePic samples
    -- endPoints     = map (renderControlNode red) [e1, e2]
    -- controlPoints = map (renderControlNode green) [c1, c2]
    derivs        = map (cubicCurveDerivative bezier) samplePoints
    normDerivs    = map normalize derivs
    crossBars     = [ (v ^+^ offset , v ^-^ offset)
                    | (dv,v) <- normDerivs `zip` samples
                    , let offset = roadWidth *^ perp dv
                    ]
    segments      = zipWith mkTrackSegment crossBars (tail crossBars)
    segmentPics   = zipWith color (cycle [blue, dim blue]) (map polygonPic segments)
    -- leftVertices = 
    -- cbPics        = map (\(x,y) -> color blue $ linePic [x,y]) crossBars
    -- arrows        = thinOut 4 [ color (dim blue) $ linePic [v, v ^+^ dv] | (dv,v) <- derivs `zip` samples ]
  in
    pictures segmentPics

renderControlNode :: Color -> Vec World -> Picture
renderControlNode col pos = color col $ translatePic pos $ circlePic controlNodeRadius

sampleCurve :: Int -> (Double -> Vec w) -> [Vec w]
sampleCurve nSamples curve =
  let
    nSamples' = fromIntegral nSamples
    samplePoints = map (/nSamples') [0..nSamples']
  in
    map curve samplePoints

iterativelyApproximateCurveLength :: Double -> (Double -> Vec w) -> (Int, Double)
iterativelyApproximateCurveLength epsilon curve =
  let
    approximations = [(nSamples, approximateCurveLength nSamples curve) | nSamples <- iterate (*2) 16]
    pairs = approximations `zip` tail approximations
    test ((_,apx1), (_,apx2)) = apx2 - apx1 <= epsilon * apx2
  in
    fst $ head $ dropWhile (not . test) pairs

-- | Approximate the length of a curve (from 0 to 1).
approximateCurveLength :: Int -> (Double -> Vec w) -> Double
approximateCurveLength nSamples curve =
  let samplePoints = sampleCurve nSamples curve
  in foldl' (^+^) zeroVec $ zipWith (<->) samplePoints (tail samplePoints)
  
